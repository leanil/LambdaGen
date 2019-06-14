{-# LANGUAGE FlexibleInstances, LambdaCase, OverloadedStrings, ScopedTypeVariables, TemplateHaskell, TypeFamilies, TypeSynonymInstances, ViewPatterns #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Generate.Contraction where

import Expr
import LinAlg
import Naming
import Recursion
import Type
import Utility
import Control.Applicative ((<$>))
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree(..))
import Control.Monad ((<=<), foldM)
import Control.Monad.State (State, StateT, evalState, evalStateT, get, modify, put)
import Data.Aeson (FromJSON(..), ToJSON(..), (.=), (.:), object, withObject)
import Data.Foldable (foldrM)
import Data.Functor.Foldable (Base, ana, cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List (foldl', partition)
import Data.List.Ordered (has, member, minus, sort, union)
import Data.Maybe (isJust)
import qualified Data.Set as Set (Set, fromList, insert, unions)
import Data.Text (unpack)
import Hedgehog (Gen, MonadGen)
import qualified Hedgehog.Gen as Gen (choice, filter, int, list, recursive, sample, subsequence)
import qualified Hedgehog.Range as Range (constant)

data ContExpr -- A contraction expression
    = Tensor { getId :: Int, getIndices :: [Int] } -- Tensors have an id, so they can appear multiple times. Indices i, j, ... are stored as Int-s.
    | Sum { getIndex :: Int, getOperands :: [ContExpr] }
    deriving Show

makeBaseFunctor ''ContExpr

expr = calcDims $ Sum 1 [Tensor 0 [0,1], Sum 2 [Tensor 1 [0,1,2], Tensor 2 [1,2]]]

leafCount :: ContExpr -> Int
leafCount = cata (\case TensorF{} -> 1; SumF _ ops -> sum ops)

type ContEq = Cofree ContExprF [Int] -- A contraction equation, each subexpression annotated with its shape


instance ToJSON ContEq where
    toJSON = cata alg where
        alg (shape ::< TensorF name indices) = object ["tag" .= ("Tensor"::String), "id" .= name, "indices" .= indices, "shape" .= shape]
        alg (shape ::< SumF index ops) = object ["tag" .= ("Sum"::String), "index" .= index, "operands" .= ops, "shape" .= shape]
instance FromJSON ContEq where
    parseJSON = anaM alg where
        alg = withObject "ContEq" $ \v -> do
            tag <- v .: "tag"
            (::<) <$> v .: "shape" <*> case tag of
                ("Tensor"::String) -> TensorF <$> v .: "id" <*> v .: "indices"
                "Sum" -> SumF <$> v .: "index" <*> v .: "operands"
               
calcDims :: ContExpr -> ContEq
calcDims = cata alg where
    alg node@(TensorF _ idxs) = sort idxs :< node
    alg node@(SumF idx ops) = dims :< node where
        dims = minus (foldl' union [] $ map extract ops) [idx]

dimRange = Range.constant  1 4
numOps = Range.constant  1 4

data GenState = GenState { tensorId :: Int, sumId :: Int }

genContExpr :: StateT GenState Gen ContExpr
genContExpr = do
    resDims <- Gen.int $ Range.constant 0 2
    let initGenExpr = do put $ GenState 0 resDims; genExpr [0..resDims-1]
    --     noConstResult xs = if and $ map (has xs) [0..resDims-1] then Just () else Nothing
    --     alg (TensorF _ xs) = Just xs
    --     alg (SumF i ops) = case (and $ map (member i) ops) of
    --                             True -> Just $ foldl' union [] ops
    --                             False -> Nothing
    --     noConstSum = isJust . (noConstResult <=< cataM alg)
    Gen.filter (\a -> let x = leafCount a in x > 3 && x < 20) initGenExpr

genExpr :: [Int] -> StateT GenState Gen ContExpr
genExpr xs = Gen.recursive Gen.choice
            [
                do
                    GenState nextId _ <- get
                    subs <- subsequence (1,4) xs
                    modify (\s -> s { tensorId = nextId + 1 })
                    pure $ Tensor nextId subs
            ] [
                do
                    GenState _ nextId <- get
                    modify (\s -> s { sumId = nextId + 1 })
                    ops <- Gen.list numOps (genExpr $ xs ++ [nextId])
                    pure $ Sum nextId ops
            ]

genSimple :: StateT Int Gen ContExpr
genSimple = do
    resDims <- Gen.int $ Range.constant 0 2
    let genLeaf = do
            nextId <- get
            subs <- subsequence (0,4) [0..resDims]
            modify (+1)
            pure $ Tensor nextId subs
    ops <- Gen.list (Range.constant 1 4) genLeaf
    pure $ Sum resDims ops

subsequence :: MonadGen m => (Int,Int) -> [a] -> m [a]
subsequence (minLength,maxLength) xs = Gen.filter (\case (length -> l) -> minLength <= l && l <= maxLength) (Gen.subsequence xs)
  
sample :: IO ContEq
sample = calcDims <$> Gen.sample (evalStateT genContExpr $ GenState 0 0)

sampleSimple :: IO ContEq
sampleSimple = calcDims <$> Gen.sample (evalStateT genSimple 0)

randomContraction :: IO Expr0
randomContraction = translate smallSizes <$> sample

type Extents = Int -> Int

smallSizes :: Extents
smallSizes = (+2)

type ShapedExpr = ([Int], Expr0)
translate :: Extents -> ContEq -> Expr0
translate sizes expr = evalState (paraM alg expr) 0 where
    alg :: MRAlgebra ContEq (State Int) Expr0
    alg (_ ::< TensorF (unpack . tensorName -> name) ind) =
        return $ vecView' name $ map snd $ sort $ zip ind $ defaultStrides $ map sizes ind
    alg (free ::< SumF sumId (map (mapFst extract) -> ops)) = do
        -- Peel off a single dimension from all arguments, and compose the resulting map to the earlier ones
        let mapDim :: ([ShapedExpr], Expr0 -> Expr0) -> Int -> State Int ([ShapedExpr], Expr0 -> Expr0)
            mapDim (xs,f) i = do
                -- process a single argument, and replace it with a new variable if the outer dimension matches
                let consumeDim :: Int -> ShapedExpr -> ([ShapedExpr], [(Expr0,Expr0)]) -> State Int ([ShapedExpr], [(Expr0,Expr0)])
                    consumeDim _ x@([],_) (xs,ys) = return (x:xs, ys)
                    consumeDim i x@(i0:is, e) (xs,ys) 
                        | i0 /= i = return (x:xs,ys)
                        | otherwise = do
                            nextId <- get
                            put $ nextId + 1
                            let param = var ("a" ++ show nextId) $ case (is,e) of 
                                    ([],_) -> double
                                    (_,_ :< View _ (_:shape)) -> power' double shape
                                    (_,_ :< Variable _ (FPower _ (_:shape))) -> power' double shape
                                    _ -> power double $ map sizes is
                            return ((is, param):xs, (param,e):ys)
                (xs', unzip -> (params, args)) <- foldrM (consumeDim i) ([],[]) xs 
                return (xs', if null params then f else \e -> f $ mkZipWithN (lam params e) args)
        (ops',maps) <- foldM mapDim (ops,id) free 
        let (map snd -> scalars,map snd -> vec) = partition (null . fst) ops'
            factors = scalars ++ if null vec then [scl $ fromIntegral $ sizes sumId] else [mkRnZ sclAdd (sclMulN $ length vec) vec]
            red = case factors of [x] -> x; (x:xs) -> foldl mul x xs
        return $ maps red
