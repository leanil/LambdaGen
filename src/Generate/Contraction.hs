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
import Control.Arrow ((&&&))
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree(..))
import Control.Monad ((<=<), foldM, replicateM)
import Control.Monad.State (State, StateT, evalState, evalStateT, execState, get, modify, put)
import Data.Aeson (FromJSON(..), ToJSON(..), (.=), (.:), object, withObject)
import Data.Foldable (foldrM)
import Data.Functor.Foldable (Base, ana, cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List (foldl', partition)
import Data.List.Ordered (has, member, minus, sort, union)
import Data.Map.Strict as Map ((!), adjust, fromAscList)
import Data.Maybe (isJust)
import Data.Text (unpack)
import Hedgehog (Gen, MonadGen)
import qualified Hedgehog.Gen as Gen (element, filter, frequency, int, list, sample, shuffle, subsequence)
import qualified Hedgehog.Range as Range (constant)

data ContExpr -- A contraction expression
    = Tensor { getId :: Int, getIndices :: [Int] } -- Tensors have an id, so they can appear multiple times. Indices i, j, ... are stored as Int-s.
    | Sum { getIndex :: Int, getOperands :: [ContExpr] }
    deriving Show

makeBaseFunctor ''ContExpr

expr = calcDims $ Sum 1 [Tensor 0 [0,1], Sum 2 [Tensor 1 [0,1,2], Tensor 2 [1,2]]]

nodeCount :: Algebra ContExpr (Int,Int) -- (Sums,Tensors)
nodeCount TensorF{} = (0,1)
nodeCount (SumF _ ops) = (a+1,b) where
    (a,b) = foldl' (\(p,q) (r,s) -> (p+r,q+s)) (0,0) ops

type ContEq = Cofree ContExprF [Int] -- A contraction equation, each subexpression annotated with its shape

instance ToJSON ContEq where
    toJSON = cata alg where
        alg (shape ::< TensorF name indices) = object ["tag" .= ("Tensor"::String), "id" .= name, "indices" .= indices, "shape" .= shape]
        alg (shape ::< SumF index ops) = object ["tag" .= ("Sum"::String), "index" .= index, "operands" .= ops, "shape" .= shape]
instance FromJSON ContEq where
    parseJSON val = calcDims <$> anaM alg val where
        alg = withObject "ContEq" $ \v -> do
            tag <- v .: "tag"
            case tag of
                ("Tensor"::String) -> TensorF <$> v .: "id" <*> v .: "indices"
                "Sum" -> SumF <$> v .: "index" <*> v .: "operands"
               
calcDims :: ContExpr -> ContEq
calcDims = cata alg where
    alg node@(TensorF _ idxs) = sort idxs :< node
    alg node@(SumF idx ops) = dims :< node where
        dims = minus (foldl' union [] $ map extract ops) [idx]

dimRange = Range.constant  1 4
numOps = Range.constant  1 4

data GenConfig = GenConfig { minDims,maxDims, minOperands,maxOperands, minTens,maxTens, minSums,maxSums :: Int }
data GenState = GenState { tensorId :: Int, sumId :: Int, resultDims :: Int }

genContExpr :: GenConfig -> StateT GenState Gen ContExpr
genContExpr config = do
    resDims <- Gen.int $ Range.constant (minDims config) (maxDims config)
    let initGenExpr = do put $ GenState 0 resDims resDims; genExpr config [0..resDims-1]
    --     noConstResult xs = if and $ map (has xs) [0..resDims-1] then Just () else Nothing
    --     alg (TensorF _ xs) = Just xs
    --     alg (SumF i ops) = case (and $ map (member i) ops) of
    --                             True -> Just $ foldl' union [] ops
    --                             False -> Nothing
    --     noConstSum = isJust . (noConstResult <=< cataM alg)
    let check expr = countsOk && resultDimsOk where
            countsOk = s >= minSums config && s <= maxSums config && t >= minTens config && t <= maxTens config
            resultDimsOk = extract (calcDims expr) == [0..resDims-1]
            (s,t) = cata nodeCount expr
    Gen.filter check initGenExpr

genExpr :: GenConfig -> [Int] -> StateT GenState Gen ContExpr
genExpr config xs = do
    GenState nextTensor nextSum resDims <- get
    let pSum = if nextSum - resDims < maxSums config then 1 else 0
        genTensor = do
            modify (\s -> s { tensorId = nextTensor + 1 })
            subs <- subsequence (minDims &&& maxDims $ config) xs
            pure $ Tensor nextTensor subs
        genSum = do
            modify (\s -> s { sumId = nextSum + 1 })
            ops <- Gen.list (Range.constant (minOperands config) (maxOperands config)) (genExpr config $ xs ++ [nextSum])
            pure $ Sum nextSum ops
    Gen.frequency [(1,genTensor),(pSum,genSum)]

subsequence :: MonadGen m => (Int,Int) -> [a] -> m [a]
subsequence (minLength,maxLength) xs 
    | minLength <= min maxLength (length xs) = 
        Gen.filter (\case (length -> l) -> minLength <= l && l <= maxLength) (Gen.subsequence xs) -- >>= Gen.shuffle -- TODO: generate well-sized subsets directly
  
sample :: GenConfig -> IO ContEq
sample config = calcDims <$> Gen.sample (evalStateT (genContExpr config) (GenState 0 0 0))

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

totalSize :: Extents -> ContEq -> Int
totalSize sizes = cata (\case (_ ::< TensorF _ idxs) -> product $ map sizes idxs
                              (shape ::< SumF _ ops) -> sum ops + product (map sizes shape))

cost :: Extents -> ContEq -> [Int] -- [add, mul, good read, bad read, write]
cost sizes expr@(shape :< _) = map (resultSize*) $ cata (ignoreAlg alg) expr ++ [1] where
    alg TensorF{} = [0,0,1,0]
    alg (SumF (sizes -> rng) ops) = map (rng*) $ foldl' (zipWith (+)) [1,length ops,0,0] ops
    resultSize = product $ map sizes shape

collectIndices :: ContEq -> [Int]
collectIndices = cata (ignoreAlg alg) where
    alg (TensorF _ idxs) = sort idxs
    alg (SumF i ops) = foldl' union [i] ops
    
genExtents :: [Int] -> (Int,Int) -> ContEq -> IO Extents
genExtents sizes (minSize,maxSize) expr = Gen.sample $ (!) <$> do
    let indices = collectIndices expr
        filter (minSize,maxSize) exts = let s = sum $ cost (exts !) expr in s >= minSize && s <= maxSize
        increase exts = do ind <- Gen.element indices; pure $ adjust (*2) ind exts
    exts <- Gen.filter (filter (0,maxSize)) $ Map.fromAscList <$> mapM (\ind -> do ext <- Gen.element sizes; pure (ind,ext)) indices
    iterateUntilM (filter (minSize,maxSize)) increase exts
