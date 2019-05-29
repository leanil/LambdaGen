{-# LANGUAGE LambdaCase, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ViewPatterns #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Generate.Contraction where

import Expr
import LinAlg
import Recursion
import Type
import Utility
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree(..))
import Control.Monad ((<=<), foldM)
import Control.Monad.State (State, StateT, evalState, evalStateT, get, modify, put)
import Data.Char (chr, ord)
import Data.Foldable (foldrM)
import Data.Functor.Foldable (cata, para)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List (foldl')
import Data.List.Ordered (has, member, minus, union)
import Data.Map.Strict (Map, (!), fromSet)
import Data.Maybe (isJust)
import qualified Data.Set as Set (fromList, unions)
import Data.Text (Text, singleton, stripEnd, unpack)
import Data.Text as T (concat, cons)
import Hedgehog (Gen, MonadGen)
import qualified Hedgehog.Gen as Gen (choice, filter, int, list, recursive, sample, subsequence)
import qualified Hedgehog.Range as Range (constant)
import NeatInterpolation

data ContExpr -- A contraction expression
    = Tensor { getId :: Int, getIndices :: [Int] } -- Tensors have an id, so they can appear multiple times. Indices i, j, ... are stored as Int-s.
    | Sum { getIndex :: Int, getOperands :: [ContExpr] }
    deriving Show

makeBaseFunctor ''ContExpr

expr = Sum 1 [Tensor 0 [0,1], Sum 2 [Tensor 1 [0,1,2], Tensor 2 [1,2]]]

nodeCount :: ContExpr -> Int
nodeCount = cata (\case TensorF{} -> 1; SumF _ ops -> sum ops)

type ContEq = Cofree ContExprF [Int] -- A contraction equation, each subexpression annotated with its shape

calcDims :: ContExpr -> ContEq
calcDims = cata alg where
    alg (node@(TensorF _ idxs)) = idxs :< node
    alg (node@(SumF idx ops)) = dims :< node where
        dims = minus (foldl' union [] $ map extract ops) [idx]

dimRange = Range.constant  1 4
numOps = Range.constant  1 4

data GenState = GenState { tensorId :: Int, sumId :: Int }

genContExpr :: StateT GenState Gen ContExpr
genContExpr = do
    resDims <- Gen.int $ Range.constant 0 2
    let noConstResult xs = if and $ map (has xs) [0..resDims-1] then Just () else Nothing
        alg (TensorF _ xs) = Just xs
        alg (SumF i ops) = case (and $ map (member i) ops) of
                                True -> Just $ foldl' union [] ops
                                False -> Nothing
        noConstSum = isJust . (noConstResult <=< cataM alg)
        initGenExpr = do put $ GenState 0 resDims; genExpr [0..resDims-1]
    Gen.filter (\a -> noConstSum a && nodeCount a > 3) $ initGenExpr

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

subsequence :: MonadGen m => (Int,Int) -> [a] -> m [a]
subsequence (minLength,maxLength) xs = Gen.filter (\case (length -> l) -> minLength <= l && l <= maxLength) (Gen.subsequence xs)
  
sample :: IO ContExpr
sample = Gen.sample $ evalStateT genContExpr $ GenState 0 0 

randomContraction :: IO Expr0
randomContraction = do
    expr <- sample
    let sizes = makeSizes expr
    return $ translate (calcDims expr) sizes

makeSizes :: ContExpr -> Map Int Int
makeSizes = fromSet (+2) . cata alg where
    alg (TensorF _ idxs) = Set.fromList idxs
    alg (SumF _ ops) = Set.unions ops

translate :: ContEq -> Map Int Int -> Expr0
translate expr sizes = evalState (paraM alg expr) 0 where
    alg :: MRAlgebra ContEq (State Int) Expr0
    alg (_ ::< TensorF (unpack . tensorName -> name) ind) =
        return $ vecView name $ map (sizes !) ind
    alg (free ::< SumF sumId (map (mapFst extract) -> ops)) = do
        let mapDim :: ([([Int], Expr0)], Expr0 -> Expr0) -> Int -> State Int ([([Int], Expr0)], Expr0 -> Expr0)
            mapDim (xs,f) i = do
                let consumeDim :: Int -> ([Int], Expr0) -> ([([Int], Expr0)], [(Expr0,Expr0)]) -> State Int ([([Int], Expr0)], [(Expr0,Expr0)])
                    --consumeDim _ _ x@([],_) = return (x:xs, ys)
                    consumeDim i x@((i0:is), e) (xs,ys) 
                        | i0 /= i = return (x:xs,ys)
                        | otherwise = do
                            nextId <- get
                            put $ nextId + 1
                            let param = var [charShift 'a' nextId] (power double $ map (sizes !) is)
                            return ((is, param):xs, (param,e):ys)
                (xs', unzip -> (params, args)) <- foldrM (consumeDim i) ([],[]) xs 
                return $ (xs', \e -> f $ mkZipWithN (lam params e) args)
        (ops',maps) <- foldM mapDim (ops,id) free 
        let red = mkRnZ sclAdd sclMul (map snd ops')
        return $ maps red

printContraction :: Bool -> ContEq -> Text
printContraction tex expr = math [text|R_$shape = $code|] where
    (math,sub',sum) = case tex of
        True -> (\t -> [text|$$$t$$|], \t -> [text|{$t}|], "\\sum\\limits")
        False -> (id, id, "sum")
    shape = sub $ extract expr
    code = para rAlg expr
    sub = sub' . T.concat . map indexName
    rAlg (_ ::< TensorF (tensorName -> name) (sub -> idxs)) = [text|${name}_$idxs|]
    rAlg (_ ::< SumF (indexName -> name) ops) = stripEnd [text|${sum}_$name$ops'|] where
        ops' = T.concat $ map (stripEnd . T.cons ' ' . (\case (_ :< TensorF{},txt) -> txt; (_ :< SumF{},txt) -> [text|($txt)|])) ops

tensorName :: Int -> Text
tensorName = singleton . charShift 'A'

indexName :: Int -> Text
indexName = singleton . charShift 'i'

charShift :: Char -> Int -> Char
charShift c n = chr $ ord c + n