{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ViewPatterns #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Generate.ConGenTest where

import Control.Monad (Monad, (<=<), forM)
import Control.Monad.Morph (MFunctor(..))
import Control.Monad.State (State, evalStateT)
import Data.Char (chr, ord)
import Data.Functor.Foldable (Base, Corecursive, Recursive, cata, embed, para, project)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List (foldl')
import Data.List.Ordered (has, member, union)
import Data.Maybe (isJust)
import Data.Text (Text, singleton, stripEnd)
import Data.Text as T (concat, cons, unlines)
import Data.Text.IO as T (putStr)
import NeatInterpolation

import           Hedgehog
import qualified Hedgehog.Internal.Gen as Gen
--import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Control.Monad.State.Class as State

data ContExpr -- A contraction expression
    = Tensor { getId :: Int, getIndices :: [Int] } -- Tensors have an id, so they can appear multiple times. Indices i, j, ... are stored as Int-s.
    | Sum { getIndex :: Int, getOperands :: [ContExpr] }
    deriving Show

makeBaseFunctor ''ContExpr

expr = Sum 1 [Tensor 0 [0,1], Sum 2 [Tensor 1 [0,1,2], Tensor 2 [1,2]]]

dimRange = Range.linear 1 10
numOps = Range.linear 1 10
--maxDepth = 4

data GenState = GenState { tensorId :: Int, sumId :: Int, idxs :: [Int] }

genFiltered :: (MonadGen m, State.MonadState GenState m) => m ContExpr
genFiltered = do
    resDims <- Gen.int dimRange
    let noConstResult xs = if and $ map (has xs) [0..resDims-1] then Just () else Nothing
        alg (TensorF _ xs) = Just xs
        alg (SumF i ops) = case (and $ map (member i) ops) of
                                True -> Just $ foldl' union [] ops
                                False -> Nothing
        noConstSum = isJust . (noConstResult <=< cataM alg)
        initGenExpr = do State.put $ GenState 0 resDims [0..resDims-1]; genExpr
        hasSum = \case Sum{} -> True; Tensor{} -> False
    Gen.filter (\a -> noConstSum a && hasSum a) $ initGenExpr

genExpr :: (MonadGen m, State.MonadState GenState m) => m ContExpr
genExpr = Gen.recursive Gen.choice
            [
                do
                    GenState nextId _ idxs <- State.get
                    subs <- subsequence (1,4) idxs
                    State.modify (\s -> s { tensorId = nextId + 1 })
                    pure $ Tensor nextId subs
            ] [
                do
                    GenState _ nextId xs <- State.get
                    State.modify (\s -> s { sumId = nextId + 1, idxs = xs ++ [nextId] })
                    ops <- Gen.list numOps genExpr
                    State.modify (\s -> s { idxs = xs })
                    pure $ Sum nextId ops
            ]

--subsequence :: MonadGen m => Range Int -> [a] -> m a
subsequence :: MonadGen m => (Int,Int) -> [a] -> m [a]
subsequence (minLength,maxLength) xs = Gen.filter (\case (length -> l) -> minLength <= l && l <= maxLength) (Gen.subsequence xs)
  
smpl :: IO [ContExpr]
smpl = forM [0..10] (\_ -> Gen.sample $ hoist (`evalStateT` GenState 0 0 []) genFiltered)

smpl2 :: IO [[Int]]
smpl2 = forM [0..10] (\_ -> Gen.sample $ subsequence (1,4) [1..10])

printContraction :: Bool -> ContExpr -> Text
printContraction tex expr = math code where
    (math,sub',sum) = case tex of
        True -> (\t -> [text|$$$t$$|], \t -> [text|{$t}|], "\\sum\\limits")
        False -> (id, id, "sum")
    code = para rAlg expr
    sub = sub' . T.concat . map indexName
    rAlg (TensorF (tensorName -> name) (sub -> idxs)) = stripEnd [text|${name}_$idxs|]
    rAlg (SumF (indexName -> name) ops) = stripEnd [text|${sum}_$name$ops'|] where
        ops' = T.concat $ map (stripEnd . T.cons ' ' . (\case (Tensor{},txt) -> txt; (Sum{},txt) -> [text|($txt)|])) ops

tensorName :: Int -> Text
tensorName n = singleton $ chr $ ord 'A' + n

indexName :: Int -> Text
indexName n = singleton $ chr $ ord 'i' + n

anaM :: (Monad m, Traversable (Base t), Corecursive t) => (a -> m (Base t a)) -> a -> m t
anaM coalg = a where a = (return . embed) <=< traverse a <=< coalg

cataM :: (Monad m, Traversable (Base t), Recursive t) => (Base t a -> m a) -> t ->  m a
cataM alg = c where c = alg <=< traverse c . project

main :: IO ()
main = do
    xs <- smpl
    --print xs
    T.putStr $ T.unlines $ map (printContraction False) xs