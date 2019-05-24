{-# LANGUAGE LambdaCase, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ViewPatterns #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Generate.Contraction where

import Recursion
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree(..))
import Data.Char (chr, ord)
import Data.Functor.Foldable (cata, para)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List (foldl')
import Data.List.Ordered (union, minus)
import Data.Text (Text, singleton, stripEnd)
import Data.Text as T (concat, cons)
import NeatInterpolation

data ContExpr -- A contraction expression
    = Tensor { getId :: Int, getIndices :: [Int] } -- Tensors have an id, so they can appear multiple times. Indices i, j, ... are stored as Int-s.
    | Sum { getIndex :: Int, getOperands :: [ContExpr] }
    deriving Show

makeBaseFunctor ''ContExpr

expr = calcDims $ Sum 1 [Tensor 0 [0,1], Sum 2 [Tensor 1 [0,1,2], Tensor 2 [1,2]]]

type ContEq = Cofree ContExprF [Int] -- A contraction equation, each subexpression annotated with its shape

calcDims :: ContExpr -> ContEq
calcDims = cata alg where
    alg (node@(TensorF _ idxs)) = idxs :< node
    alg (node@(SumF idx ops)) = dims :< node where
        dims = minus (foldl' union [] $ map extract ops) [idx]

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
tensorName n = singleton $ chr $ ord 'A' + n

indexName :: Int -> Text
indexName n = singleton $ chr $ ord 'i' + n