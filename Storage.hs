{-# LANGUAGE DataKinds, FlexibleContexts, TupleSections, TypeOperators, ViewPatterns #-}

module Storage where

import Expr
import Recursion
import Typecheck
import TypePrinter
import Control.Arrow
import Control.Comonad.Cofree
import Data.List (unfoldr)
import Data.Vinyl
import Data.Vinyl.Functor 
import Data.Functor.Foldable (ana)
import System.Random

type AssignStgT = (StdGen, Bool)
type ResultStg = Maybe Int

assignStgAlg :: TypecheckT ∈ fields => CoAlgebra (Cofree ExprF (R (ResultStg ': fields))) ((Cofree ExprF (R fields)), AssignStgT)

assignStgAlg (r :< Scalar x, s) = (Identity (snd $ assignHelper s r) :& r) ::< Scalar x

assignStgAlg (r :< VectorView a b c, s) = (Identity (snd $ assignHelper s r) :& r) ::< VectorView a b c

assignStgAlg (r :< Vector elements, s) = (Identity s' :& r) ::< (Vector $ zip elements (map (,True) $ unfoldr (Just .split) g'))where
    (g', s') = assignHelper s r

assignStgAlg (r :< Addition a b, s) = (Identity s' :& r) ::< Addition (a,(g1,True)) (b,(g2,True)) where
    (g', s') = assignHelper s r
    (g1, g2) = split g'

assignStgAlg (r :< Multiplication a b, s) = (Identity s' :& r) ::< Multiplication (a,(g1,True)) (b,(g2,True)) where
    (g', s') = assignHelper s r
    (g1, g2) = split g'

assignStgAlg (r :< Apply a b, s) = (Identity s' :& r) ::< Apply (a,(g1,False)) (b,(g2,True)) where
    (g', s') = assignHelper s r
    (g1, g2) = split g'

assignStgAlg (r :< Lambda i t a, s) = (Identity Nothing :& r) ::< Lambda i t (a,s)

assignStgAlg (r :< Variable i t, s) = (Identity (snd $ assignHelper s r) :& r) ::< Variable i t

assignStgAlg (r :< Map a b, s) = (Identity s' :& r) ::< Map (a,(g1,False)) (b,(g2,True)) where
        (g', s') = assignHelper s r
        (g1, g2) = split g'

assignStgAlg (r :< Reduce a b, s) = (Identity s' :& r) ::< Reduce (a,(g1,False)) (b,(g2,True)) where
        (g', s') = assignHelper s r
        (g1, g2) = split g'

assignStgAlg (r :< ZipWith a b c, s) = (Identity s' :& r) ::< ZipWith (a,(g1,False)) (b,(g2,True)) (c,(g3,True)) where
        (g', s') = assignHelper s r
        (g1, (g2, g3)) = fmap split $ split g'

assignHelper :: TypecheckT ∈ fields => AssignStgT -> R fields -> (StdGen, ResultStg)
assignHelper (g, False) _ = (g, Nothing)
assignHelper (g, True) (getType -> t) = (g', Just x) where
    (x, g') = next g

assignStorage :: TypecheckT ∈ fields => Cofree ExprF (R fields) -> Cofree ExprF (R (ResultStg ': fields))
assignStorage e = ana assignStgAlg (e,(mkStdGen 0, True))

type ResultPack = [(Int,[Int],[Int])]

collectStgAlg :: (ResultStg ∈ fields, TypecheckT ∈ fields) => Algebra (Cofree ExprF (R fields)) ResultPack

collectStgAlg (r ::< Scalar{}) = getStgAndDims r

collectStgAlg (r ::< VectorView{}) = getStgAndDims r

collectStgAlg (r ::< Vector elements) = getStgAndDims r ++ concat elements

collectStgAlg (r ::< Addition a b) = getStgAndDims r ++ a ++ b

collectStgAlg (r ::< Multiplication a b) = getStgAndDims r ++  a ++ b

collectStgAlg (r ::< Apply a b) = getStgAndDims r ++ a ++ b

collectStgAlg (r ::< Lambda _ _ a) = getStgAndDims r ++ a

collectStgAlg (r ::< Variable{}) = getStgAndDims r

collectStgAlg (r ::< Map a b) = getStgAndDims r ++ a ++ b
    
collectStgAlg (r ::< Reduce a b) = getStgAndDims r ++ a ++ b

collectStgAlg (r ::< ZipWith a b c) = getStgAndDims r ++ a ++ b ++ c

getStgAndDims :: (TypecheckT ∈ fields, ResultStg ∈ fields) => R (fields) -> ResultPack
getStgAndDims (fieldVal ([] :: [ResultStg]) &&& countDims . getType -> (Just x, ds)) = [(x, ds, defaultStrides ds)]
getStgAndDims (fieldVal ([] :: [ResultStg]) &&& countDims . getType -> (Nothing, _)) = []
