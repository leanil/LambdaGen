{-# LANGUAGE DataKinds, FlexibleContexts, TupleSections, TypeOperators, ViewPatterns #-}

module Parallel where

import Expr
import Recursion
import Type
import Typecheck
import Control.Comonad.Cofree (Cofree(..))
import Data.Functor.Foldable (Fix(..))
import Data.Vinyl
import Data.Vinyl.Functor (Identity(..))
import Data.Functor.Foldable (ana)

data ParState = None | Started Int | Full Int
type ParData = (Int, Maybe Int) -- (threads at this point, threads to start here)

parallelizerAlg :: TypecheckT ∈ fields => Int -> CoAlgebra (Cofree ExprF (R (ParData ': fields))) ((Cofree ExprF (R fields)), ParState)

parallelizerAlg _ (r :< Scalar x, p) = (Identity (parData p) :& r) ::< Scalar x

parallelizerAlg _ (r :< VectorView a b c, p) = (Identity (parData p) :& r) ::< VectorView a b c

parallelizerAlg _ (r :< Vector elements, p) = (Identity (parData p) :& r) ::< (Vector $ map (,p) elements)

parallelizerAlg _ (r :< Addition a b, p) = (Identity (parData p) :& r) ::< Addition (a,p) (b,p)

parallelizerAlg _ (r :< Multiplication a b, p) = (Identity (parData p) :& r) ::< Multiplication (a,p) (b,p)

parallelizerAlg _ (r :< Apply a b, None) = (Identity (1, Just 1) :& r) ::< Apply (a,Started 1) (b,None)
parallelizerAlg _ (r :< Apply a b, Started i) = (Identity (1, Nothing) :& r) ::< Apply (a,Started i) (b,None)
parallelizerAlg _ (r :< Apply a b, p@Full{}) = (Identity (parData p) :& r) ::< Apply (a,p) (b,p)

parallelizerAlg _ (r :< Lambda a b c, Started i) = (Identity (i, Just i) :& r) ::<
    case getType r of
        (Fix (Arrow _ (Fix Arrow{}))) -> Lambda a b (c,Started i)
        _                             -> Lambda a b (c, Full i)
parallelizerAlg _ (r :< Lambda a b c, p) = (Identity (parData p) :& r) ::< Lambda a b (c,p)

parallelizerAlg _ (r :< Variable i t, p) = (Identity (parData p) :& r) ::< Variable i t

parallelizerAlg t (r :< Map a b, None) = (Identity (1, Just t) :& r) ::< Map (a,Started t) (b,None)
parallelizerAlg _ (r :< Map a b, p@Full{}) = (Identity (parData p) :& r) ::< Map (a,p) (b,p)

parallelizerAlg t (r :< Reduce a b, None) = (Identity (1, Just t) :& r) ::< Reduce (a,Started t) (b,None)
parallelizerAlg _ (r :< Reduce a b, p@Full{}) = (Identity (parData p) :& r) ::< Reduce (a,p) (b,p)

parallelizerAlg t (r :< ZipWith a b c, None) = (Identity (1, Just t) :& r) ::< ZipWith (a,Started t) (b,None) (c,None)
parallelizerAlg _ (r :< ZipWith a b c, p@Full{}) = (Identity (parData p) :& r) ::< ZipWith (a,p) (b,p) (c,p)

parData :: ParState -> ParData
parData None = (1, Nothing)
parData (Full i) = (i, Nothing)

parallelize :: TypecheckT ∈ fields => Int -> Cofree ExprF (R fields) -> Cofree ExprF (R (ParData ': fields))
parallelize t = root . ana (parallelizerAlg t) . (,None) where
    root t@((getParData -> (_, Just _)) :< _) = t
    root (r :< t) = rput (Identity ((1, Just 1) :: ParData)) r :< t

getParData :: ParData ∈ fields => R fields -> ParData
getParData (fieldVal -> p) = p