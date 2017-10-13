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

parallelizerAlg :: TypecheckT ∈ fields => Int -> CoAlgebra (Cofree ExprF ParData) (Cofree ExprF (R fields), ParState)

parallelizerAlg _ (r :< Scalar x, p) = parData p ::< Scalar x

parallelizerAlg _ (r :< VectorView a b c, p) = parData p ::< VectorView a b c

parallelizerAlg _ (r :< Vector elements, p) = parData p ::< (Vector $ map (,p) elements)

parallelizerAlg _ (r :< Addition a b, p) = parData p ::< Addition (a,p) (b,p)

parallelizerAlg _ (r :< Multiplication a b, p) = parData p ::< Multiplication (a,p) (b,p)

parallelizerAlg _ (r :< Apply a b, None) = (1, Just 1) ::< Apply (a,Started 1) (b,None)
parallelizerAlg _ (r :< Apply a b, Started i) = (1, Nothing) ::< Apply (a,Started i) (b,None)
parallelizerAlg _ (r :< Apply a b, p@Full{}) = (parData p) ::< Apply (a,p) (b,p)

parallelizerAlg _ (r :< Lambda a b c, Started i) = (i, Just i) ::<
    case getType r of
        (Fix (Arrow _ (Fix Arrow{}))) -> Lambda a b (c,Started i)
        _                             -> Lambda a b (c, Full i)
parallelizerAlg _ (r :< Lambda a b c, p) = parData p ::< Lambda a b (c,p)

parallelizerAlg _ (r :< Variable i t, p) = parData p ::< Variable i t

parallelizerAlg t (r :< Map a b, None) = (1, Just t) ::< Map (a,Started t) (b,None)
parallelizerAlg _ (r :< Map a b, p@Full{}) = parData p ::< Map (a,p) (b,p)

parallelizerAlg t (r :< Reduce a b, None) = (1, Just t) ::< Reduce (a,Started t) (b,None)
parallelizerAlg _ (r :< Reduce a b, p@Full{}) = parData p ::< Reduce (a,p) (b,p)

parallelizerAlg t (r :< ZipWith a b c, None) = (1, Just t) ::< ZipWith (a,Started t) (b,None) (c,None)
parallelizerAlg _ (r :< ZipWith a b c, p@Full{}) = parData p ::< ZipWith (a,p) (b,p) (c,p)

parData :: ParState -> ParData
parData None = (1, Nothing)
parData (Full i) = (i, Nothing)

parallelize :: TypecheckT ∈ fields => Int -> Cofree ExprF (R fields) -> Cofree ExprF (R (ParData ': fields))
parallelize t = root . ana (annotateAna $ parallelizerAlg t) . (,None) where
    root t@((getParData -> (_, Just _)) :< _) = t
    root (r :< t) = rput (Identity ((1, Just 1) :: ParData)) r :< t

getParData :: ParData ∈ fields => R fields -> ParData
getParData (fieldVal -> p) = p