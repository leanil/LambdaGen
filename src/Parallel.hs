{-# LANGUAGE DataKinds, FlexibleContexts, TupleSections, TypeOperators, ViewPatterns #-}

module Parallel where

import Expr
import Recursion
import Type
import Typecheck
import Control.Comonad.Cofree (Cofree(..))
import Data.Vinyl
import Data.Vinyl.Functor (Identity(..))
import Data.Functor.Foldable (ana)

data ParState = None | Started Int | Full Int
type ParData = (Int, Maybe Int) -- (threads at this point, threads to start here)

parallelizerAlg :: TypecheckT ∈ fields => Int -> CoAlgebra (Cofree ExprF ParData) (Cofree ExprF (R fields), ParState)

parallelizerAlg _ (_ :< Apply a b, None) = (1, Just 1) ::< Apply (a,Started 1) (zip b (repeat None))
parallelizerAlg _ (_ :< Apply a b, Started i) = (1, Nothing) ::< Apply (a,Started i) (zip b (repeat None))

parallelizerAlg _ (_ :< Let n v e, Started i) = (1, Nothing) ::< Let n (v,None) (e,Started i)

parallelizerAlg _ ((getType -> (FArrow _ FArrow{})) :< a, p@(Started _)) = parData p ::< fmap (,p) a
parallelizerAlg _ (_ :< a, p@(Started i)) = parData p ::< fmap (,Full i) a

parallelizerAlg t (_ :< Map a b, None) = (1, Just t) ::< Map (a,Started t) (b,None)

parallelizerAlg t (_ :< Reduce a b, None) = (1, Just t) ::< Reduce (a,Started t) (b,None)

parallelizerAlg t (_ :< ZipWith a b c, None) = (1, Just t) ::< ZipWith (a,Started t) (b,None) (c,None)

parallelizerAlg _ (_ :< a, p) = parData p ::< fmap (,p) a

parData :: ParState -> ParData
parData None        = (1, Nothing)
parData (Started i) = (1, Just i)
parData (Full i)    = (i, Nothing)

parallelize :: TypecheckT ∈ fields => Int -> Cofree ExprF (R fields) -> Cofree ExprF (R (ParData ': fields))
parallelize t = root . ana (annotateAna $ parallelizerAlg t) . (,None) where
    root n@((getParData -> (_, Just _)) :< _) = n
    root (r :< n) = rput (Identity ((1, Just 1) :: ParData)) r :< n

getParData :: ParData ∈ fields => R fields -> ParData
getParData (fieldVal -> p) = p