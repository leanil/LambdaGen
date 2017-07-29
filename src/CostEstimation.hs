{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators, ViewPatterns #-}

module CostEstimation where

import Expr
import Recursion
import Type
import Typecheck
import Control.Comonad.Cofree
import Data.Functor.Foldable
import Data.Vinyl
import Data.Vinyl.Functor

newtype Cost = Cost { cost :: Int } deriving (Eq, Show)

costEstAlg :: TypecheckT ∈ fields => RAlgebra (Cofree ExprF (R fields)) Cost

costEstAlg (_ ::< Scalar _) = Cost 1

costEstAlg (_ ::< Variable{}) = Cost 1

costEstAlg (_ ::< VectorView{}) = Cost 1

costEstAlg (_ ::< Vector elements) = Cost $ sum $ map (cost . snd) elements

costEstAlg (_ ::< Addition (_,a) (_,b)) = Cost $ cost a + cost b + 1

costEstAlg (_ ::< Multiplication (_,a) (_,b)) = Cost $ cost a + cost b + 1

costEstAlg (_ ::< Apply (_,a) (_,b)) = Cost $ cost a + cost b + 1

costEstAlg (_ ::< Lambda _ _ (_,a)) = a

costEstAlg (t ::< Map (_,f) (_,v)) = Cost $ cost f * getDim t + cost v + 1 where
    getDim (getType -> (Fix (Power _ (Fix (Dim s))))) = s

costEstAlg (_ ::< Reduce (_,f) (t :< _,v)) = Cost $ cost f * getDim t + cost v + 1 where
    getDim (getType -> (Fix (Power _ (Fix (Dim s))))) = s

costEstAlg (t ::< ZipWith (_,f) (_,v1) (_,v2)) = Cost $ cost f * getDim t + cost v1 + cost v2 + 1 where
    getDim (getType -> (Fix (Power _ (Fix (Dim s))))) = s

getCost :: Cost ∈ fields => R fields -> Int
getCost = cost . fieldVal