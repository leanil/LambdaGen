module CostEstimation where

import Data.Functor.Foldable

import Expr
import Recursion

type Cost = Int

costEstAlg :: ExprF (Fix ExprF, Cost) -> Cost

costEstAlg (Scalar _) = 1

costEstAlg (Variable _ _) = 1

costEstAlg (VectorView _ _) = 1

costEstAlg (Vector elements) = sum $ map snd elements

costEstAlg (Addition (_,a) (_,b)) = a + b + 1

costEstAlg (Multiplication (_,a) (_,b)) = a + b + 1

costEstAlg (Apply (_,a) (_,b)) = a + b + 1

costEstAlg (Lambda _ _ (_,a)) = a

costEstAlg (Map (_,f) (Fix (Vector e), v)) = f * length e + v + 1

costEstAlg (Reduce (_,f) (Fix (Vector e), v)) = f * length e + v + 1

costEstAlg (ZipWith (_,f) (Fix (Vector e), v1) (_, v2)) = f * length e + v1 + v2 + 1
