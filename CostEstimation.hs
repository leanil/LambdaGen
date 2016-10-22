module CostEstimation where

import Expr
import Recursion
import Type
import Control.Comonad.Cofree
import Data.Functor.Foldable

type Cost = Int

costEstAlg :: RAlgebra (Cofree ExprF Type) Cost

costEstAlg (MyPair (_, Scalar _)) = 1

costEstAlg (MyPair (_,Variable{})) = 1

costEstAlg (MyPair (_,VectorView{})) = 1

costEstAlg (MyPair (_,Vector elements)) = sum $ map snd elements

costEstAlg (MyPair (_, Addition (_,a) (_,b))) = a + b + 1

costEstAlg (MyPair (_, Multiplication (_,a) (_,b))) = a + b + 1

costEstAlg (MyPair (_, Apply (_,a) (_,b))) = a + b + 1

costEstAlg (MyPair (_, Lambda _ _ (_,a))) = a

costEstAlg (MyPair (Fix (Power _ (Fix (Dim s))), Map (_,f) (_,v))) = f * s + v + 1

costEstAlg (MyPair (_, Reduce (_,f) (Fix (Power _ (Fix (Dim s))) :< _,v))) = f * s + v + 1

costEstAlg (MyPair (Fix (Power _ (Fix (Dim s))), ZipWith (_,f) (_,v1) (_,v2))) = f * s + v1 + v2 + 1

--costEstAlg a = error (show a)
