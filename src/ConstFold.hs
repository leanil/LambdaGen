module ConstFold where

import Expr
import Recursion
import Control.Comonad.Cofree (Cofree(..))
import Data.Functor.Foldable

constFoldAlg :: Algebra (Cofree ExprF a) (Cofree ExprF a)

constFoldAlg (r ::< Addition (_ :< Scalar a) (_ :< Scalar b)) = r :< Scalar (a+b)

constFoldAlg (_ ::< Addition (_ :< Scalar 0) a) = a

constFoldAlg (_ ::< Addition a (_ :< Scalar 0)) = a

constFoldAlg (r ::< Multiplication (_ :< Scalar a) (_ :< Scalar b)) = r :< Scalar (a*b)

constFoldAlg (_ ::< Multiplication (_ :< Scalar 1) a) = a

constFoldAlg (_ ::< Multiplication a (_ :< Scalar 1)) = a

constFoldAlg (r ::< Multiplication (_ :< Scalar 0) _) = r :< Scalar 0

constFoldAlg (r ::< Multiplication _ (_ :< Scalar 0)) = r :< Scalar 0

constFoldAlg x = embed x