module ConstFold where

import Expr
import Recursion
import Control.Comonad.Cofree (Cofree(..))
import Data.Functor.Foldable

constFoldAlg :: Algebra (Cofree ExprF a) (Cofree ExprF a)

constFoldAlg (r ::< ScalarOp '+' (_ :< Scalar a) (_ :< Scalar b)) = r :< Scalar (a+b)

constFoldAlg (_ ::< ScalarOp '+' (_ :< Scalar 0) a) = a

constFoldAlg (_ ::< ScalarOp '+' a (_ :< Scalar 0)) = a

constFoldAlg (r ::< ScalarOp '*' (_ :< Scalar a) (_ :< Scalar b)) = r :< Scalar (a*b)

constFoldAlg (_ ::< ScalarOp '*' (_ :< Scalar 1) a) = a

constFoldAlg (_ ::< ScalarOp '*' a (_ :< Scalar 1)) = a

constFoldAlg (r ::< ScalarOp '*' (_ :< Scalar 0) _) = r :< Scalar 0

constFoldAlg (r ::< ScalarOp '*' _ (_ :< Scalar 0)) = r :< Scalar 0

constFoldAlg x = embed x