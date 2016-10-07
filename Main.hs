import Data.Functor.Foldable

import CostEstimation
import ErrorTest
import Expr
import FunctionalTest
import Type
import Typecheck
import TypePrinter
import Recursion
import Control.Comonad.Cofree

getAnnotation (b :< _) = b

test = funcTest6

tc = cata (attrCata typecheckAlg) test

main = case tc of
    (Left _ :< _) -> print $ getAnnotation $ para (attrCofPara costEstAlg) $ cata extractTypeAlg tc
    (Right errors :< _ ) -> print errors
