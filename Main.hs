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

getError (b :< _) = b
--getError _ = Nothing

test = funcTest5

main = case cata typecheckAlg test of
    Left _ -> print $ getError $ para (attribute costEstAlg) test
    Right errors -> print errors
