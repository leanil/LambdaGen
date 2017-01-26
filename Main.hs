{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

import CodeGeneration
import CostEstimation
import Cpp
import ErrorTest
import Expr
import FunctionalTest
import Parallel
import PerformanceTest
import Recursion
import Storage
import Type
import Typecheck
import Control.Comonad (extract)
import Data.Functor.Foldable (cata, para)
import System.IO (print)

test = funcTest9

tc = cata (annotate typecheckAlg) test

main =
    case fieldVal ([] :: [TypecheckT]) $ extract tc of
    (Left _) -> writeFile "../test/result.hpp" $
                createEvaluator $ getCode $ extract $
                para (annotatePara codeGenAlg) $
                cata (annotate collectStgAlg) $
                assignStorage $
                parallelize 8 tc

    (Right errors) -> print errors
