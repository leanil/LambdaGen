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
import Control.Monad (foldM)
import Data.Functor.Foldable (cata, para)
import System.IO (print)

process test result =
    let tc = cata (annotate typecheckAlg) test in
    case fieldVal ([] :: [TypecheckT]) $ extract tc of
    (Left _) -> writeFile result $
                createEvaluator $ getCode $ extract $
                para (annotatePara codeGenAlg) $
                cata (annotate collectStgAlg) $
                assignStorage $
                parallelize 8 tc

    (Right errors) -> print errors

main =
    foldM (\_ (t,i) -> process t ("result" ++ show i ++ ".hpp")) () (zip funcTests [1..])
