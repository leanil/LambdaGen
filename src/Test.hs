{-# LANGUAGE FlexibleContexts, TypeApplications, TypeFamilies #-}

import CodeGeneration
import ConstFold
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
import Text.Printf (printf)

process test result =
    let tc = cata (annotate typecheckAlg) test in
    case fieldVal @TypecheckT $ extract tc of
    (Left _) -> writeFile result $
                createEvaluator $ extract $
                para (annotatePara codeGenAlg) $
                cata (annotate collectStgAlg) $
                assignStorage $
                parallelize 4 $
                cata constFoldAlg tc

    (Right errors) -> print errors

main =
    foldM (\_ (t,i) -> process t ("result" ++ printf "%02d" i ++ ".hpp")) () (zip funcTests [(1::Int)..])
