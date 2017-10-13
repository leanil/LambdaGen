{-# LANGUAGE DataKinds, FlexibleContexts, TypeApplications, TypeFamilies #-}

import CodeGeneration
import ConstFold
import CostEstimation
import Cpp
import ErrorTest
import Expr
import FunctionalTest
import Parallel
import PerformanceTest
import Print
import Recursion
import Storage
import Type
import Typecheck
import Control.Comonad (extract)
import Data.Functor.Foldable (cata, para)
import Data.List (intercalate)
import Data.Proxy (Proxy(Proxy))
import System.IO (print)

test = test1

process expr =
    para (annotatePara codeGenAlg) $
    cata (annotate collectStgAlg) $
    assignStorage $
    parallelize 4 $
    cata constFoldAlg expr

main = do
    let tcd = cata (annotate typecheckAlg) test
    case fieldVal @TypecheckT $ extract tcd of
        (Left _) -> do
            -- let prd = process tcd
            -- writeFile "../test/result.hpp" $ createEvaluator $ extract prd
            -- putStr $ printExpr (Proxy :: Proxy (R '[TypecheckT, ParData, Result])) prd
            let var1 = cata constFoldAlg tcd
            putStr $ printExpr (Proxy :: Proxy (R '[TypecheckT])) var1
        (Right errors) ->
            putStr $ intercalate "\n" errors ++ "\n\n" ++
            printExpr (Proxy :: Proxy (R '[TypecheckT])) tcd
