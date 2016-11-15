{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

import CodeGeneration
import CostEstimation
import Cpp
import ErrorTest
import Expr
import FunctionalTest
import Recursion
import Storage
import Type
import Typecheck
import Control.Comonad (extract)
import Data.Functor.Foldable (cata, para)
import System.IO (print)

test = funcTest6

tc = cata (annotate typecheckAlg) test

main =
    case fieldVal ([] :: [TypecheckT]) $ extract tc of
    (Left _) -> writeFile "../test/result.hpp" $
                createEvaluator $ getCode $ extract $
                cata (annotate $ codeGenAlg 1000) $
                cata (annotate collectStgAlg) $
                assignStorage $
                para (annotatePara costEstAlg) tc
                        
    (Right errors) -> print errors
