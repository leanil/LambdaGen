import Data.Functor.Foldable

import CodeGeneration
import CostEstimation
import Cpp
import ErrorTest
import Expr
import FunctionalTest
import Recursion
import Type
import Typecheck
import TypePrinter
import Control.Comonad.Cofree
import System.IO

getAnnotation (b :< _) = b

test = funcTest6

tc = cata (attrCata typecheckAlg) test

main = case tc of
    (Left _ :< _) -> writeFile "../test/result.cpp" $
                        createEvaluator $ fst $ 
                        cata (codeGenAlg 20) $
                        para (attrCofPara costEstAlg) $
                        cata extractTypeAlg tc
                        
    (Right errors :< _ ) -> print errors
