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

test = funcTest8

tc = cata (attrCata typecheckAlg) test

main = case tc of
    (Left _ :< _) -> writeFile "../test/result.hpp" $
                        createEvaluator $ fst $ 
                        cata (codeGenAlg 1000) $
                        para (attrCofPara costEstAlg) $
                        cata extractTypeAlg tc
                        
    (Right errors :< _ ) -> print errors
