{-# LANGUAGE DataKinds, FlexibleContexts, TypeFamilies #-}

import CodeGeneration
import Cpp
import Expr
import Input
import Parallel
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

main =
    let tc = cata (annotate typecheckAlg) input in
    case fieldVal ([] :: [TypecheckT]) $ extract tc of
    (Left _) ->       writeFile "result.hpp" $
                      createEvaluator $ extract $
                      para (annotatePara codeGenAlg) $
                      cata (annotate collectStgAlg) $
                      assignStorage $
                      parallelize 8 tc
    (Right errors) -> putStr $ intercalate "\n" errors ++ "\n\n" ++
                      printExpr (Proxy :: Proxy (R '[TypecheckT])) tc
