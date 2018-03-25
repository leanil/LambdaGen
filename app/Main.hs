{-# LANGUAGE DataKinds, FlexibleContexts, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}

import CodeGeneration
import ConstFold
import Cpp
import Expr
import FunctionalTest
import Parallel
import Print
import Recursion
import Storage
import Typecheck
import Control.Comonad (extract)
import Data.Functor.Foldable (cata, para)
import Data.List (intercalate)
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl

test :: Expr0
test = fst test4

process :: TypecheckT ∈ fields => Expr fields -> Expr (CodeGenT ': ResultPack ': Result ': ParData ': fields)
process expr =
    para (annotatePara codeGenAlg) $
    cata (annotate collectStgAlg) $
    assignStorage $
    parallelize 4 $
    cata constFoldAlg expr

main :: IO ()
main = do
    let tcd = cata (annotate typecheckAlg) test
    case fieldVal @TypecheckT $ extract tcd of
        (Left _) -> do
            let prd = process tcd
            writeFile "../result.hpp" $ createEvaluator "evaluator" $ extract prd
            putStr $ printExpr (Proxy :: Proxy (R '[TypecheckT, ParData, Result])) prd
            --let var1 = cata constFoldAlg tcd
            --putStr $ printExpr (Proxy :: Proxy (R '[TypecheckT])) var1
        (Right errors) ->
            putStr $ intercalate "\n" errors ++ "\n\n" ++
            printExpr (Proxy :: Proxy (R '[TypecheckT])) tcd

getLeft :: Either a b -> a
getLeft (Left l) = l