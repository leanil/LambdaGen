{-# LANGUAGE DataKinds, FlexibleContexts, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}

import ClosureConversion
import ConstFold
import CpuCodeGenInliner
import Expr
import FunctionalTest
import Metrics
import Parallel
import qualified PerformanceTest
import Print
import Recursion
import Replace
import StorageClosureConv
import Transformation
import Typecheck
import Control.Comonad (extract)
import Data.Functor.Foldable (cata)
import Data.List (intercalate)
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl

import Data.Map.Strict
import Control.Monad.State

test :: Expr0
test = fst calleeCheck -- closureConvCheck

process :: TypecheckT ∈ fields => Expr fields -> 
    Expr (ResultPack ': Result ': ParData ': IsFreeVar ': Callee ': ClosureT ': LetId ': NodeId ': fields)
process expr =
    collectStorage $
    assignStorage $
    parallelize 4 $
    closureConversion $
    assignLetId $
    assignNodeId $
    cata constFoldAlg expr

compile :: String -> String -> Expr0 -> IO ()
compile fileName kernelName expr = do
    let tcd = cata (annotate typecheckAlg) $ makeSymbolsUnique expr
    case fieldVal @TypecheckT $ extract tcd of
        (Left _) -> do
            let rep = replaceAll partialApp partialAppTrans tcd
            let prd = process $ typecheck' rep
            --writeFile fileName $ cpuCodeGen kernelName prd
            putStr $ printExpr (Proxy :: Proxy (R '[Callee, NodeId])) prd
        (Right errors) ->
            putStr $ intercalate "\n" errors ++ "\n\n" ++
            printExpr (Proxy :: Proxy (R '[TypecheckT])) tcd

main :: IO ()
main = do
    compile "../test/result.hpp" "eval" test
    -- compile "../test/result1.hpp" "matMatMul" PerformanceTest.matMatMul
    -- putStrLn ""
    -- compile "../test/result2.hpp" "matMatMulTrans" PerformanceTest.matMatMulTrans
    

getLeft :: Either a b -> a
getLeft (Left l) = l