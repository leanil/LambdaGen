{-# LANGUAGE DataKinds, FlexibleContexts, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}

import ClosureConversion
import ConstFold
import CpuCodeGenClosureConv
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
import Data.Text.IO as T (writeFile)
import Data.Vinyl

import Data.Map.Strict
import Control.Monad.State
import LinAlg

test :: Expr0
test = fst calleeCheck

process :: TypecheckT ∈ fields => Expr fields -> 
    (Expr (OwnStorage ': HofSpecId ': ParamSet ': IsFreeVar ': ClosureT ': Callee ': LetId ': NodeId ': fields), HofSpec, [Storage])
process expr = (expr'', hofSpec, storage) where
    (expr'',storage) = assignStorage expr'
    (expr',hofSpec) = closureConversion $ assignLetId $ assignNodeId {-$ cata constFoldAlg-} expr

compile :: String -> String -> Expr0 -> IO ()
compile fileName kernelName expr = do
    let tcd = cata (annotate typecheckAlg) $ makeSymbolsUnique expr
    case fieldVal @TypecheckT $ extract tcd of
        (Left _) -> do
            let rep = replaceAll partialApp partialAppTrans tcd
            let (prd,hofSpec,storage) = process $ typecheck' rep
            T.writeFile fileName $ cpuCodeGen kernelName prd hofSpec storage
            putStr $ printExpr (Proxy :: Proxy (R '[])) prd
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