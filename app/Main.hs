{-# LANGUAGE DataKinds #-}

<<<<<<< HEAD
import Compile
=======
import ClosureConversion
import ConstFold
import CpuCodeGenInliner
>>>>>>> master
import Expr
import FunctionalTest
import Metrics
import Print
import Recursion
<<<<<<< HEAD
=======
import Replace
import StorageClosureConv
import Transformation
>>>>>>> master
import Typecheck
import Control.Comonad (extract)
import Data.Functor.Foldable (cata)
import Data.List (intercalate)
import Data.Proxy (Proxy(Proxy))
import Data.Text.IO as T (writeFile)
import System.FilePath ((</>))

import Data.Map.Strict
import Control.Monad.State

test :: Expr0
<<<<<<< HEAD
test = fst rSubdiv
=======
test = fst calleeCheck -- closureConvCheck

process :: TypecheckT ∈ fields => Expr fields -> 
    Expr (ResultPack ': Result ': ParData ': IsFreeVar ': Callee ': ClosureT ': NodeId ': SubtreeSize ': fields)
process expr =
    collectStorage $
    assignStorage $
    parallelize 4 $
    closureConversion $
    assignNodeId $
    cata constFoldAlg expr

compile :: String -> String -> Expr0 -> IO ()
compile fileName kernelName expr = do
    let tcd = cata (annotate typecheckAlg) $ makeSymbolsUnique expr
    case fieldVal @TypecheckT $ extract tcd of
        (Left _) -> do
            let rep = replaceAll partialApp partialAppTrans tcd
            print $ runState (cataM calleeAlg $ assignNodeId rep) empty 
            let prd = process $ typecheck' rep
            --writeFile fileName $ cpuCodeGen kernelName prd
            putStr $ printExpr (Proxy :: Proxy (R '[Callee, NodeId])) prd
        (Right errors) ->
            putStr $ intercalate "\n" errors ++ "\n\n" ++
            printExpr (Proxy :: Proxy (R '[TypecheckT])) tcd
>>>>>>> master

main :: IO ()
main = do
    result <- compile (".." </> "test") "eval" test
    case result of
        Just expr -> putStr $ printExpr (Proxy :: Proxy (R '[TypecheckT])) expr
        Nothing -> return ()
