{-# LANGUAGE DataKinds, FlexibleContexts, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}


import ConstFold
import CpuCodeGen
import Expr
import FunctionalTest
import Metrics
import Parallel
import Print
import Recursion
import Replace
import Storage
import Transformation
import Typecheck
import Control.Comonad (extract)
import Data.Functor.Foldable (cata)
import Data.List (intercalate)
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl

test :: Expr0
test = fst test5

process :: TypecheckT ∈ fields => Expr fields -> 
    Expr (ResultPack ': Result ': ParData ': NodeId ': SubtreeSize ': fields)
process expr =
    collectStorage $
    assignStorage $
    parallelize 4 $
    assignNodeId $
    cata constFoldAlg expr

main :: IO ()
main = do
    let tcd = cata (annotate typecheckAlg) $ makeSymbolsUnique test
    case fieldVal @TypecheckT $ extract tcd of
        (Left _) -> do
            let rep = replaceAll partialAppPat partialAppRep partialAppConstraint partialAppTransform tcd
            let recheck = cata (annotate typecheckAlg) rep
            let prd = process recheck
            writeFile "../test/result.hpp" $ cpuCodeGen "eval" prd
            putStr $ printExpr (Proxy :: Proxy (R '[TypecheckT, ParData, Result])) prd
        (Right errors) ->
            putStr $ intercalate "\n" errors ++ "\n\n" ++
            printExpr (Proxy :: Proxy (R '[TypecheckT])) tcd

getLeft :: Either a b -> a
getLeft (Left l) = l