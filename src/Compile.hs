{-# LANGUAGE DataKinds, FlexibleContexts, TypeApplications, TypeOperators #-}

module Compile where

import ClosureConversion
import ConstFold
import CpuCodeGenClosureConv
import Expr
import Metrics
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
import Data.Text (Text, unpack)
import Data.Text.IO as T (writeFile)
import Data.Vinyl
import System.FilePath (FilePath,(</>),(<.>))

type NewFields = [OwnStorage, HofSpecId, ParamSet, IsFreeVar, ClosureT,Callee, LetId, NodeId, TypecheckT]

process :: Expr0 -> 
    (Expr NewFields, HofSpec, [Storage])
process expr = (expr'', hofSpec, storage) where
    (expr'',storage) = assignStorage expr'
    (expr',hofSpec) = closureConversion $ assignLetId $ assignNodeId {-$ cata constFoldAlg-} $ typecheck' expr

compile :: FilePath -> Text -> Expr0 -> IO (Maybe (Expr NewFields))
compile path kernelName expr = do
    let tcd = cata (annotate typecheckAlg) $ makeSymbolsUnique expr
    case fieldVal @TypecheckT $ extract tcd of
        Left _ -> do
            let rep = replaceAll partialApp partialAppTrans tcd
                (prd,hofSpec,storage) = process rep
                (header,cpp) = cpuCodeGen kernelName prd hofSpec storage
            T.writeFile (path </> unpack kernelName <.> "h") header
            T.writeFile (path </> unpack kernelName <.> "cpp") cpp
            return $ Just prd
        Right errors -> do
            putStr $ intercalate "\n" errors ++ "\n\n" ++
                printExpr (Proxy :: Proxy (R '[TypecheckT])) tcd
            return Nothing