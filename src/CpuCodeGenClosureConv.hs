{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedStrings, TypeOperators, ViewPatterns #-}

module CpuCodeGenClosureConv where

import Prelude hiding (null)
import ClosureConversion
import CppTemplateClosureConv
import Expr
import Metrics
import Recursion
import StorageClosureConv
import Type
import Typecheck
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree(..))
import Data.Functor.Foldable (para)
import Data.Map.Strict (mapWithKey, toList, null)
import Data.Maybe (fromJust)
import Data.Set (member)
import Data.Text (Text, append, pack, singleton, unpack)
import Data.Vinyl

data CpuCodeT = CpuCodeT { getCode :: Text, getFunctions :: [(Text,Text)] } deriving Show

cpuCodeGenAlg :: (TypecheckT ∈ fields, NodeId ∈ fields, Callee ∈ fields, ClosureT ∈ fields, IsFreeVar ∈ fields, ParamSet ∈ fields, Result ∈ fields) => RAlgebra (Expr fields) CpuCodeT

cpuCodeGenAlg (_ ::< Scalar x) = CpuCodeT (pack $ show x) []

cpuCodeGenAlg (_ ::< View name _ _) = CpuCodeT (pack name) []

cpuCodeGenAlg ((fieldVal -> IsFreeVar free) ::< Variable name _) = CpuCodeT (pack $ closure ++ name) [] where 
    closure = if free then "_cl." else ""

cpuCodeGenAlg (_ ::< ScalarOp op (_,CpuCodeT codeA funA) (_,CpuCodeT codeB funB)) =
    CpuCodeT (scalarOpTemplate (singleton op) codeA codeB) (funA ++ funB)

cpuCodeGenAlg (r ::< Apply (_,CpuCodeT codeLam funLam) (map snd -> args)) =
    CpuCodeT (appTemplate (fromJust $ getCallee $ fieldVal r) codeLam $ map (\(CpuCodeT arg _) -> arg) args) $ foldr (\(CpuCodeT _ fs) funs -> funs ++ fs) funLam args
        
cpuCodeGenAlg (r ::< Lambda params binds (r' :< _,CpuCodeT code funs)) =
    CpuCodeT (makeClosureTemplate vars) $ lambdaTemplate retT (getNodeId r) hasClosure params binds' code : funs' where
        vars = toList $ mapWithKey (\name _ -> member name $ getParamSet r) $ getClosure $ fieldVal r
        retT = getExType r'
        hasClosure = not $ null $ getClosure $ fieldVal r
        binds' = map (\(name, (r'' :< _,CpuCodeT code _)) -> (getExType r'', pack name, code)) binds
        funs' = foldr (\(_,(_,CpuCodeT _ f)) fs -> fs ++ f) funs binds

-- cpuCodeGenAlg (r ::< RnZ (getParamNames -> pa,CpuCodeT a) (getParamNames -> pb,CpuCodeT b) vs@(unzipCodes -> (evals,names))) =
--     CpuCodeT $ rnzTemplate evals resultId temp (pack $ makeHofIdx r) (pack $ show $ getVecSize vs) a pa b pb names where
--         resultId = getResultId r
--         temp = append "tmp_" $ pack $ show $ getNodeId r

-- cpuCodeGenAlg (r ::< ZipWithN (getParamNames -> pa,CpuCodeT a) vs@(unzipCodes -> (evals,names))) =
--     CpuCodeT $ zipWithNTemplate evals (pack $ makeHofIdx r) (pack $ show $ getVecSize vs) a pa names

-- cpuCodeGenAlg (r ::< Flip (i,j) (ra :< _,CpuCodeT a)) =
--     CpuCodeT $ flipTemplate a auto (getResultId r) (pack $ show i) (pack $ show j) (getResultId ra) where
--         auto = case fieldVal r of
--             Std _ True _ -> ""
--             Std _ False _ -> "auto "

-- cpuCodeGenAlg (r ::< Subdiv i b (ra :< _,CpuCodeT a)) =
--     CpuCodeT $ subdivTemplate a auto (getResultId r) (pack $ show i) (pack $ show b) (getResultId ra) where
--         auto = case fieldVal r of
--             Std _ True _ -> ""
--             Std _ False _ -> "auto "

-- cpuCodeGenAlg (r ::< Flatten i (ra :< _,CpuCodeT a)) =
--     CpuCodeT $ flattenTemplate a auto (getResultId r) (pack $ show i) (getResultId ra) where
--         auto = case fieldVal r of
--             Std _ True _ -> ""
--             Std _ False _ -> "auto "            

-- unzipCodes :: Result ∈ fields => [(Expr fields,CpuCodeT)] -> ([Text],[Text])
-- unzipCodes = unzip . map (\(r :< _,CpuCodeT a) -> (a,getResultId r))

-- getVecSize :: TypecheckT ∈ fields => [(Expr fields,a)] -> Int
-- getVecSize (((getType -> FPower _ ((s,_):_)) :< _,_):_) = s

-- getParamNames :: Expr fields -> [Text]
-- getParamNames (_ :< Lambda params _ _) = map (pack . fst) params

cpuCodeGen :: (TypecheckT ∈ fields, NodeId ∈ fields, Callee ∈ fields, ClosureT ∈ fields, IsFreeVar ∈ fields, ParamSet ∈ fields, Result ∈ fields, ResultPack ∈ fields) => 
    String -> Expr fields -> String
cpuCodeGen evalName expr = unpack $ cpuEvaluatorTemplate closures funs retT (pack evalName) code where
    closures = map (fmap toList) $ getClosureList $ fieldVal $ extract expr
    CpuCodeT code (reverse -> funs) = para cpuCodeGenAlg expr
    retT = getType $ extract expr

-- indent :: String -> String -> String
-- indent tabs code = unlines (map (tabs++) (lines code))

-- withNL :: String -> String
-- withNL [] = []
-- withNL s  = s ++ ",\n"

-- getResultId :: Result ∈ fields => R fields -> Text
-- getResultId (fieldVal -> Std resultId _ _) = pack resultId