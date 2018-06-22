{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedStrings, TypeOperators, ViewPatterns #-}

module CpuCodeGen where

import CppTemplate
import Expr
import Metrics
import Recursion
import Storage
import Type
import Typecheck
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree(..))
import Data.Functor.Foldable (para)
import Data.Text (Text, append, pack, singleton, unpack)
import Data.Vinyl

newtype CpuCodeT = CpuCodeT Text deriving Show
cpuCodeGenAlg :: (NodeId ∈ fields, Result ∈ fields, NodeId ∈ fields, TypecheckT ∈ fields) => RAlgebra (Expr fields) CpuCodeT

cpuCodeGenAlg ((fieldVal -> Std _ False _) ::< Scalar{}) = CpuCodeT ""
cpuCodeGenAlg ((fieldVal -> Std mem True _) ::< Scalar x) = CpuCodeT $ assignTemplate (pack mem) (pack (show x))

cpuCodeGenAlg ((fieldVal -> Std _ False _) ::< View{}) = CpuCodeT ""
cpuCodeGenAlg ((fieldVal -> Std mem True _) ::< View name _ _) = CpuCodeT $ assignTemplate (pack mem) (pack name)

cpuCodeGenAlg ((fieldVal -> Std _ False _) ::< Variable{}) = CpuCodeT ""
cpuCodeGenAlg ((fieldVal -> Std mem True _) ::< Variable name _) = CpuCodeT $ assignTemplate (pack mem) (pack name)

cpuCodeGenAlg (r ::< ScalarOp op (ra :< _,CpuCodeT a) (rb :< _,CpuCodeT b)) =
    CpuCodeT $ scalarOpTemplate a b (getResultId r) (getResultId ra) (singleton op) (getResultId rb)

cpuCodeGenAlg (_ ::< Apply (getParamNames -> pa,CpuCodeT a) (unzipCodes -> (evals,names))) =
    CpuCodeT $ appTemplate evals a pa names where
        
cpuCodeGenAlg (_ ::< Lambda _ binds (_,CpuCodeT a)) =
    CpuCodeT $ lambdaTemplate evals names values a where
        (evals, names, values) = unzip3 $ map (\(n,(r :< _,CpuCodeT b)) -> (b,pack n,getResultId r)) binds

cpuCodeGenAlg (r ::< RnZ (getParamNames -> pa,CpuCodeT a) (getParamNames -> pb,CpuCodeT b) vs@(unzipCodes -> (evals,names))) =
    CpuCodeT $ rnzTemplate evals resultId temp (pack $ makeHofIdx r) (pack $ show $ getVecSize vs) a pa b pb names where
        resultId = getResultId r
        temp = append "tmp_" $ pack $ show $ getNodeId r

cpuCodeGenAlg (r ::< ZipWithN (getParamNames -> pa,CpuCodeT a) vs@(unzipCodes -> (evals,names))) =
    CpuCodeT $ zipWithNTemplate evals (pack $ makeHofIdx r) (pack $ show $ getVecSize vs) a pa names

cpuCodeGenAlg (r ::< Flip (i,j) (ra :< _,CpuCodeT a)) =
    CpuCodeT $ flipTemplate a auto (getResultId r) (pack $ show i) (pack $ show j) (getResultId ra) where
        auto = case fieldVal r of
            Std _ True _ -> ""
            Std _ False _ -> "auto "

cpuCodeGenAlg (r ::< Subdiv i b (ra :< _,CpuCodeT a)) =
    CpuCodeT $ subdivTemplate a auto (getResultId r) (pack $ show i) (pack $ show b) (getResultId ra) where
        auto = case fieldVal r of
            Std _ True _ -> ""
            Std _ False _ -> "auto "

cpuCodeGenAlg (r ::< Flatten i (ra :< _,CpuCodeT a)) =
    CpuCodeT $ flattenTemplate a auto (getResultId r) (pack $ show i) (getResultId ra) where
        auto = case fieldVal r of
            Std _ True _ -> ""
            Std _ False _ -> "auto "            

unzipCodes :: Result ∈ fields => [(Expr fields,CpuCodeT)] -> ([Text],[Text])
unzipCodes = unzip . map (\(r :< _,CpuCodeT a) -> (a,getResultId r))

getVecSize :: TypecheckT ∈ fields => [(Expr fields,a)] -> Int
getVecSize (((getType -> FPower _ ((s,_):_)) :< _,_):_) = s

getParamNames :: Expr fields -> [Text]
getParamNames (_ :< Lambda params _ _) = map (pack . fst) params

cpuCodeGen :: (NodeId ∈ fields, Result ∈ fields, ResultPack ∈ fields, NodeId ∈ fields, TypecheckT ∈ fields) => 
    String -> Expr fields -> String
cpuCodeGen evalName expr = unpack $ cpuEvaluatorTemplate (pack evalName) userData allocViews evalCode resultName where
    ResultPack (alloc, user) = fieldVal $ extract expr
    userData = map (\(BigVector (pack -> name) (pack -> dataId) mem) -> 
        viewTemplate "double const*" "double" mem name ("(userData[\"" `append` dataId `append` "\"])")) user
    allocViews = map (\(ResultStg (pack -> name) _ mem) -> viewTemplate "double*" "double" mem name "") alloc
    CpuCodeT evalCode = para cpuCodeGenAlg expr
    resultName = getResultId $ extract expr

indent :: String -> String -> String
indent tabs code = unlines (map (tabs++) (lines code))

withNL :: String -> String
withNL [] = []
withNL s  = s ++ ",\n"

getResultId :: Result ∈ fields => R fields -> Text
getResultId (fieldVal -> Std resultId _ _) = pack resultId