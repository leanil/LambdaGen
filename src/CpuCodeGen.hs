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
import Data.Text (Text, append, pack, unpack)
import Data.Vinyl

newtype CpuCodeT = CpuCodeT Text deriving Show
cpuCodeGenAlg :: (Result ∈ fields, NodeId ∈ fields, TypecheckT ∈ fields) => RAlgebra (Expr fields) CpuCodeT

cpuCodeGenAlg ((fieldVal -> Std (_,True)) ::< Scalar{}) = CpuCodeT ""
cpuCodeGenAlg ((fieldVal -> Std (mem,_)) ::< Scalar x) = CpuCodeT $ assignTemplate (pack mem) (pack (show x))

cpuCodeGenAlg ((fieldVal -> Std (_,True)) ::< VectorView{}) = CpuCodeT ""
cpuCodeGenAlg ((fieldVal -> Std (mem,_)) ::< VectorView name _ _) = CpuCodeT $ assignTemplate (pack mem) (pack name)

cpuCodeGenAlg ((fieldVal -> Std (_,True)) ::< Variable{}) = CpuCodeT ""
cpuCodeGenAlg ((fieldVal -> Std (mem,_)) ::< Variable name _) = CpuCodeT $ assignTemplate (pack mem) (pack name)

cpuCodeGenAlg (r ::< Addition (ra :< _,CpuCodeT a) (rb :< _,CpuCodeT b)) =
    CpuCodeT $ addTemplate a b (getResultId r) (getResultId ra) (getResultId rb)

cpuCodeGenAlg (r ::< Multiplication (ra :< _,CpuCodeT a) (rb :< _,CpuCodeT b)) =
    CpuCodeT $ mulTemplate a b (getResultId r) (getResultId ra) (getResultId rb)

cpuCodeGenAlg (_ ::< Apply (_,CpuCodeT a) vals) =
    CpuCodeT $ appTemplate evals a names where
        (evals,names) = unzip $ map (\(rb :< _,CpuCodeT b) -> (b,getResultId rb)) vals

cpuCodeGenAlg (_ ::< Let n (ra :< _,CpuCodeT a) (_,CpuCodeT b)) =
    CpuCodeT $ letTemplate a (pack n) (getResultId ra) b
       
cpuCodeGenAlg (_ ::< Lambda vs (_,CpuCodeT a)) =
    CpuCodeT $ lambdaTemplate (map (append "auto " . pack . fst) vs) a

cpuCodeGenAlg (r ::< Map (_,CpuCodeT a) (rb :< _,CpuCodeT b)) =
    CpuCodeT $ mapTemplate b (pack $ makeHofIdx r) (pack $ show $ getVecSize rb) a (getResultId rb)

cpuCodeGenAlg (r ::< Reduce (_,CpuCodeT a) (rb :< _,CpuCodeT b)) =
    CpuCodeT $ reduceTemplate b (getResultId r) (pack $ makeHofIdx r) (pack $ show $ getVecSize rb) a (getResultId rb)

cpuCodeGenAlg (r ::< ZipWith (_,CpuCodeT a) (rb :< _,CpuCodeT b) (rc :< _,CpuCodeT c)) =
    CpuCodeT $ zipWithTemplate b c (pack $ makeHofIdx r) (pack $ show $ getVecSize rb) a (getResultId rb) (getResultId rc)

getVecSize :: TypecheckT ∈ fields => R fields -> Int
getVecSize (getType -> FPower _ (FDim s)) = s

cpuCodeGen :: (Result ∈ fields, ResultPack ∈ fields, NodeId ∈ fields, TypecheckT ∈ fields) => 
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
getResultId (fieldVal -> Std (resultId,_)) = pack resultId