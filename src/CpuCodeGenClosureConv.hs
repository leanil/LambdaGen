{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedStrings, TypeOperators, ViewPatterns #-}

module CpuCodeGenClosureConv where

import Prelude hiding (filter, null)
import ClosureConversion
import CppTemplateClosureConv
import Expr
import Metrics
import Naming
import Recursion
import StorageClosureConv
import Type
import Typecheck
import Utility (tshow)
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree(..))
import Control.Monad.Writer.Lazy (Writer, runWriter, tell)
import Data.Foldable (fold)
import Data.Functor.Foldable (para)
import Data.Map.Strict (mapWithKey, toList, null)
import Data.Maybe (fromJust, catMaybes)
import Data.Set (member)
import Data.Text (Text, append, filter, pack, singleton, unpack)
import Data.Vinyl


type Functions = [(Text,Text)]

cpuCodeGenAlg :: (TypecheckT ∈ fields, NodeId ∈ fields, LetId ∈ fields, Callee ∈ fields, ClosureT ∈ fields, IsFreeVar ∈ fields, ParamSet ∈ fields, HofSpecId ∈ fields, OwnStorage ∈ fields) =>
 MRAlgebra (Expr fields) (Writer Functions) CodeT

cpuCodeGenAlg (r ::< node) =
    let eval = fold $ fmap (getEval . snd) node
        resultName = case (fieldVal r, fieldVal r, getType r) of
            (OwnStorage False, _, _)  -> OutParam
            (_, _, FPower{})          -> Tensor $ pack $ getMemId r
            (_, LetId (Just name), _) -> LetBound $ pack name 
            otherwise                 -> None

    in case (fmap (fmap getValue) node) of

        Scalar x -> return $ scalarTemplate resultName x

        View name dims strides -> return $ viewTemplate resultName "double*" "double" dims strides (pack name)

        Variable name _ -> return $ variableTemplate (getLetId $ fieldVal r) resultName name (isFreeVar $ fieldVal r)

        ScalarOp op (_,codeA) (_,codeB) -> return $ scalarOpTemplate eval resultName (singleton op) codeA codeB

        Apply (_,lam) (map snd -> args) -> return $ appTemplate eval resultName callee lam args where
            callee = head $ getCallee $ fieldVal r

        Lambda params binds (r' :< _,code) -> do
            tell funs
            return $ makeClosureTemplate resultName (getNodeId r) closureVars
            where
                funs = lambdaTemplate (getExType r') (getNodeId r) params eval code
                closureVars = toList $ mapWithKey (\name _ -> not $ member name $ getParamSet r) $ getClosure $ fieldVal r

        
    
-- cpuCodeGenAlg (r ::< RnZ (rR :< _,codeRed) (rZ :< _,codeZip) vecs) = do
--     modify $ (<> CpuCodeT evals [])
--     return $ rnzTemplate hofSpecId codeRed codeZip vecNames outParam where
--         evals = map snd vecs
--         hofSpecId = fromJust $ getHofSpec $ fieldVal r
--         -- redName = lamIdToName $ getNodeId rR
--         -- zipName = lamIdToName $ getNodeId rZ
--         vecNames = map (tResultTensor . getNodeId . extract . fst) vecs
--         outParam = case (getType r, ownStorage $ fieldVal r) of
--             (_, False)       -> Just outParamName
--             (FPower{}, True) -> Just $ tResultTensor $ getNodeId r
--             (FDouble, True)  -> Nothing


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

textToMaybe :: Text -> Maybe Text
textToMaybe "" = Nothing
textToMaybe t = Just t

cpuCodeGen :: (TypecheckT ∈ fields, NodeId ∈ fields, LetId ∈ fields, Callee ∈ fields, ClosureT ∈ fields, IsFreeVar ∈ fields, ParamSet ∈ fields, HofSpecId ∈ fields, OwnStorage ∈ fields) => 
    String -> Expr fields -> HofSpec -> [Storage] -> Text
cpuCodeGen evalName expr hofSpec storage = filter (\c -> c /= '\r') genCode where
        genCode = cpuEvaluatorTemplate closures funs retT (pack evalName) eval code
        closures = map (fmap toList) $ getClosureList $ fieldVal $ extract expr
        (CodeT code eval, funs) = runWriter (paraM cpuCodeGenAlg expr)
        retT = getType $ extract expr

-- indent :: String -> String -> String
-- indent tabs code = unlines (map (tabs++) (lines code))

-- withNL :: String -> String
-- withNL [] = []
-- withNL s  = s ++ ",\n"

-- getResultId :: Result ∈ fields => R fields -> Text
-- getResultId (fieldVal -> Std resultId _ _) = pack resultId