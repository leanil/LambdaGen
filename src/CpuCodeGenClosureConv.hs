{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedStrings, TypeOperators, ViewPatterns #-}

module CpuCodeGenClosureConv where

import Prelude hiding (null)
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
import Control.Monad.State (State, evalState, get, modify, runState)
import Data.Functor.Foldable (para)
import Data.Map.Strict (mapWithKey, toList, null)
import Data.Maybe (fromJust, catMaybes)
import Data.Set (member)
import Data.Text (Text, append, pack, singleton, unpack)
import Data.Vinyl


data CpuCodeT = CpuCodeT { getEvals :: [Text], getFunctions :: [(Text,Text)] } deriving Show
instance Semigroup CpuCodeT where (CpuCodeT evalA funA) <> (CpuCodeT evalB funB) = CpuCodeT (evalA ++ evalB) (funA ++ funB)
instance Monoid CpuCodeT where mempty = CpuCodeT [] []

cpuCodeGenAlg :: (TypecheckT ∈ fields, NodeId ∈ fields, Callee ∈ fields, ClosureT ∈ fields, IsFreeVar ∈ fields, ParamSet ∈ fields, HofSpecId ∈ fields, OwnStorage ∈ fields) =>
 MRAlgebra (Expr fields) (State CpuCodeT) Text

cpuCodeGenAlg (_ ::< Scalar x) = return $ tshow x

cpuCodeGenAlg (r ::< View name dims strides) = 
    return $ viewTemplate (tResultTensor $ getNodeId r) "double*" "double" dims strides (pack name)

cpuCodeGenAlg ((fieldVal -> IsFreeVar free) ::< Variable name _) = 
    return $ varTemplate name free

cpuCodeGenAlg (_ ::< ScalarOp op (_,codeA) (_,codeB)) =
    return $ scalarOpTemplate (singleton op) codeA codeB

cpuCodeGenAlg (r ::< Apply (_,textToMaybe -> lam) args) = do
    modify $ (<> CpuCodeT evals [])
    return $ appTemplate callee wrapped lam (map fst args') outPar where 
        args' = map (\(r' :< _, arg) -> case getType r' of
            FPower{} -> (tResultTensor $ getNodeId r', Just arg) where 
            FDouble -> (arg, Nothing)) args
        evals = catMaybes $ map snd args'
        callee = head $ getCallee $ fieldVal r
        (outPar,wrapped) = case (getType r, ownStorage $ fieldVal r) of
                (FArrow{},_)     -> (Nothing, False)
                (_, False)       -> (Just outParamName, False)
                (FPower{}, True) -> (Just $ tResultTensor $ getNodeId r, False)
                (FDouble, True)  -> (Nothing, True)
        
cpuCodeGenAlg (r ::< Lambda params binds (r' :< _,code)) = do
    CpuCodeT evals _ <- get
    let funs = lambdaTemplate retT (getNodeId r) hasClosure params binds' code bodyOwnsStorage
        vars = toList $ mapWithKey (\name _ -> not $ member name $ getParamSet r) $ getClosure $ fieldVal r
        retT = getExType r'
        hasClosure = not $ null $ getClosure $ fieldVal r
        binds' = map (\(name, (r'' :< _,code)) -> letBindingTemplate (getExType r'') (pack name) code) binds ++ evals
        bodyOwnsStorage = ownStorage $ fieldVal r'
    modify $ (\(CpuCodeT _ fs) -> CpuCodeT [] (fs ++ funs))
    return $ makeClosureTemplate vars
    

cpuCodeGenAlg (r ::< RnZ (rR :< _,codeRed) (rZ :< _,codeZip) vecs) = do
    modify $ (<> CpuCodeT evals [])
    return $ rnzTemplate hofSpecId codeRed codeZip vecNames outParam where
        evals = map snd vecs
        hofSpecId = fromJust $ getHofSpec $ fieldVal r
        -- redName = lamIdToName $ getNodeId rR
        -- zipName = lamIdToName $ getNodeId rZ
        vecNames = map (tResultTensor . getNodeId . extract . fst) vecs
        outParam = case (getType r, ownStorage $ fieldVal r) of
            (_, False)       -> Just outParamName
            (FPower{}, True) -> Just $ tResultTensor $ getNodeId r
            (FDouble, True)  -> Nothing


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

cpuCodeGen :: (TypecheckT ∈ fields, NodeId ∈ fields, Callee ∈ fields, ClosureT ∈ fields, IsFreeVar ∈ fields, ParamSet ∈ fields, HofSpecId ∈ fields, OwnStorage ∈ fields) => 
    String -> Expr fields -> HofSpec -> [Storage] -> String
cpuCodeGen evalName expr hofSpec storage =
    unpack $ cpuEvaluatorTemplate closures funs retT (pack evalName) code where
        closures = map (fmap toList) $ getClosureList $ fieldVal $ extract expr
        (code, CpuCodeT evals funs) = runState (paraM cpuCodeGenAlg expr) mempty
        retT = getType $ extract expr

-- indent :: String -> String -> String
-- indent tabs code = unlines (map (tabs++) (lines code))

-- withNL :: String -> String
-- withNL [] = []
-- withNL s  = s ++ ",\n"

-- getResultId :: Result ∈ fields => R fields -> Text
-- getResultId (fieldVal -> Std resultId _ _) = pack resultId