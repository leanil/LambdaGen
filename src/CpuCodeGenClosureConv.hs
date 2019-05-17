{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedStrings, TypeOperators, ViewPatterns #-}

module CpuCodeGenClosureConv where

<<<<<<< HEAD
import Prelude hiding (null)
=======
>>>>>>> master
import ClosureConversion
import CppTemplateClosureConv
import Expr
import Metrics
<<<<<<< HEAD
import Naming
=======
>>>>>>> master
import Recursion
import StorageClosureConv
import Type
import Typecheck
<<<<<<< HEAD
import Utility (tshow)
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree(..))
import Control.Monad.Writer.Lazy (Writer, runWriter, tell)
import Data.Foldable (fold)
import Data.Functor.Foldable (para)
import Data.Map.Strict (assocs, mapWithKey, toList, null)
import Data.Maybe (fromJust, catMaybes)
import Data.Set (member)
import Data.Text (Text, append, pack, singleton, unpack)
import Data.Vinyl


type Functions = [(Text,Text)]

cpuCodeGenAlg :: (TypecheckT ∈ fields, NodeId ∈ fields, LetId ∈ fields, Callee ∈ fields, ClosureT ∈ fields, IsFreeVar ∈ fields, ParamSet ∈ fields, HofSpecId ∈ fields, OwnStorage ∈ fields) =>
 MRAlgebra (Expr fields) (Writer Functions) CodeT

cpuCodeGenAlg (r ::< node) =
    let eval = fold $ fmap (getEval . snd) node
        resultName = case (fieldVal r, fieldVal r, getType r) of
            (OwnStorage False, _, _)  -> OutParam
            (_, _, FPower{})          -> Tensor $ getMemId r
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

        RnZ (_,reducer) (_,zipper) (map snd -> vecNames) ->
            return $ rnzAppTemplate eval resultName hofSpecId reducer zipper (getNodeId r) vecNames where
                hofSpecId = fromJust $ getHofSpec $ fieldVal r

        ZipWithN (_,code) (map snd -> vecNames) ->
            return $ zipAppTemplate eval resultName hofSpecId code vecNames where
                hofSpecId = fromJust $ getHofSpec $ fieldVal r

        LayoutOp op (_,code) ->
            return $ layoutOpTemplate eval resultName op shape code where
                shape = case getType r of FPower _ s -> unzip s          

textToMaybe :: Text -> Maybe Text
textToMaybe "" = Nothing
textToMaybe t = Just t

cpuCodeGen :: (TypecheckT ∈ fields, NodeId ∈ fields, LetId ∈ fields, Callee ∈ fields, ClosureT ∈ fields, IsFreeVar ∈ fields, ParamSet ∈ fields, HofSpecId ∈ fields, OwnStorage ∈ fields) => 
    String -> Expr fields -> HofSpec -> [Storage] -> (Text,Text)
cpuCodeGen evalName expr (HofSpec rnzSpec zipSpec) storage =
    cpuEvaluatorTemplate closures storage (funs ++ hofs) retT (pack evalName) eval code where
        closures = map (fmap toList) $ getClosureList $ fieldVal $ extract expr
        (CodeT code eval, funs) = runWriter (paraM cpuCodeGenAlg expr)
        hofs = concatMap rnzTemplate (assocs rnzSpec) ++ map zipTemplate (assocs zipSpec)
        retT = getType $ extract expr
=======
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree(..))
import Data.Functor.Foldable (para)
import Data.Text (Text, append, pack, singleton, unpack)
import Data.Vinyl

data CpuCodeT = CpuCodeT { getCode :: Text, getFunctions :: [Text] } deriving Show

cpuCodeGenAlg :: (NodeId ∈ fields, Result ∈ fields, TypecheckT ∈ fields, IsFreeVar ∈ fields, ClosureT ∈ fields) => RAlgebra (Expr fields) CpuCodeT

--cpuCodeGenAlg ((fieldVal -> Std _ False _) ::< Scalar{}) = CpuCodeT ""
cpuCodeGenAlg (_ ::< Scalar x) = CpuCodeT (pack $ show x) []

--cpuCodeGenAlg ((fieldVal -> Std _ False _) ::< View{}) = CpuCodeT ""
cpuCodeGenAlg (_ ::< View name _ _) = CpuCodeT (pack name) []

--cpuCodeGenAlg ((fieldVal -> Std _ False _) ::< Variable{}) = CpuCodeT ""
cpuCodeGenAlg (r ::< Variable name _) = CpuCodeT (pack $ closure ++ name) [] where 
    closure = case fieldVal r of
        IsFreeVar True -> "_cl."
        otherwise      -> ""

cpuCodeGenAlg (_ ::< ScalarOp op (_,CpuCodeT codeA funA) (_,CpuCodeT codeB funB)) =
    CpuCodeT (scalarOpTemplate (singleton op) codeA codeB) (funA ++ funB)

-- cpuCodeGenAlg (_ ::< Apply (getParamNames -> pa,CpuCodeT a) (unzipCodes -> (evals,names))) =
--     CpuCodeT $ appTemplate evals a pa names where
        
-- cpuCodeGenAlg (_ ::< Lambda _ binds (_,CpuCodeT a)) =
--     CpuCodeT $ lambdaTemplate evals names values a where
--         (evals, names, values) = unzip3 $ map (\(n,(r :< _,CpuCodeT b)) -> (b,pack n,getResultId r)) binds

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

-- cpuCodeGen :: (NodeId ∈ fields, Result ∈ fields, ResultPack ∈ fields, TypecheckT ∈ fields, IsFreeVar ∈ fields, ClosureT ∈ fields) => 
--     String -> Expr fields -> String
-- cpuCodeGen evalName expr = unpack $ cpuEvaluatorTemplate (pack evalName) userData allocViews evalCode resultName where
--     ResultPack (alloc, user) = fieldVal $ extract expr
--     userData = map (\(BigVector (pack -> name) (pack -> dataId) mem) -> 
--         viewTemplate "double const*" "double" mem name ("(userData[\"" `append` dataId `append` "\"])")) user
--     allocViews = map (\(ResultStg (pack -> name) _ mem) -> viewTemplate "double*" "double" mem name "") alloc
--     CpuCodeT evalCode = para cpuCodeGenAlg expr
--     resultName = getResultId $ extract expr

-- indent :: String -> String -> String
-- indent tabs code = unlines (map (tabs++) (lines code))

-- withNL :: String -> String
-- withNL [] = []
-- withNL s  = s ++ ",\n"

-- getResultId :: Result ∈ fields => R fields -> Text
-- getResultId (fieldVal -> Std resultId _ _) = pack resultId
>>>>>>> master
