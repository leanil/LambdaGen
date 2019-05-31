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

        View name shape -> return $ viewTemplate resultName "double*" "double" shape (pack name)

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
                shape = case getType r of FPower _ s -> s          

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
