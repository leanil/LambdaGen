{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}

module CppTemplateClosureConv where

import ClosureConversion
import Naming
import StorageClosureConv
import Type
import Utility (mapFst, tshow)
import Control.Arrow ((&&&))
import Data.Maybe (isJust, maybeToList)
import Data.Text (Text, append, concat, cons, intercalate, pack, snoc, strip, unlines, unpack)
import qualified Data.Text as T (null)
import NeatInterpolation
import Prelude hiding (concat, unlines)

data CodeT = CodeT { getValue :: Text, getEval :: Text } -- evaulator code may be empty
data StoreResult = OutParam | Tensor Text | LetBound Text | None

assignTemplate :: Text -> Text -> Text
assignTemplate name value = [text|$name = $value;|]

declTemplate :: Text -> Text -> Text
declTemplate ty name = [text|$ty $name;|]

initTemplate :: Text -> Text -> Text -> Text
initTemplate ty name val = [text|$ty $name = $val;|]

scalarTemplate :: StoreResult -> Double -> CodeT
scalarTemplate result (tshow -> x) = case result of
        OutParam -> CodeT outParamName (assignTemplate outParamName x)
        LetBound name -> CodeT name (initTemplate "double" name x)
        None -> CodeT x ""

viewTemplate :: StoreResult -> Text -> Text -> [Int] -> [Int] -> Text -> CodeT
viewTemplate result pointerT dataT dims strides dataName = case result of
    OutParam -> CodeT outParamName (assignTemplate outParamName code)
    Tensor name -> CodeT name [text|$viewT $name(userData["$dataName"]);|]
    None -> CodeT code ""
    where
        viewT = viewTypeTemplate pointerT dataT (dims,strides)
        code = strip [text|$viewT(userData["$dataName"])|]

varTemplate :: String -> Bool -> Text
varTemplate (pack -> name) free = strip $ if free then [text|$closureParamName.$name|] else [text|$name|]

variableTemplate :: Maybe String -> StoreResult -> String -> Bool -> CodeT
variableTemplate letId result name free = case letId of
    Just (pack -> letId) -> CodeT letId (initTemplate "auto" letId code) -- TODO: store template type of function parameter tensors and use it here. Or remove strides from view type.
    Nothing -> case result of
        OutParam -> CodeT outParamName (assignTemplate outParamName code)
        _ -> CodeT code ""
    where code = varTemplate name free

scalarOpTemplate :: Text -> StoreResult -> Text -> Text -> Text -> CodeT
scalarOpTemplate eval result op lhs rhs = case result of
    OutParam -> CodeT outParamName (append eval $ assignTemplate outParamName code)
    LetBound name -> CodeT name (append eval $ initTemplate "double" name code)
    None -> CodeT code eval
    where
        code = strip [text|($lhs) $op ($rhs)|]

appTemplate :: Text -> StoreResult -> Int -> Text -> [Text] -> CodeT
appTemplate eval result (lamIdToName -> funName) closure args = case outParam of
    Just name -> CodeT name (append eval [text|$code;|])
    _ -> case result of
        None -> CodeT code eval
        LetBound name -> CodeT name (append eval $ initTemplate "double" name code)
    where
        outParam = case result of
            OutParam -> Just outParamName
            Tensor name -> Just name
            _ -> Nothing
        code = strip $ [text|$funName($args')|]
        args' = intercalate ", " $ closure : args ++ maybeToList outParam
        
-- | The Bool value tells if the variable is from the parameter set of the enclosing lambda (or inherited from it's closure).
makeClosureTemplate :: StoreResult -> Int -> [(String, Bool)] -> CodeT
makeClosureTemplate result (lamIdToClosure -> ty) vars = case result of
    LetBound name -> CodeT name $ initTemplate ty name code
    None          -> CodeT code ""
    where
        code = strip [text|{$vars'}|]
        vars' = intercalate ", " $ map (uncurry varTemplate) vars
    
paramTemplate :: (Text,Text) -> Text
paramTemplate (name,ty) = strip [text|$ty $name|]

-- | A lambda function's result can be a scalar value, a tensor or another lambda.
--   Tensors should be returned through an 'out' parameter to avoid redundant memory allocation.
--   On the other hand, Lambdas (closures) should be returned regularly, to make nested calls easier.
--   Scalar valued lambdas can be used in both kinds of contexts.

lambdaTemplate :: ExType -> Int -> [(String,Type)] -> Text -> Text -> [(Text,Text)]
lambdaTemplate retT lamId (map (mapFst pack) -> params) eval body =
    [([text|
        $templateParams
        $retT' $name($params'')|],
    [text|
        $eval
        $return|])] ++ wrapper
    where
        templateParams = templateParamsTemplate templateCnt
        (retT',outParam,return) = case retT of
            Right FDouble -> ("void", [(outParamName,power double [])], "")
            Right ty@FPower{} -> ("void", [(outParamName,ty)],"")
            Left{}   -> (cppExType retT, [], [text|return $body;|])
        name = lamIdToName lamId
        params'' = intercalate ", " $ map paramTemplate $ closure ++ params'
        wrapper = case retT of
            Right FDouble -> [wrapperFunctionTemplate (templateCnt-1) name (closure ++ init params'::[(Text,Text)]) ]
            otherwise     -> []
        (reverse -> params',templateCnt) = foldl hideParams ([],0) $ params ++ outParam where
            hideParams (ps,cnt) (name,FDouble) = ((name,"double"):ps, cnt)
            hideParams (ps,cnt) (name,FPower{}) = ((name,templateArgName $ cnt+1):ps, cnt+1)
        closure = [(closureParamName,lamIdToClosure lamId)]

templateParamsTemplate :: Int -> Text
templateParamsTemplate 0   = ""
templateParamsTemplate cnt = [text|template<$params>|] where
    params = intercalate ", " $ map (append "typename " . templateArgName) [1..cnt]

wrapperFunctionTemplate :: Int -> Text -> [(Text,Text)] -> (Text,Text)
wrapperFunctionTemplate templateCnt funName params =
    ([text|
        $templateParams
        double $funName($params')|],
    [text|
        double result;
        $funName($args, View<double*, double, to_list_t<>, true>(&result));
        return result;|])
    where
        templateParams = templateParamsTemplate templateCnt
        params' = intercalate ", " $ map paramTemplate params
        args = intercalate ", " $ map fst params

cppType :: Type -> Text
cppType FDouble = "double"
cppType (FPower _ dims) = viewTypeTemplate "double*" "double" (unzip dims)

cppExType :: ExType -> Text
cppExType = either lamIdToClosure cppType

rnzAppTemplate :: Text -> StoreResult -> Int -> Text -> Text -> Int -> [Text] -> CodeT
rnzAppTemplate eval result hofId reducer zipper nodeId vecNames = case result of
    OutParam -> CodeT outParamName $ append eval [text|$code;|]
    Tensor name -> CodeT name $ append eval [text|$code;|]
    LetBound name -> CodeT name (append eval $ initTemplate "double" name code)
    None -> CodeT code eval
    where
        code = [text|$hofName($reducer, $zipper, $params)|] where
        hofName = rnzIdToName hofId `append` if isJust outParam then "" else wrapperSuffix
        params = intercalate ", " $ maybeToList outParam ++ (tempStorage : vecNames) 
        outParam = case result of
            OutParam -> Just outParamName
            Tensor name -> Just name
            _ -> Nothing
        tempStorage = tResultTensor nodeId `append` tmpSuffix

rnzTemplate :: ((Int,Int),(Type,Int)) -> [(Text,Text)]
rnzTemplate ((redId, zipId), (ty, rnzIdToName -> hofName)) =
    ([text|
        template<typename _T1, typename _T2, typename... _T>
        void $hofName($clRed _clRed, $clZip _clZip, _T1 _result, _T2 _tmp, _T... vecs)|],
    [text|
        for (int i = 0; i < _tmp.size; ++i)
            $lamZip(_clZip, vecs[i]..., _tmp[i]);
        _result = _tmp[0];
        for (int i = 1; i < _tmp.size; ++i)
            $lamRed(_clRed, _result, _tmp[i], _result);|]) : wrapper
    where
        (clRed,lamRed) = lamIdToClosure &&& lamIdToName $ redId
        (clZip,lamZip) = lamIdToClosure &&& lamIdToName $ zipId
        wrapper = case ty of
            FDouble -> [rnzWrapperTemplate clRed clZip hofName]
            _       -> []

rnzWrapperTemplate :: Text -> Text -> Text -> (Text,Text)
rnzWrapperTemplate clRed clZip hofName =
    ([text|
        template<typename _T1, typename... _T>
        double $wrapName($clRed _clRed, $clZip _clZip, _T1 _tmp, _T... vecs)|],
    [text|
        double result;
        $hofName(_clRed, _clZip, View<double*, double, to_list_t<>, true>(&result), _tmp, vecs...);
        return result;|])
    where
        wrapName = append hofName wrapperSuffix

zipTemplate = undefined

-- zipWithNTemplate :: [Text] -> Text -> Text -> Text -> [Text] -> [Text] -> Text
-- zipWithNTemplate (concat -> evalVecs) idx size lambda params args =
--     let setParams =  makeAssignments params (indexedVecs args idx) in
--     [text|
--         $evalVecs
--         for (int $idx = 0; $idx < $size; ++$idx) {
--             $setParams
--             $lambda
--         }
--     |]

-- flipTemplate :: Text -> Text -> Text -> Text -> Text -> Text -> Text
-- flipTemplate rhsCode auto result idx1 idx2 rhsResult =
--     [text|
--         $rhsCode
--         $auto$result = flip<$idx1>($rhsResult);
--     |]

-- subdivTemplate :: Text -> Text -> Text -> Text -> Text -> Text -> Text
-- subdivTemplate rhsCode auto result idx block rhsResult =
--     [text|
--         $rhsCode
--         $auto$result = subdiv<$idx,$block>($rhsResult);
--     |]

-- flattenTemplate :: Text -> Text -> Text -> Text -> Text -> Text
-- flattenTemplate rhsCode auto result idx rhsResult =
--     [text|
--         $rhsCode
--         $auto$result = flatten<$idx>($rhsResult);
--     |]

viewDimElemTemplate :: (Int, Int) -> Text
viewDimElemTemplate (1,1) = ""
viewDimElemTemplate (tshow -> size, tshow -> stride) =
    strip [text|P<$size,$stride>|]

viewDimListTemplate :: ([Int], [Int]) -> Text
viewDimListTemplate (intercalate ", " . map viewDimElemTemplate . uncurry zip -> dims) =
    [text|to_list_t<$dims>|]

viewTypeTemplate :: Text -> Text -> ([Int], [Int]) -> Text
viewTypeTemplate pointerT dataT (viewDimListTemplate -> dims) =
    [text|View<$pointerT, $dataT, $dims>|]

closureTemplate :: Int -> [(String,ExType)] -> Text
closureTemplate (lamIdToClosure -> clName) vars =
    let vars' = concat $ map (\(name,ty) -> declTemplate (cppExType ty) (pack name)) vars in
    [text|struct $clName {
        $vars'
    };|]

allocTemplate :: Storage -> Text
allocTemplate (Storage memId ty) = case ty of
    FPower _ shape -> [text|$viewT $memId;|] where
        viewT = viewTypeTemplate "double*" "double" (unzip $ shape)
    FDouble -> [text|double $memId;|]

funDefTemplate :: Text -> Text -> Text
funDefTemplate decl def = 
    [text|$decl {
        $def
    }|]

cpuEvaluatorTemplate :: [(Int,[(String,ExType)])] -> [Storage] -> [(Text,Text)] -> Type -> Text -> Text -> Text -> Text
cpuEvaluatorTemplate closures storage funs retT evaluatorName eval code =
    [text|
        #include "View.h"
        #include <map>
        #include <string>

        $closures'

        $allocs

        $funDecls

        $funDefs

        $retT' $evaluatorName(std::map<std::string, double*> userData) {
            $eval
            $return
        }
    |]
    where
        closures' = concat $ map (uncurry closureTemplate) closures
        allocs = concat $ map allocTemplate storage
        funDecls = concat $ map ((`append` ";\n") . strip . fst) funs
        funDefs = unlines $ map (strip . uncurry funDefTemplate) funs
        retT' = cppType retT
        return = case retT of
            FDouble -> [text|return $code;|]
            _       -> ""