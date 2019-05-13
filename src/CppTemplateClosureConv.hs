{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}

module CppTemplateClosureConv where

import ClosureConversion
import Naming
import Type
import Utility (mapFst, tshow)
import Control.Arrow ((***))
import Data.Maybe (isJust, maybeToList)
import Data.Text (Text, append, concat, cons, intercalate, pack, snoc, stripEnd, unpack)
import qualified Data.Text as T (null)
import NeatInterpolation
import Prelude hiding (concat)

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
    LetBound name -> CodeT name [text|$viewT $name(userData[$dataName]);|]
    None -> CodeT code ""
    where
        viewT = viewTypeTemplate pointerT dataT (dims,strides)
        code = stripEnd [text|$viewT(userData[$dataName])|]

varTemplate :: String -> Bool -> Text
varTemplate (pack -> name) free = stripEnd $ if free then [text|$closureParamName.$name|] else [text|$name|]

variableTemplate :: Maybe String -> StoreResult -> String -> Bool -> CodeT
variableTemplate letId result name free = case letId of
    Just (pack -> letId) -> CodeT letId (initTemplate "auto" letId code) -- TODO: store template type of function parameter tensors and use it here. Or clear strides from view type.
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
        code = stripEnd [text|($lhs) $op ($rhs)|]

appTemplate :: Text -> StoreResult -> Int -> Text -> [Text] -> CodeT
appTemplate eval result lamId closure args = case outParam of
    Just name -> CodeT name (append eval [text|$code;|])
    _ -> case result of
        None -> CodeT code eval
        LetBound name -> CodeT name (append eval $ initTemplate "double" name code)
    where
        outParam = case result of
            OutParam -> Just outParamName
            Tensor name -> Just name
            _ -> Nothing
        code = stripEnd $ [text|$funName($args')|]
        funName = lamIdToName lamId `append` if isJust outParam then "" else wrapperSuffix
        args' = intercalate ", " $ closure : args ++ maybeToList outParam
        
-- | The Bool value tells if the variable is from the parameter set of the enclosing lambda (or inherited from it's closure).
makeClosureTemplate :: StoreResult -> Int -> [(String, Bool)] -> CodeT
makeClosureTemplate result (lamIdToClosure -> ty) vars = case result of
    LetBound name -> CodeT name $ initTemplate ty name code
    None          -> CodeT code ""
    where
        code = stripEnd [text|{$vars'}|]
        vars' = intercalate ", " $ map (uncurry varTemplate) vars
    
paramTemplate :: (Text,Text) -> Text
paramTemplate (name,ty) = stripEnd [text|$ty $name|]

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
        (params',templateCnt) = foldr hideParams ([],0) $ params ++ outParam where
            hideParams (name,FDouble) (ps,cnt) = ((name,"double"):ps, cnt)
            hideParams (name,FPower{}) (ps,cnt) = ((name,templateArgName $ cnt+1):ps, cnt+1)
        closure = [(closureParamName,lamIdToClosure lamId)]

templateParamsTemplate :: Int -> Text
templateParamsTemplate 0   = ""
templateParamsTemplate cnt = [text|template<$params>|] where
    params = intercalate ", " $ map (append "typename " . templateArgName) [1..cnt]

wrapperFunctionTemplate :: Int -> Text -> [(Text,Text)] -> (Text,Text)
wrapperFunctionTemplate templateCnt funName params =
    ([text|
        $templateParams
        double $funName'($params')|],
    [text|
        double result;
        $funName($args, View<double*, double, to_list_t<>, true>(&result));
        return result;|])
    where
        templateParams = templateParamsTemplate templateCnt
        funName' = append funName wrapperSuffix
        params' = intercalate ", " $ map paramTemplate params
        args = intercalate ", " $ map fst params

cppType :: Type -> Text
cppType FDouble = "double"
cppType (FPower _ dims) = viewTypeTemplate "double*" "double" (unzip dims)

cppExType :: ExType -> Text
cppExType = either lamIdToClosure cppType

-- indexedVecs :: [Text] -> Text -> [Text]
-- indexedVecs vs (cons '[' . (flip snoc) ']' -> idx) = map (`append` idx) vs

rnzTemplate :: Int -> Text -> Text -> [Text] -> Maybe Text -> Text
rnzTemplate hofId reducer zipper vecNames outParam =
    [text|$hofName($reducer, $zipper, $params)$seCol|] where
        hofName = rnzIdToName hofId `append` if isJust outParam then "" else wrapperSuffix
        params = intercalate ", " $ vecNames ++ maybeToList outParam
        seCol = if isJust outParam then ";" else ""

    -- [text|
    --     $evalVecs
    --     for (int $idx = 0; $idx < $size; ++$idx) {
    --         $setZipParams
    --         $zipper
    --         if($idx) {
    --             $setRedParams
    --             $reducer
    --         }
    --         else
    --             $resultId = $temp;
    --     }
    -- |]

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
    stripEnd [text|P<$size,$stride>|]

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

funDefTemplate :: Text -> Text -> Text
funDefTemplate decl def = 
    [text|$decl {
        $def
    }|]

cpuEvaluatorTemplate :: [(Int,[(String,ExType)])] -> [(Text,Text)] -> Type -> Text -> Text -> Text -> Text
cpuEvaluatorTemplate closures funs retT evaluatorName eval code =
    [text|
        #include "View.h"
        #include <map>
        #include <string>

        $closures'

        $funDefs

        $retT' $evaluatorName(std::map<std::string, double*> userData) {
            $eval
            $return
        }
    |]
    where
        closures' = concat $ map (uncurry closureTemplate) closures
        funDefs = concat $ map (uncurry funDefTemplate) funs
        retT' = cppType retT
        return = case retT of
            FDouble -> [text|return $code;|]
            _       -> ""