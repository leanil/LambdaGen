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

assignTemplate :: Text -> Text -> Text
assignTemplate name value = [text|$name = $value;|]

declTemplate :: Text -> Text -> Text
declTemplate ty name = [text|$ty $name;|]

initTemplate :: Text -> Text -> Text -> Text
initTemplate ty name val = [text|$ty $name = $val;|]

varTemplate :: String -> Bool -> Text
varTemplate (pack -> name) free = stripEnd $ if free then [text|$closureParamName.$name|] else [text|$name|]

scalarOpTemplate :: Text -> Text -> Text -> Text
scalarOpTemplate op lhs rhs = [text|($lhs) $op ($rhs)|]

appTemplate :: Int -> Bool -> Maybe Text -> [Text] -> Maybe Text -> Text
appTemplate lamId wrapped closure args outParam = stripEnd [text|$name($args')$seCol|] where 
    name = lamIdToName lamId `append` if wrapped then wrapperSuffix else ""
    args' = intercalate ", " $ maybeToList closure ++ args ++ maybeToList outParam
    seCol = if isJust outParam then ";" else ""

-- | The Bool value tells if the variable is from the parameter set of the enclosing lambda (or inherited from it's closure).
makeClosureTemplate :: [(String, Bool)] -> Text
makeClosureTemplate [] = ""
makeClosureTemplate vars = [text|{$vars'}|] where
    vars' = intercalate ", " $ map (uncurry varTemplate) vars
    

paramTemplate :: (Text,Text) -> Text
paramTemplate (name,ty) = stripEnd [text|$ty $name|]

-- | A lambda function's result can be a scalar value, a tensor or another lambda.
--   Tensors should be returned through an 'out' parameter to avoid redundant memory allocation.
--   On the other hand, Lambdas (closures) should be returned regularly, to make nested calls easier.
--   Scalar valued lambdas can be used in both kinds of contexts.

lambdaTemplate :: ExType -> Int -> Bool -> [(String,Type)] -> [Text] -> Text -> Bool -> [(Text,Text)]
lambdaTemplate retT lamId hasClosure (map (mapFst pack) -> params) (concat -> binds) body bodyOwnsStorage =
    [([text|
        $templateParams
        $retT' $name($params'')
    |],[text|
        $binds
        $return
    |])] ++ wrapper where
        templateParams = templateParamsTemplate templateCnt
        (retT',outParam,return) = case retT of
            Right FDouble -> ("void", [(outParamName,power double [])],
                if bodyOwnsStorage then [text|$outParamName = $body;|] else body)
            Right ty@FPower{} -> ("void", [(outParamName,ty)],body)
            Left{}   -> (cppExType retT, [], [text|return $body;|])
        name = lamIdToName lamId
        params'' = intercalate ", " $ map paramTemplate $ closure ++ params'
        wrapper = case retT of
            Right FDouble -> [wrapperFunctionTemplate (templateCnt-1) name (closure ++ init params'::[(Text,Text)]) ]
            otherwise     -> []
        (params',templateCnt) = foldr hideParams ([],0) $ params ++ outParam where
            hideParams (name,FDouble) (ps,cnt) = ((name,"double"):ps, cnt)
            hideParams (name,FPower{}) (ps,cnt) = ((name,templateArgName $ cnt+1):ps, cnt+1)
        closure = if hasClosure then [(closureParamName,lamIdToClosure lamId)] else []
        

templateParamsTemplate :: Int -> Text
templateParamsTemplate 0   = ""
templateParamsTemplate cnt = [text|template<$params>|] where
    params = intercalate ", " $ map (append "typename " . templateArgName) [1..cnt]

letBindingTemplate :: ExType -> Text -> Text -> Text
letBindingTemplate (Right FPower{}) _ code = code
letBindingTemplate ty name code = initTemplate (cppExType ty) name code

wrapperFunctionTemplate :: Int -> Text -> [(Text,Text)] -> (Text,Text)
wrapperFunctionTemplate templateCnt funName params =
    ([text|
        $templateParams
        double $funName'($params')
    |],[text|
        double result;
        $funName($args, View<double*, double, to_list_t<>, true>(&result));
        return result;
    |]) where
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

viewTemplate :: Text -> Text -> Text -> [Int] -> [Int] -> Text -> Text
viewTemplate varName pointerT dataT dims strides dataName =
    [text|$viewT $varName(userData[$dataName]);|] where
        viewT = viewTypeTemplate pointerT dataT (dims,strides)

closureTemplate :: Int -> [(String,ExType)] -> Text
closureTemplate _ [] = ""
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

cpuEvaluatorTemplate :: [(Int,[(String,ExType)])] -> [(Text,Text)] -> Type -> Text -> Text -> Text
cpuEvaluatorTemplate closures funs (cppType -> retT) evaluatorName code =
    let closures' = concat $ map (uncurry closureTemplate) closures
        funDefs = concat $ map (uncurry funDefTemplate) funs in
    [text|
        #include "View.h"
        #include <map>
        #include <string>

        $closures'

        $funDefs

        $retT $evaluatorName(std::map<std::string, double*> userData) {
            return $code;
        }
    |]