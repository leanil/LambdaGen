{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}

module CppTemplateClosureConv where

import ClosureConversion
import Type
import Utility (tshow)
import Control.Arrow ((***))
import Data.Text (Text, append, concat, cons, init, intercalate, pack, snoc)
import qualified Data.Text as T (null)
import NeatInterpolation
import Prelude hiding (init, concat)

assignTemplate :: Text -> Text -> Text
assignTemplate name value = [text|$name = $value;|]

declTemplate :: Text -> Text -> Text
declTemplate ty name = [text|$ty $name;|]

initTemplate :: Text -> Text -> Text -> Text
initTemplate ty name val = [text|$ty $name = $val;|]

scalarOpTemplate :: Text -> Text -> Text -> Text
scalarOpTemplate op lhs rhs = [text|($lhs) $op ($rhs)|]

appTemplate :: Int -> Text -> [Text] -> Text
appTemplate (lamIdToName -> name) closure args =
    let args' = intercalate "," $ if T.null closure then args else closure : args in
    [text|$name($args')|]

makeClosureTemplate :: [(String, Bool)] -> Text
makeClosureTemplate [] = ""
makeClosureTemplate vars =
    let vars' = intercalate ", " $ map (\(name,bound) -> append (if bound then "" else "_cl.") $ pack name) vars in
    [text|{$vars'}|]

paramTemplate :: (String,Type) -> Text
paramTemplate (pack -> name,cppType -> ty) = [text|$ty $name|]

lambdaTemplate :: ExType -> Int -> Bool -> [(String,Type)] -> [(ExType,Text,Text)] -> Text -> (Text,Text)
lambdaTemplate (cppExType -> retT) lamId hasClosure params binds body =
    let name = lamIdToName lamId
        closure = if hasClosure then lamIdToClosure lamId `append` " _cl, " else ""
        params' = intercalate ", " $ map paramTemplate params
        binds' = concat $ map (\(ty,name,val) -> initTemplate (cppExType ty) name val) binds in
    ([text|$retT $name($closure$params')|],
    [text|
        $binds'
        return $body;
    |])

lamIdToName :: Int -> Text
lamIdToName = append "_lam" . tshow

lamIdToClosure :: Int -> Text
lamIdToClosure = append "_Cl" . tshow

cppType :: Type -> Text
cppType FDouble = "double"
cppType (FPower _ dims) = viewTypeTemplate "double*" "double" (unzip dims)

cppExType :: ExType -> Text
cppExType = either lamIdToClosure cppType

-- indexedVecs :: [Text] -> Text -> [Text]
-- indexedVecs vs (cons '[' . (flip snoc) ']' -> idx) = map (`append` idx) vs

-- rnzTemplate :: [Text] -> Text -> Text -> Text -> Text -> Text -> [Text] -> Text -> [Text] -> [Text] -> Text
-- rnzTemplate (concat -> evalVecs) resultId temp idx size reducer red_params zipper zip_params args =
--     let setZipParams = makeAssignments zip_params (indexedVecs args idx) 
--         setRedParams = makeAssignments red_params [resultId,temp] in
--     [text|
--         $evalVecs
--         for (int $idx = 0; $idx < $size; ++$idx) {
--             $setZipParams
--             $zipper
--             if($idx) {
--                 $setRedParams
--                 $reducer
--             }
--             else
--                 $resultId = $temp;
--         }
--     |]

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
    init [text|P<$size,$stride>|]

viewDimListTemplate :: ([Int], [Int]) -> Text
viewDimListTemplate (intercalate ", " . map viewDimElemTemplate . uncurry zip -> dims) =
    [text|to_list_t<$dims>|]

viewTypeTemplate :: Text -> Text -> ([Int], [Int]) -> Text
viewTypeTemplate pointerT dataT (viewDimListTemplate -> dims) =
    [text|View<$pointerT, $dataT, $dims>|]

viewTemplate :: Text -> Text -> ([Int], [Int]) -> Text -> Text -> Text
viewTemplate pointerT dataT dims viewName dataName =
    let viewT = viewTypeTemplate pointerT dataT dims in
    [text|$viewT $viewName$dataName;|]

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