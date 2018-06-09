{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}

module CppTemplate where

import Data.Text (Text, append, init, concat, cons, intercalate, pack, snoc)
import NeatInterpolation
import Prelude hiding (init, concat)

assignTemplate :: Text -> Text -> Text
assignTemplate name value = 
    [text|$name = $value;|]

scalarOpTemplate :: Text -> Text -> Text -> Text -> Text -> Text -> Text
scalarOpTemplate lhsCode rhsCode result lhsResult op rhsResult = 
    [text|
        $lhsCode
        $rhsCode
        $result = $lhsResult $op $rhsResult;
    |]

appTemplate :: [Text] -> Text -> [Text] -> [Text] -> Text
appTemplate (concat -> evals) lambda params args = 
    let setParams = makeAssignments params args in
    [text|
        $evals
        $setParams
        $lambda
    |]

makeAssignments :: [Text] -> [Text] -> Text
makeAssignments names values = concat $ zipWith assignTemplate (map (append "auto ") names) values

-- TODO: let-bound expressions should allocate with a given name
-- The caller sets the parameter values.
lambdaTemplate :: [Text] -> [Text] -> [Text] -> Text -> Text
lambdaTemplate (concat -> evals) names values body =
    let assigns = makeAssignments names values in
    [text|
        $evals
        $assigns
        $body
    |]

indexedVecs :: [Text] -> Text -> [Text]
indexedVecs vs (cons '[' . (flip snoc) ']' -> idx) = map (`append` idx) vs

rnzTemplate :: [Text] -> Text -> Text -> Text -> Text -> Text -> [Text] -> Text -> [Text] -> [Text] -> Text
rnzTemplate (concat -> evalVecs) resultId temp idx size reducer red_params zipper zip_params args =
    let setZipParams = makeAssignments zip_params (indexedVecs args idx) 
        setRedParams = makeAssignments red_params [resultId,temp] in
    [text|
        $evalVecs
        for (int $idx = 0; $idx < $size; ++$idx) {
            $setZipParams
            $zipper
            if($idx) {
                $setRedParams
                $reducer
            }
            else
                $resultId = $temp;
        }
    |]

zipWithNTemplate :: [Text] -> Text -> Text -> Text -> [Text] -> [Text] -> Text
zipWithNTemplate (concat -> evalVecs) idx size lambda params args =
    let setParams =  makeAssignments params (indexedVecs args idx) in
    [text|
        $evalVecs
        for (int $idx = 0; $idx < $size; ++$idx) {
            $setParams
            $lambda
        }
    |]

flipTemplate :: Text -> Text -> Text -> Text -> Text -> Text -> Text
flipTemplate rhsCode auto result idx1 idx2 rhsResult =
    [text|
        $rhsCode
        $auto$result = flip<$idx1>($rhsResult);
    |]

subdivTemplate :: Text -> Text -> Text -> Text -> Text -> Text -> Text
subdivTemplate rhsCode auto result idx block rhsResult =
    [text|
        $rhsCode
        $auto$result = subdiv<$idx,$block>($rhsResult);
    |]

viewDimElemTemplate :: (Int, Int) -> Text
viewDimElemTemplate (1,1) = ""
viewDimElemTemplate (pack . show -> size, pack . show -> stride) =
    init [text|P<$size,$stride>|]

viewDimListTemplate :: ([Int], [Int]) -> Text
viewDimListTemplate (intercalate ", " . map viewDimElemTemplate . uncurry zip -> dims) =
    [text|to_list_t<$dims>|]

viewTemplate :: Text -> Text -> ([Int], [Int]) -> Text -> Text -> Text
viewTemplate pointerT dataT (viewDimListTemplate -> dims) viewName dataName =
    [text|View<$pointerT, $dataT, $dims> $viewName$dataName;|]

cpuEvaluatorTemplate :: Text -> [Text] -> [Text] -> Text -> Text -> Text
cpuEvaluatorTemplate evaluatorName (concat -> userData) (concat -> allocViews) evalCode resultName =
    [text|
        #include "View.h"
        #include <map>
        #include <string>

        auto $evaluatorName(std::map<std::string, double*> userData) {
            $userData
            $allocViews
            $evalCode
            return $resultName;
        }
    |]