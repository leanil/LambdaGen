{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}

module CppTemplate where

import Data.Text (Text, init, concat, intercalate, pack)
import NeatInterpolation
import Prelude hiding (init, concat)

assignTemplate :: Text -> Text -> Text
assignTemplate name value = 
    [text|$name = $value;|]

addTemplate :: Text -> Text -> Text -> Text -> Text -> Text
addTemplate lhsCode rhsCode result lhsResult rhsResult = 
    [text|
        $lhsCode
        $rhsCode
        $result = $lhsResult + $rhsResult;
    |]

mulTemplate :: Text -> Text -> Text -> Text -> Text -> Text
mulTemplate lhsCode rhsCode result lhsResult rhsResult = 
    [text|
        $lhsCode
        $rhsCode
        $result = $lhsResult * $rhsResult;
    |]

appTemplate :: [Text] -> Text -> [Text] -> Text
appTemplate (concat -> evals) lambda (intercalate ", " -> names) = 
    [text|
        ${evals}
        ${lambda}($names);
    |]

letTemplate :: Text -> Text -> Text -> Text -> Text
letTemplate eval newName valName expr =
    [text|
        ${eval}
        auto $newName = $valName;
        $expr
    |]

lambdaTemplate :: [Text] -> Text -> Text
lambdaTemplate (intercalate ", " -> params) body =
    [text|
        [=]($params){
            $body
        }
    |]

mapTemplate :: Text -> Text -> Text -> Text -> Text -> Text
mapTemplate evalVec idx size lambda vecName =
    [text|
        $evalVec
        for (int $idx = 0; $idx < $size; ++$idx) {
            $lambda($vecName[$idx]);
        }
    |]

reduceTemplate :: Text -> Text -> Text -> Text -> Text -> Text -> Text
reduceTemplate evalVec resultId idx size lambda vecName =
    [text|
        $evalVec
        $resultId = $vecName[0];
        for (int $idx = 1; $idx < $size; ++$idx) {
            $lambda($resultId,$vecName[$idx]);
        }
    |]

zipWithTemplate :: Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text
zipWithTemplate evalVec1 evalVec2 idx size lambda vecName1 vecName2 =
    [text|
        $evalVec1
        $evalVec2
        for (int $idx = 0; $idx < $size; ++$idx) {
            $lambda($vecName1[$idx],$vecName2[$idx]);
        }
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