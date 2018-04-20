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

appTemplate :: [Text] -> Text -> [Text] -> Text
appTemplate (concat -> evals) lambda (intercalate ", " -> names) = 
    [text|
        $evals
        $lambda($names);
    |]

-- TODO: let-bound expressions should allocate with a given name
lambdaTemplate :: [Text] -> [Text] -> [Text] -> [Text] -> Text -> Text
lambdaTemplate (intercalate ", " -> params) (concat -> evals) (map (append "auto ") -> names) values body =
    let assigns = concat $ zipWith assignTemplate names values in
    [text|
        [=]($params){
            $evals
            $assigns
            $body
        }
    |]

indexVecs :: [Text] -> Text -> Text
indexVecs vs (cons '[' . (flip snoc) ']' -> idx) = intercalate "," $ map (`append` idx) vs

rnzTemplate :: [Text] -> Text -> Text -> Text -> Text -> Text -> Text -> [Text] -> Text
rnzTemplate (concat -> evalVecs) resultId temp idx size reducer zipper vecNames =
    let indexedVecs = indexVecs vecNames idx in
    [text|
        $evalVecs
        for (int $idx = 0; $idx < $size; ++$idx) {
            $zipper($indexedVecs);
            if($idx)
                $reducer($resultId,$temp);
            else
                $resultId = $temp;
        }
    |]

zipWithNTemplate :: [Text] -> Text -> Text -> Text -> [Text] -> Text
zipWithNTemplate (concat -> evalVecs) idx size lambda vecNames =
    let indexedVecs = indexVecs vecNames idx in
    [text|
        $evalVecs
        for (int $idx = 0; $idx < $size; ++$idx) {
            $lambda($indexedVecs);
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