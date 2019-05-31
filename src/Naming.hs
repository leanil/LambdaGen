{-# LANGUAGE OverloadedStrings #-}

module Naming where 
    
import Utility (charShift, tshow)
import Data.Text (Text, append, pack, singleton)

closureParamName :: Text
closureParamName = "_cl"

resultTensor :: Int -> String
resultTensor = ("_t"++) . show 

tResultTensor :: Int -> Text
tResultTensor = pack . resultTensor

outParamName :: Text
outParamName = "_result"

lamIdToName :: Int -> Text
lamIdToName = append "_lam" . tshow

lamIdToClosure :: Int -> Text
lamIdToClosure = append "_Cl" . tshow

templateArgName :: Int -> Text
templateArgName = append "_T" . tshow

rnzIdToName :: Int -> Text
rnzIdToName = append "_rnz" . tshow

zipIdToName :: Int -> Text
zipIdToName = append "_zip" . tshow

wrapperSuffix :: Text
wrapperSuffix = "w"

tmpSuffix :: Text -- For the temporary storage of reduction
tmpSuffix = "_tmp"

tensorName :: Int -> Text
tensorName = singleton . charShift 'A'

indexName :: Int -> Text
indexName = singleton . charShift 'i'