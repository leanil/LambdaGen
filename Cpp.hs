{-# LANGUAGE FlexibleContexts, TypeOperators #-}

module Cpp where

import CodeGeneration
import Parallel
import Recursion
import Storage
import Data.Vinyl
import Data.List (intercalate)

createEvaluator :: (CodeGenT ∈ fields, Result ∈ fields, ResultPack ∈ fields, ParData ∈ fields) => R fields -> String
createEvaluator expr =
    concatMap (\h -> "#include " ++ h ++ "\n") headers ++
    "\n" ++ signature ++ "{\n" ++
    "\t" ++ fst returnType ++ " result(" ++ snd returnType ++ ", new double[" ++ sMemSize (getMemStruct resPack) ++ "]);\n" ++
    indent "\t" (makeBuffers resPack) ++
    "\tcl::sycl::queue deviceQueue((cl::sycl::gpu_selector()));\n" ++
    indent "\t" (getCode expr) ++
    "\treturn result;\n}\n"
    where
        headers = ["\"helper.h\"", "\"my_sycl.h\"", "\"View.h\"", "<map>", "<string>"]
        resPack = fieldVal ([] :: [ResultPack]) expr
        returnType = getReturnType resPack
        signature = fst returnType ++ " evaluator(std::map<std::string, double*> bigVectors)"
        getMemStruct (ResultPack (ResultStg _ _ mem : _,_)) = mem

getReturnType :: ResultPack -> (String,String)
getReturnType (ResultPack (ResultStg _ tn mem : _,_)) = viewType mem tn "double*"

makeBuffers :: ResultPack -> String
makeBuffers (ResultPack (ResultStg id _ mem : stg,bigVec)) =
    concatMap
        (\(id, mem) -> "buffer_t b_" ++ id ++ "(const_cast<const double*>(bigVectors.at(\"" ++ id ++ "\"))," ++ sMemSize mem ++ ");\n")
        bigVec ++
    "buffer_t b_" ++ show id ++ "(result.data, " ++ sMemSize mem ++ ");\n" ++
    concatMap
        (\(ResultStg id tn mem) -> "buffer_t b_" ++ show id ++ "(" ++ show (tn * memSize mem) ++ ");\n")
        stg

memSize :: MemStruct -> Int
memSize ([], [])     = 1
memSize (x:xs, y:ys) = (x-1)*y + memSize (xs,ys)
sMemSize = show . memSize