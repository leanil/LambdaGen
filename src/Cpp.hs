{-# LANGUAGE FlexibleContexts, TypeOperators #-}

module Cpp where

import CodeGeneration
import Parallel
import Recursion
import Storage
import Data.Vinyl

-- createEvaluator :: (CodeGenT ∈ fields, Result ∈ fields, ResultPack ∈ fields, ParData ∈ fields) => String -> R fields -> String
-- createEvaluator evaluatorName expr =
--     concatMap (\h -> "#include " ++ h ++ "\n") headers ++
--     "\n" ++ signature ++ "{\n" ++
--     "\t" ++ fst returnType ++ " result(" ++ snd returnType ++ ", new double[" ++ sMemSize (getMemStruct resPack) ++ "]);\n" ++
--     indent "\t" (makeBuffers resPack) ++
--     "\tcl::sycl::queue deviceQueue;\n" ++ -- ((cl::sycl::gpu_selector()))
--     indent "\t" (getCode expr) ++
--     "\treturn result;\n}\n"
--     where
--         headers = ["\"helper.h\"", "\"my_sycl.h\"", "\"View.h\"", "<map>", "<string>"]
--         resPack = fieldVal expr
--         returnType = getReturnType resPack
--         signature = fst returnType ++ " " ++ evaluatorName ++ "(std::map<std::string, double*> bigVectors)"
--         getMemStruct (ResultPack (ResultStg _ _ mem : _,_)) = mem

-- getReturnType :: ResultPack -> (String,String)
-- getReturnType (ResultPack (ResultStg _ tn mem : _,_)) = viewType mem tn "double*"

-- makeBuffers :: ResultPack -> String
-- makeBuffers (ResultPack (ResultStg i _ mem : stg,bigVec)) =
--     concatMap
--         (\(BigVector n d m) -> "buffer_t b_" ++ n ++ "(const_cast<const double*>(bigVectors.at(\"" ++ d ++ "\"))," ++ sMemSize m ++ ");\n")
--         bigVec ++
--     "buffer_t b_" ++ show i ++ "(result.data, " ++ sMemSize mem ++ ");\n" ++
--     concatMap
--         (\(ResultStg n tn m) -> "buffer_t b_" ++ show n ++ "(" ++ show (tn * memSize m) ++ ");\n")
--         stg

-- memSize :: MemStruct -> Int
-- memSize ([], [])     = 1
-- memSize (x:xs, y:ys) = (x-1)*y + memSize (xs,ys)

-- sMemSize :: MemStruct -> String
-- sMemSize = show . memSize