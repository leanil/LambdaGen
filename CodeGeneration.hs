{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators, ViewPatterns #-}

module CodeGeneration where

import Expr
import Parallel
import Recursion
import Storage
import Type
import Typecheck
import TypePrinter
import Control.Arrow ((&&&))
import Control.Comonad.Cofree (Cofree(..))
import Data.Functor.Foldable (Fix(..))
import Data.List (intercalate)
import Data.Vinyl

newtype CodeGenT = CodeGenT [String] deriving Show
codeGenAlg :: (Result ∈ fields, ResultPack ∈ fields, ParData ∈ fields, TypecheckT ∈ fields) =>
    RAlgebra (Cofree ExprF (R fields)) CodeGenT

codeGenAlg (r ::< Scalar s) = CodeGenT [""]

codeGenAlg (r ::< VectorView id d s) = CodeGenT [""]

-- codeGenAlg (r ::< Vector e) = let (strs, bools) = unzip $ snd $ unzip e in
--     ("make_vector({" ++ intercalate "," strs ++ "})", or bools)

codeGenAlg (r ::< Addition (ra :< _,CodeGenT (a:as)) (rb :< _,CodeGenT (b:bs))) =
    CodeGenT $ (withNL a ++ withNL b ++ getName r ++ "=" ++ getName ra ++ "+" ++ getName rb) : bs ++ as

codeGenAlg (r ::< Multiplication (ra :< _,CodeGenT (a:as)) (rb :< _,CodeGenT (b:bs))) =
    CodeGenT $ (withNL a ++ withNL b ++ getName r ++ "=" ++ getName ra ++ "*" ++ getName rb) : bs ++ as

codeGenAlg (r ::< Apply (_,CodeGenT (a:as)) (rb :< _,CodeGenT (b:bs))) =
    CodeGenT $ (decorate r $ withNL b ++ a ++ "(" ++ getName rb ++ ")") : bs ++ as where
        decorate (getType -> Fix Arrow{}) s = s -- TODO: should it be parenthesized? '(' : s ++ ")"
        decorate _                        s = s ++ "(" ++ getName r ++ ")"

codeGenAlg (r@(getType -> (Fix (Arrow b _))) ::< Lambda id _ (_,CodeGenT (a:as))) =
     CodeGenT $ ("[=](" ++ mkView b ++ " " ++ id ++ "){return\n" ++ getBody a r ++ ";}") : as where
     getBody s (getType -> (Fix (Arrow _ (Fix Arrow{})))) = s
     getBody s (getType -> (Fix (Arrow _ c))) = "[=](" ++ mkView c ++ " result" ++ threadId r ++ "){\n" ++ s ++ ";}"
        where ds = countDims c
     threadId (snd . getParData -> Just _)  = ", unsigned thread_id"
     threadId (snd . getParData -> Nothing) = ""
     mkView (countDims -> ds) = fst $ viewType (ds,[]) 1 "accessor"

codeGenAlg (r ::< Variable id _) = CodeGenT [""]

codeGenAlg (r ::< Map (_,CodeGenT (a:as)) (rb :< _,CodeGenT (b:bs))) =
    mkPar r (withNL b) "Map" (a ++ "," ++ getName rb ++ "," ++ getName r) (bs ++ as)
        

codeGenAlg (r ::< Reduce (_,CodeGenT (a:as)) (rb :< _,CodeGenT (b:bs))) =
    mkPar r (withNL b) "Reduce" (a ++ "," ++ getName rb ++ "," ++ getName r ++ mkTemp r) (bs ++ as) where
        mkTemp (fieldVal ([] :: [Result]) -> Red (_,Prealloc x)) = ",v_" ++ show x
        mkTemp _                                          = ""

codeGenAlg (r ::< ZipWith (_,CodeGenT (a:as)) (rb :< _,CodeGenT (b:bs)) (rc :< _,CodeGenT (c:cs))) =
    mkPar r (withNL b ++ withNL c) "Zip" (a ++ "," ++ intercalate "," (map getName [rb, rc, r])) (cs ++ bs ++ as)

mkPar :: (ParData ∈ fields, ResultPack ∈ fields) => R fields -> String -> String -> String -> [String] -> CodeGenT
mkPar r@(snd . getParData -> Just t) vec hof body groups =
    CodeGenT $ "" : (if hof == "Reduce" then [mkCommandGroup r ("ParReduceJoin(" ++ body ++ "," ++ show t ++ ");")] else []) ++
               [(vec ++ mkCommandGroup r ("Par" ++ hof ++ "(" ++ body ++ "," ++ show t ++ ");"))] ++ groups
mkPar (snd . getParData -> Nothing) vec hof body groups =
    CodeGenT $ (vec ++ hof ++ "(" ++ body ++ ")") : groups

mkCommandGroup :: ResultPack ∈ fields => R fields -> String -> String
mkCommandGroup (fieldVal ([] :: [ResultPack]) -> ResultPack (stg,bigVec)) s =
    "deviceQueue.submit([&] (cl::sycl::handler &cgh) {\n" ++
    "\tact_cgh = &cgh;\n" ++
    concatMap
        (\(id, mem) -> let (view, strides) = viewType mem 1 "accessor" in
            "\t" ++ view ++ " v_" ++ id ++ "(" ++ strides ++ ",b_" ++ id ++ ".get_access<rw_access>(cgh));\n")
        bigVec ++
    concatMap
        (\(ResultStg id tn mem) -> let (view, strides) = viewType mem tn "accessor" in
            "\t" ++ view ++ " v_" ++ show id ++ "(" ++ strides ++ ",b_" ++ show id ++ ".get_access<rw_access>(cgh));\n")
        stg ++ 
    indent "\t" s ++ "});\n"

indent :: String -> String -> String
indent tabs code = unlines (map (tabs++) (lines code))

withNL [] = []
withNL s  = s ++ ",\n"

getName :: (Result ∈ fields, ParData ∈ fields) => R fields -> String
getName (getPrimary . fieldVal ([] :: [Result]) -> Inherit) = "result"
getName (getPrimary . fieldVal ([] :: [Result]) -> Implicit s) = s
getName (getPrimary . fieldVal ([] :: [Result]) &&& fst . getParData -> (Prealloc x, tn))
    | tn == 1 = "v_" ++ show x
    | tn > 1  = "v_" ++ show x ++ "[thread_id]"

getCode :: CodeGenT ∈ fields => R fields -> String
getCode (fieldVal ([] :: [CodeGenT]) -> CodeGenT groups) = concat $ reverse groups