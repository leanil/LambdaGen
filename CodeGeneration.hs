{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators, ViewPatterns #-}

module CodeGeneration where

import Expr
import Parallel
import Recursion
import Storage
import Type
import Typecheck
import Control.Arrow ((&&&))
import Control.Comonad.Cofree (Cofree(..))
import Data.Functor.Foldable (Fix(..))
import Data.List (intercalate)
import Data.Vinyl

type Threshold = Int
type CodeGenT = String
codeGenAlg :: (Result ∈ fields, ParData ∈ fields, TypecheckT ∈ fields) =>
    RAlgebra (Cofree ExprF (R fields)) CodeGenT

codeGenAlg (r ::< Scalar s) = ""

codeGenAlg (r ::< VectorView id d s) = "" --getName r ++ "=bigVectors.at(\"" ++ id ++ "\")"

-- codeGenAlg (r ::< Vector e) = let (strs, bools) = unzip $ snd $ unzip e in
--     ("make_vector({" ++ intercalate "," strs ++ "})", or bools)

codeGenAlg (r ::< Addition (ra :< _, a) (rb :< _, b)) =
    withNL a ++ withNL b ++
    getName r ++ "=" ++ getName ra ++ "+" ++ getName rb

codeGenAlg (r ::< Multiplication (ra :< _, a) (rb :< _,b)) =
    withNL a ++ withNL b ++
    getName r ++ "=" ++ getName ra ++ "*" ++ getName rb

codeGenAlg (r ::< Apply (_,a) (rb :< _,b)) = decorate r $ withNL b ++ a ++ "(" ++ getName rb ++ ")" where
    decorate (getType -> Fix Arrow{}) s = '(' : s ++ ")"
    decorate _                        s = s ++ "(" ++ getName r ++ ")"

codeGenAlg (r ::< Lambda id _ (_,a)) =
    "[&](const auto& " ++ id ++ "){return\n" ++ getBody a r ++ ";}" where
     getBody s (getType -> (Fix (Arrow _ (Fix Arrow{})))) = s
     getBody s r = "[&](auto& result" ++ threadId r ++ "){\n" ++ s ++ ";}"
     threadId (snd . getParData -> Just _)  = ", unsigned thread_id"
     threadId (snd . getParData -> Nothing) = ""

codeGenAlg (r ::< Variable id _) = ""

codeGenAlg (r ::< Map (_,a) (rb :< _,b)) =
    withNL b ++ mkPar r "Map" (a ++ "," ++ getName rb ++ "," ++ getName r)
        

codeGenAlg (r ::< Reduce (_,a) (rb :< _,b)) =
    withNL b ++ mkPar r "Reduce" (a ++ "," ++ getName rb ++ "," ++ getName r ++ mkTemp r) where
        mkTemp (fieldVal ([] :: [Result]) -> Red (_,Prealloc x)) = ",s" ++ show x
        mkTemp _                                          = ""

codeGenAlg (r ::< ZipWith (_,a) (rb :< _,b) (rc :< _,c)) =
    withNL b ++ withNL c ++ mkPar r "Zip" (a ++ "," ++ intercalate "," (map getName [rb, rc, r]))

mkPar :: ParData ∈ fields => R fields -> String -> String -> String
mkPar (snd . getParData -> Just t) hof body  = "Par" ++ hof ++ "(" ++ body ++ "," ++ show t ++ ")"
mkPar (snd . getParData -> Nothing) hof body = hof ++ "(" ++ body ++ ")"

withNL [] = []
withNL s  = s ++ ",\n"

getName :: (Result ∈ fields, ParData ∈ fields) => R fields -> String
getName (getPrimary . fieldVal ([] :: [Result]) -> Inherit) = "result"
getName (getPrimary . fieldVal ([] :: [Result]) -> Implicit s) = s
getName (getPrimary . fieldVal ([] :: [Result]) &&& fst . getParData -> (Prealloc x, tn))
    | tn == 1 = 's' : show x
    | tn > 1  = 's' : show x ++ "[thread_id]"

getCode :: (CodeGenT ∈ fields, Result ∈ fields, ResultPack ∈ fields, ParData ∈ fields) => R fields -> String
getCode r =
    preAlloc (fieldVal ([] :: [ResultPack]) r) ++
    fieldVal ([] :: [CodeGenT]) r ++ ";\n" ++
    "return " ++ getName r ++ ";\n"

preAlloc :: ResultPack -> String
preAlloc = concatMap (\(ResultStg id tn d s) -> "View<double" ++ sizes d s++ "> s" ++ show id ++
    (if tn > 1 then '[' : show tn ++ "]" else "") ++ ";\n")