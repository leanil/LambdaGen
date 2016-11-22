{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators, ViewPatterns #-}

module CodeGeneration where

import CostEstimation
import Expr
import Recursion
import Storage
import Type
import Typecheck
import Control.Comonad.Cofree
import Data.Functor.Foldable (Fix(..))
import Data.List(intercalate)
import Data.Vinyl
import Data.Vinyl.Functor

type Threshold = Int
type CodeGenT = (String, Bool)
codeGenAlg :: (TypecheckT ∈ fields, Cost ∈ fields, ResultStg ∈ fields) => Threshold -> RAlgebra (Cofree ExprF (R fields)) CodeGenT

codeGenAlg _ (r ::< Scalar s) = (getName r ++ "=" ++ show s, False)

codeGenAlg _ (r ::< VectorView id d s) = (getName r ++ "=bigVectors.at(\"" ++ id ++ "\")", False)

-- codeGenAlg _ (r ::< Vector e) = let (strs, bools) = unzip $ snd $ unzip e in
--     ("make_vector({" ++ intercalate "," strs ++ "})", or bools)

codeGenAlg _ (r ::< Addition (ra :< _, (af,as)) (rb :< _,(bf,bs))) =
    (af ++ ",\n" ++ bf ++ ",\n" ++
    getName r ++ "=" ++ getName ra ++ "+" ++ getName rb, as || bs)

codeGenAlg _ (r ::< Multiplication (ra :< _, (af,as)) (rb :< _,(bf,bs))) =
    (af ++ ",\n" ++ bf ++ ",\n" ++
    getName r ++ "=" ++ getName ra ++ "*" ++ getName rb, as || bs)

codeGenAlg _ (r ::< Apply (_,(af,as)) (rb :< _,(bf,bs))) = (decorate r $ bf ++ ",\n" ++ af ++ "(" ++ getName rb ++ ")", as || bs) where
    decorate (getType -> Fix Arrow{}) s = '(' : s ++ ")"
    decorate _                        s = s ++ "(" ++ getName r ++ ")"

codeGenAlg _ (r ::< Lambda id _ (_,(af,as))) =
    ("[&](const auto& " ++ id ++ "){return\n" ++ getBody af r ++ ";}", as) where
     getBody s (getType -> (Fix (Arrow _ (Fix Arrow{})))) = s
     --getBody s (getType -> (Fix (Arrow _ (Fix Double)))) = "[&](auto&& result){result=" ++ s ++ ",}"
     getBody s _ = "[&](auto&& result){\n" ++ s ++ ";}"

codeGenAlg _ (r ::< Variable id _) = (getName r ++ "=" ++ id, False)

codeGenAlg t (r ::< Map (_,(af,as)) (rb :< _,(bf,bs))) =
    (bf ++ ",\n" ++ par ++ "Map(" ++ af ++ "," ++ getName rb ++ "," ++ getName r ++ ")", b) where
        b = t > 0 && getCost r > t
        par = if b && not (as || bs) then "Par" else ""

codeGenAlg t (r ::< Reduce (_,(af,as)) (rb :< _,(bf,bs))) =
    (bf ++ ",\n" ++ par ++ "Reduce(" ++ af ++ "," ++ getName rb ++ "," ++ getName r ++ ")", b) where
        b = t > 0 && getCost r > t
        par = if b && not (as || bs) then "Par" else ""

codeGenAlg t (r ::< ZipWith (_,(af,as)) (rb :< _,(bf,bs)) (rc :< _,(cf,cs))) =
    (bf ++ ",\n" ++ cf ++ ",\n" ++ par ++ "Zip(" ++ af ++ "," ++ intercalate "," (map getName [rb, rc, r]) ++ ")", b) where
        b = t > 0 && getCost r > t
        par = if b && not (as || bs || cs) then "Par" else ""

-- getDecl :: (ResultStg ∈ fields) => R fields -> String
-- getDecl (fieldVal ([] :: [ResultStg]) -> Just (d,s,x)) = "View<double" ++ sizes d s ++ "> s" ++ show x
-- getDecl (fieldVal ([] :: [ResultStg]) -> Nothing) = ""

-- getFullDecl :: (ResultStg ∈ fields) => R fields -> String
-- getFullDecl (getDecl -> x) = if x == "" then "" else x ++ ",\n"

getName :: (ResultStg ∈ fields) => R fields -> String
getName (fieldVal ([] :: [ResultStg]) -> Just x) = 's' : show x
getName (fieldVal ([] :: [ResultStg]) -> Nothing) = "result"

getCode :: (CodeGenT ∈ fields, ResultStg ∈ fields, ResultPack ∈ fields) => R fields -> String
getCode r =
    preAlloc (fieldVal ([] :: [ResultPack]) r) ++
    fst (fieldVal ([] :: [CodeGenT]) r) ++ ";\n" ++
    "return " ++ getName r ++ ";\n"

preAlloc :: ResultPack -> String
preAlloc = concatMap (\(id, d, s) -> "View<double" ++ sizes d s++ "> s" ++ show id ++ ";\n")

sizes :: [Int] -> [Int] -> String
sizes [] [] = ""
sizes d s = "," ++ intercalate "," (zipWith (\x y -> "Pair<" ++ show x ++ "," ++ show y ++ ">") d s)