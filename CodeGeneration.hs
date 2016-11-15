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
codeGenAlg :: (TypecheckT ∈ fields, Cost ∈ fields, ResultId ∈ fields) => Threshold -> Algebra (Cofree ExprF (R fields)) CodeGenT

codeGenAlg _ (_ ::< Scalar s) = (show s, False)

codeGenAlg _ (_ ::< VectorView id d s) = ("View<double," ++ sizes d s ++ ">(bigVectors.at(\"" ++ id ++ "\"))", False)

codeGenAlg _ (_ ::< Vector e) = let (strs, bools) = unzip e in
    ("make_vector({" ++ intercalate "," strs ++ "})", or bools)

codeGenAlg _ (_ ::< Addition (af,as) (bf,bs)) =
    ("(" ++ af ++ ")+(" ++ bf ++ ")", as || bs)

codeGenAlg _ (_ ::< Multiplication (af,as) (bf,bs)) =
    ("(" ++ af ++ ")*(" ++ bf ++ ")", as || bs)

codeGenAlg _ (r ::< Apply (af,as) (bf,bs)) =
    (af ++ "(" ++ bf ++ ")" ++ storage r, as || bs) where
        storage (getType -> Fix Arrow{})                 = ""
        storage (fieldVal ([] :: [ResultId]) -> Just x)  = "(s" ++ show x ++ ")"
        storage (fieldVal ([] :: [ResultId]) -> Nothing) = "(result)"

codeGenAlg _ (t ::< Lambda id _ (af,as)) =
    ("[&](const auto& " ++ id ++ "){return " ++ getBody af t ++ ";}", as) where
     getBody s (getType -> (Fix (Arrow _ (Fix Arrow{})))) = s
     getBody s (getType -> (Fix (Arrow _ (Fix Double)))) = "[&](auto&& result){result=" ++ s ++ ";}"
     getBody s _ = "[&](auto&& result){" ++ s ++ ";}"

codeGenAlg _ (_ ::< Variable id _) = (id, False)

codeGenAlg t (c ::< Map (af,as) (bf,bs)) =
    (par ++ "Map(" ++ af ++ "," ++ bf ++ "," ++ storage c ++ ")", b) where
        b = t > 0 && getCost c > t
        par = if b && not (as || bs) then "Par" else ""
        storage (fieldVal ([] :: [ResultId]) -> Just x) = "s" ++ show x
        storage (fieldVal ([] :: [ResultId]) -> Nothing) = "result"

codeGenAlg t (c ::< Reduce (af,as) (bf,bs)) =
    (par ++ "Reduce(" ++ af ++ "," ++ bf ++ "," ++ storage c ++ ")", b) where
        b = t > 0 && getCost c > t
        par = if b && not (as || bs) then "Par" else ""
        storage (fieldVal ([] :: [ResultId]) -> Just x) = "s" ++ show x
        storage (fieldVal ([] :: [ResultId]) -> Nothing) = "result"

codeGenAlg t (c ::< ZipWith (af,as) (bf,bs) (cf,cs)) =
    (par ++ "Zip(" ++ intercalate "," [af,bf,cf] ++ "," ++ storage c ++ ")", b) where
        b = t > 0 && getCost c > t
        par = if b && not (as || bs || cs) then "Par" else ""
        storage (fieldVal ([] :: [ResultId]) -> Just x) = "s" ++ show x
        storage (fieldVal ([] :: [ResultId]) -> Nothing) = "result"

getCode :: (CodeGenT ∈ fields, ResultPack ∈ fields) => R fields -> String
getCode r =
    preAlloc (fieldVal ([] :: [ResultPack]) r) ++
    fst (fieldVal ([] :: [CodeGenT]) r) ++  ";\n" ++ 
    let sel1 (a,_,_) = a in
    "return s" ++ show (sel1 $ head $ fieldVal ([] :: [ResultPack]) r) ++ ";\n"

preAlloc :: ResultPack -> String
preAlloc = concatMap (\(id, d, s) -> "View<double," ++ sizes d s++ "> s" ++ show id ++ ";\n")

sizes :: [Int] -> [Int] -> String
sizes d s = intercalate "," $ zipWith (\x y -> "Pair<" ++ show x ++ "," ++ show y ++ ">") d s