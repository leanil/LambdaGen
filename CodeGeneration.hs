{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}

module CodeGeneration where

import CostEstimation
import Expr
import Recursion
import Type
import Control.Comonad.Cofree
import Data.List(intercalate)
import Data.Vinyl
import Data.Vinyl.Functor

type Threshold = Int
type CodeGenT = (String, Bool)
codeGenAlg :: Cost ∈ fields => Threshold -> Algebra (Cofree ExprF (R fields)) CodeGenT

codeGenAlg _ (_ ::< Scalar s) = (show s, False)

codeGenAlg _ (_ ::< VectorView id d s) = ("View<double," ++ sizes ++ ">(bigVectors.at(\"" ++ id ++ "\"))", False) where
    sizes = intercalate "," $ zipWith (\x y -> "Pair<" ++ show x ++ "," ++ show y ++ ">") d s

codeGenAlg _ (_ ::< Vector e) = let (strs, bools) = unzip e in
    ("make_vector({" ++ intercalate "," strs ++ "})", or bools)

codeGenAlg _ (_ ::< Addition (af,as) (bf,bs)) =
    ("(" ++ af ++ ")+(" ++ bf ++ ")", as || bs)

codeGenAlg _ (_ ::< Multiplication (af,as) (bf,bs)) =
    ("(" ++ af ++ ")*(" ++ bf ++ ")", as || bs)

codeGenAlg _ (_ ::< Apply (af,as) (bf,bs)) =
    (af ++ "(" ++ bf ++ ")", as || bs)

codeGenAlg _ (_ ::< Lambda id _ (af,as)) =
    ("[&](const auto& " ++ id ++ "){return " ++ af ++ ";}", as)

codeGenAlg _ (_ ::< Variable id _) = (id, False)

codeGenAlg t (c ::< Map (af,as) (bf,bs)) =
    (par ++ "Map(" ++ af ++ "," ++ bf ++ ")", b) where
        b = t > 0 && getCost c > t
        par = if b && not (as || bs) then "Par" else ""

codeGenAlg t (c ::< Reduce (af,as) (bf,bs)) =
    (par ++ "Reduce(" ++ af ++ "," ++ bf ++ ")", b) where
        b = t > 0 && getCost c > t
        par = if b && not (as || bs) then "Par" else ""

codeGenAlg t (c ::< ZipWith (af,as) (bf,bs) (cf,cs)) =
    (par ++ "Zip(" ++ intercalate "," [af,bf,cf] ++ ")", b) where
        b = t > 0 && getCost c > t
        par = if b && not (as || bs || cs) then "Par" else "" 

getCode :: CodeGenT ∈ fields => R fields -> String
getCode = fst . fieldVal ([] :: [CodeGenT])