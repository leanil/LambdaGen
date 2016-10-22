module CodeGeneration where

import CostEstimation(Cost)
import Expr
import Recursion
import Type
import Control.Comonad.Cofree
import Data.List(intercalate)

type Threshold = Int
type CodeGenT = (String, Bool)
codeGenAlg :: Threshold -> Algebra (Cofree ExprF (Type, Cost)) CodeGenT

codeGenAlg _ (MyPair (_, Scalar s)) = (show s, False)

codeGenAlg _ (MyPair (_, VectorView id d s)) = ("View<double," ++ sizes ++ ">(bigVectors.at(\"" ++ id ++ "\"))", False) where
    sizes = intercalate "," $ zipWith (\x y -> "Pair<" ++ show x ++ "," ++ show y ++ ">") d s

codeGenAlg _ (MyPair (_, Vector e)) = let (strs, bools) = unzip e in
    ("make_vector({" ++ intercalate "," strs ++ "})", or bools)

codeGenAlg _ (MyPair (_, Addition (af,as) (bf,bs))) =
    ("(" ++ af ++ ")+(" ++ bf ++ ")", as || bs)

codeGenAlg _ (MyPair (_, Multiplication (af,as) (bf,bs))) =
    ("(" ++ af ++ ")*(" ++ bf ++ ")", as || bs)

codeGenAlg _ (MyPair (_, Apply (af,as) (bf,bs))) =
    (af ++ "(" ++ bf ++ ")", as || bs)

codeGenAlg _ (MyPair (_, Lambda id _ (af,as))) =
    ("[&](const auto& " ++ id ++ "){return " ++ af ++ ";}", as)

codeGenAlg _ (MyPair (_, Variable id _)) = (id, False)

codeGenAlg t (MyPair ((_,c), Map (af,as) (bf,bs))) =
    (par ++ "Map(" ++ af ++ "," ++ bf ++ ")", b) where
        b = t > 0 && c > t
        par = if b && not (as || bs) then "Par" else ""

codeGenAlg t (MyPair ((_,c), Reduce (af,as) (bf,bs))) =
    (par ++ "Reduce(" ++ af ++ "," ++ bf ++ ")", b) where
        b = t > 0 && c > t
        par = if b && not (as || bs) then "Par" else ""

codeGenAlg t (MyPair ((_,c), ZipWith (af,as) (bf,bs) (cf,cs))) =
    (par ++ "Zip(" ++ intercalate "," [af,bf,cf] ++ ")", b) where
        b = t > 0 && c > t
        par = if b && not (as || bs || cs) then "Par" else "" 