{-# LANGUAGE DataKinds, FlexibleContexts, TypeApplications, TypeOperators, ViewPatterns #-}

module Typecheck where

import Expr
import Recursion
import Type
import TypePrinter
import Control.Comonad.Cofree (Cofree)
import Data.Either
import Data.Maybe
import Data.Vinyl

type Error = String
type TypecheckT = Either Type [Error]

allLeft :: [TypecheckT] -> (Bool, [Type])
allLeft x = let r = lefts x in (length r == length x, r)

typecheckAlg :: Algebra (Cofree ExprF (R fields)) TypecheckT

typecheckAlg (_ ::< Scalar _) = Left double

typecheckAlg (_ ::< Variable _ t) = Left t

typecheckAlg (_ ::< VectorView _ d _) = Left $ foldr (flip power . dim) double d

typecheckAlg (_ ::< Addition (Left FDouble) (Left FDouble)) = Left double
typecheckAlg (_ ::< Addition (Left a) (Left b)) = Right $ mapMaybe scalarCheck [a,b]
                      
typecheckAlg (_ ::< Multiplication (Left FDouble) (Left FDouble)) = Left double
typecheckAlg (_ ::< Multiplication (Left a) (Left b)) = Right $ mapMaybe scalarCheck [a,b] 

typecheckAlg (_ ::< Apply (Left (FArrow a b)) (allLeft -> (True, c)))
    | length c > length a = Right ["more arguments than lambda parameters in Apply"]
    | and (zipWith (==) a c) = Left $ if length a == length c then b else arrow (drop (length c) a) b
    | otherwise = Right $ catMaybes (zipWith eqCheck a c)       
typecheckAlg (_ ::< Apply (Left a) _) = Right $ [showT a ++ " not a lambda"]

-- typecheckAlg (_ ::< Let _ v e) = unwrap $ letCheck <$> v <$> e where
--     letCheck _ y = Left $ y

typecheckAlg (_ ::< Lambda v (Left b)) = Left $ arrow (map snd v) b
        
typecheckAlg (_ ::< Map (Left (FArrow [a] b)) (Left (FPower c d)))
    | a == c = Left $ power b d
    | otherwise = Right $ catMaybes [eqCheck a c]
typecheckAlg (_ ::< Map (Left a) (Left b)) = Right $ catMaybes [lambdaCheck a, vectorCheck b]

typecheckAlg (_ ::< Reduce (Left (FArrow [a,b] c)) (Left v@(FPower d (FDim e))))
    | a == b && a == c && a == d && e /= 0 = Left a
    | otherwise = Right $ catMaybes [eqCheck a b, eqCheck a c, eqCheck a d] ++ [showT v ++ " is an empty vector" | e == 0]
typecheckAlg (_ ::< Reduce (Left a) (Left b)) = Right $ catMaybes [biLambdaCheck a, vectorCheck b]            

typecheckAlg (_ ::< ZipWith (Left (FArrow [a,b] c)) (Left (FPower d e)) (Left (FPower f g)))
    | a == d && b == f && e == g = Left $ power c e
    | otherwise = Right $ catMaybes [eqCheck a d, eqCheck b f, eqCheck e g]
typecheckAlg (_ ::< ZipWith (Left a) (Left b) (Left c)) = Right $ catMaybes [biLambdaCheck a, vectorCheck b, vectorCheck c]   

typecheckAlg (_ ::< node) = Right $ foldMap (either (const []) id) node

eqCheck :: Type -> Type -> Maybe Error
eqCheck a b
    | a == b = Nothing
    | otherwise = Just $ showT a ++ " != " ++ showT b

scalarCheck :: Type -> Maybe Error
scalarCheck (FDouble) = Nothing
scalarCheck a = Just $ showT a ++ " not a scalar"

lambdaCheck :: Type -> Maybe Error
lambdaCheck (FArrow _ _) = Nothing
lambdaCheck a = Just $ showT a ++ " not an unary lambda"

biLambdaCheck :: Type -> Maybe Error
biLambdaCheck (FArrow [_,_] _) = Nothing
biLambdaCheck a = Just $ showT a ++ " not a binary lambda"

vectorCheck :: Type ->  Maybe Error
vectorCheck (FPower _ _) = Nothing
vectorCheck a = Just $ showT a ++ " not a vector"

getType :: TypecheckT ∈ fields => R fields -> Type
getType (fieldVal @TypecheckT -> Left t) = t
getType _ = error "getType on untyped expression"