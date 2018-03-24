{-# LANGUAGE DataKinds, FlexibleContexts, TypeApplications, TypeOperators, ViewPatterns #-}

module Typecheck where

import Expr
import Recursion
import Type
import TypePrinter
import Control.Applicative
import Control.Comonad.Cofree (Cofree)
import Data.Either
import Data.Functor.Foldable
import Data.Maybe
import Data.Validation (Validation(..), toEither)
import Data.Vinyl

type Error = String
type TypecheckT = Validation [Error] Type

unwrap :: Validation [Error] TypecheckT -> TypecheckT
unwrap (Success s) = s
unwrap (Failure f) = Failure f

collect :: [TypecheckT] -> Validation [Error] [Type]
collect (partitionEithers . map toEither -> ([],t)) = Success t
collect (partitionEithers . map toEither -> (e,_))  = Failure $ concat e

typecheckAlg :: Algebra (Cofree ExprF (R fields)) TypecheckT

typecheckAlg (_ ::< Scalar _) = Success double

typecheckAlg (_ ::< Variable _ t) = Success t

typecheckAlg (_ ::< VectorView _ d _) = Success $ foldr (flip power . dim) double d

typecheckAlg (_ ::< Addition a b) = unwrap $ liftA2 addCheck a b where
    addCheck :: Type -> Type -> TypecheckT
    addCheck FDouble FDouble = Success double
    addCheck x y = Failure $ mapMaybe scalarCheck (filter (/= double) [x,y])
                      
typecheckAlg (_ ::< Multiplication a b) = unwrap $ mulCheck <$> a <*> b where
    mulCheck FDouble FDouble = Success double
    mulCheck x y = Failure $ mapMaybe scalarCheck (filter (/= double) [x,y])

typecheckAlg (_ ::< Apply a b) = unwrap $ applyCheck <$> a <*> collect b where
    applyCheck :: Type -> [Type] -> TypecheckT
    applyCheck (FArrow a b) c
        | length c > length a = Failure ["more arguments than lambda parameters in Apply"]
        | and (zipWith (==) a c) = Success $ 
            if length a == length c then b else arrow (drop (length c) a) b
        | otherwise = Failure $ catMaybes (zipWith eqCheck a c)
    applyCheck x _ = Failure $ [showT x ++ " not a lambda"]

typecheckAlg (_ ::< Lambda v b) = unwrap $ lamCheck <$> b where
    lamCheck x = Success $ arrow (map snd v) x
        
typecheckAlg (_ ::< Map a b) = unwrap $ mapCheck <$> a <*> b where
    mapCheck (FArrow [a] b) (FPower c d)
        | a == c = Success $ power b d
        | otherwise = Failure $ catMaybes [eqCheck a c]
    mapCheck a b = Failure $ catMaybes [lambdaCheck a, vectorCheck b]

typecheckAlg (_ ::< Reduce a b) = unwrap $ reduceCheck <$> a <*> b where
    reduceCheck (FArrow [a,b] c) v@(FPower d (FDim e))
        | a == b && a == c && a == d && e /= 0 = Success a
        | otherwise = Failure $ catMaybes [eqCheck a b, eqCheck a c, eqCheck a d] ++
            [showT v ++ " is an empty vector" | e == 0]
    reduceCheck a b = Failure $ catMaybes [biLambdaCheck a, vectorCheck b]            

typecheckAlg (_ ::< ZipWith a b c) = unwrap $ zipCheck <$> a <*> b <*> c where
    zipCheck (FArrow [a,b] c) (FPower d e) (FPower f g)
        | a == d && b == f && e == g = Success $ power c e
        | otherwise = Failure $ catMaybes [eqCheck a d, eqCheck b f, eqCheck e g]
    zipCheck a b c = Failure $ catMaybes [biLambdaCheck a, vectorCheck b, vectorCheck c]   

eqCheck :: Type -> Type -> Maybe Error
eqCheck a b
    | a == b = Nothing
    | otherwise = Just $ showT a ++ " != " ++ showT b

scalarCheck :: Type -> Maybe Error
scalarCheck (Fix Double) = Nothing
scalarCheck a = Just $ showT a ++ " not a scalar"

lambdaCheck :: Type -> Maybe Error
lambdaCheck (FArrow _ _) = Nothing
lambdaCheck a = Just $ showT a ++ " not a lambda"

biLambdaCheck :: Type -> Maybe Error
biLambdaCheck (FArrow [_,_] _) = Nothing
biLambdaCheck a = Just $ showT a ++ " not a binary lambda"

vectorCheck :: Type ->  Maybe Error
vectorCheck (FPower _ _) = Nothing
vectorCheck a = Just $ showT a ++ " not a vector"

getType :: TypecheckT ∈ fields => R fields -> Type
getType (fieldVal @TypecheckT -> Success t) = t
getType _ = error "getType on untyped expression"