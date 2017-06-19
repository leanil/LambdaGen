{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}

module Typecheck where

import Expr
import Recursion
import Type
import TypePrinter
import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Either
import Data.Either.Combinators (fromLeft')
import Data.Functor.Foldable
import Data.Maybe
import Data.Vinyl
import Data.Vinyl.Functor

type Error = String
type TypecheckT = Either Type [Error]

typecheckAlg :: Algebra (Cofree ExprF (R fields)) TypecheckT

typecheckAlg (_ ::< Scalar val) = Left double

typecheckAlg (_ ::< Variable id t) = Left t

typecheckAlg (_ ::< VectorView id d _) = Left $ foldr (flip power . dim) double d

typecheckAlg (_ ::< Vector elements) =
    case partitionEithers elements of
        (t, []) -> diffCheck t
        (_, errors) -> Right $ concat errors
        where
            diffCheck types = case filter (/= head types) (tail types) of
                []   -> Left $ power (head types) (dim $ length types)
                diff -> Right $ mapMaybe (eqCheck (head types)) diff    

typecheckAlg (_ ::< Addition a b) =
    case partitionEithers [a, b] of
        ([ea, eb], []) -> addCheck ea eb
        (_, errors) -> Right $ concat errors
        where
            addCheck (Fix Double) (Fix Double) = Left double
            addCheck a b = Right $ mapMaybe scalarCheck (filter (/= double) [a, b])
                      
typecheckAlg (_ ::< Multiplication a b) =
    case partitionEithers [a, b] of
        ([ea, eb], []) -> mulCheck ea eb
        (_, errors) -> Right $ concat errors
        where
            mulCheck (Fix Double) (Fix Double) = Left double
            mulCheck a b = Right $ mapMaybe scalarCheck (filter (/= double) [a, b])

typecheckAlg (_ ::< Apply a b) =
    case partitionEithers [a, b] of
        ([ea, eb], []) -> applyCheck ea eb
        (_, errors) -> Right $ concat errors
        where
            applyCheck (Fix (Arrow a b)) c
                | a == c = Left b
                | otherwise = Right $ catMaybes [eqCheck a c]
            applyCheck a _ = Right $ catMaybes [lambdaCheck a]

typecheckAlg (_ ::< Lambda id t b) =
    case partitionEithers [b] of
        ([eb], []) -> Left $ arrow t eb
        (_, errors) -> Right $ concat errors
        
typecheckAlg (_ ::< Map a b) =
    case partitionEithers [a, b] of
        ([ea, eb], []) -> mapCheck ea eb
        (_, errors) -> Right $ concat errors
        where
            mapCheck (Fix (Arrow a b)) (Fix (Power c d))
                | a == c = Left $ power b d
                | otherwise = Right $ catMaybes [eqCheck a c]
            mapCheck a b = Right $ catMaybes [lambdaCheck a, vectorCheck b]

typecheckAlg (_ ::< Reduce a b) =
    case partitionEithers [a, b] of
        ([ea, eb], []) -> reduceCheck ea eb
        (_, errors) -> Right $ concat errors
        where
            reduceCheck (Fix (Arrow a (Fix (Arrow b c)))) v@(Fix (Power d (Fix (Dim e))))
                | a == b && a == c && a == d && e /= 0 = Left a
                | otherwise = Right $ catMaybes [eqCheck a b, eqCheck a c, eqCheck a d] ++
                    [showT v ++ " is an empty vector" | e == 0]
            reduceCheck a b = Right $ catMaybes [biLambdaCheck a, vectorCheck b]            

typecheckAlg (_ ::< ZipWith a b c) =
    case partitionEithers [a, b, c] of
        ([ea, eb, ec], []) -> zipCheck ea eb ec
        (_, errors) -> Right $ concat errors
        where
            zipCheck (Fix (Arrow a (Fix (Arrow b c)))) (Fix (Power d e)) (Fix (Power f g))
                | a == d && b == f && e == g = Left $ power c e
                | otherwise = Right $ catMaybes [eqCheck a d, eqCheck b f, eqCheck e g]
            zipCheck a b c = Right $ catMaybes [biLambdaCheck a, vectorCheck b, vectorCheck c]   

eqCheck :: Type -> Type -> Maybe Error
eqCheck a b
    | a == b = Nothing
    | otherwise = Just $ showT a ++ " != " ++ showT b

scalarCheck :: Type -> Maybe Error
scalarCheck (Fix Double) = Nothing
scalarCheck a = Just $ showT a ++ " not a scalar"

lambdaCheck :: Type -> Maybe Error
lambdaCheck (Fix (Arrow _ _)) = Nothing
lambdaCheck a = Just $ showT a ++ " not a lambda"

biLambdaCheck :: Type -> Maybe Error
biLambdaCheck (Fix (Arrow _ (Fix (Arrow _ _)))) = Nothing
biLambdaCheck a = Just $ showT a ++ " not a binary lambda"

vectorCheck :: Type ->  Maybe Error
vectorCheck (Fix (Power _ _)) = Nothing
vectorCheck a = Just $ showT a ++ " not a vector"

getType :: TypecheckT ∈ fields => R fields -> Type
getType = fromLeft' . getIdentity . rget ([] :: [TypecheckT])