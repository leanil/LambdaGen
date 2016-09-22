module Typecheck where

import Expr
import Recursion
import Type
import TypePrinter
import Utility (Fix(..))

import Control.Comonad.Cofree
import Data.Bifunctor --innen csak az Either-t?
import Data.Either
import Data.Maybe

type Expr_T = Cofree ExprF Type
type Error = String
type TypecheckT = Either Expr_T [Error]

typecheckAlg :: Algebra ExprF TypecheckT

typecheckAlg (Scalar val) = Left $ double :< Scalar val 

typecheckAlg (Variable id t) = Left $ t :< Variable id t

typecheckAlg (VectorView id vSize) = Left $ power double (Type.size vSize) :< VectorView id vSize

typecheckAlg (Vector elements) =
    case partitionEithers elements of
        (t, []) -> first (:< Vector t) (diffCheck $ map getType t)
        (_, errors) -> Right $ concat errors
        where
            diffCheck types = case filter (/= head types) (tail types) of
                []   -> Left $ power (head types) (size $ length types)
                diff -> Right $ mapMaybe (eqCheck (head types)) diff    

typecheckAlg (Addition a b) =
    case partitionEithers [a, b] of
        ([ea, eb], []) -> first (:< Addition ea eb) (mulCheck (getType ea) (getType eb))
        (_, errors) -> Right $ concat errors
        where
            mulCheck (Fix Double) (Fix Double) = Left double
            mulCheck a b = Right $ mapMaybe scalarCheck (filter (/= double) [a, b])
                      
typecheckAlg (Multiplication a b) =
    case partitionEithers [a, b] of
        ([ea, eb], []) -> first (:< Multiplication ea eb) (mulCheck (getType ea) (getType eb))
        (_, errors) -> Right $ concat errors
        where
            mulCheck (Fix Double) (Fix Double) = Left double
            mulCheck a b = Right $ mapMaybe scalarCheck (filter (/= double) [a, b])

typecheckAlg (Apply a b) =
    case partitionEithers [a, b] of
        ([ea, eb], []) -> first (:< Apply ea eb) (applyCheck (getType ea) (getType eb))
        (_, errors) -> Right $ concat errors
        where
            applyCheck (Fix (Arrow a b)) c
                | a == c = Left b
                | otherwise = Right $ catMaybes [eqCheck a c]
            applyCheck a _ = Right $ catMaybes [lambdaCheck a]

typecheckAlg (Lambda id t b) =
    case partitionEithers [b] of
        ([eb], []) -> Left $ arrow t (getType eb) :< Lambda id t eb
        (_, errors) -> Right $ concat errors
        
typecheckAlg (Map a b) =
    case partitionEithers [a, b] of
        ([ea, eb], []) -> first (:< Map ea eb) (mapCheck (getType ea) (getType eb))
        (_, errors) -> Right $ concat errors
        where
            mapCheck (Fix (Arrow a b)) (Fix (Power c d))
                | a == c = Left $ power b d
                | otherwise = Right $ catMaybes [eqCheck a c]
            mapCheck a b = Right $ catMaybes [lambdaCheck a, vectorCheck b]

typecheckAlg (Reduce a b) =
    case partitionEithers [a, b] of
        ([ea, eb], []) -> first (:< Reduce ea eb) (reduceCheck (getType ea) (getType eb))
        (_, errors) -> Right $ concat errors
        where
            reduceCheck (Fix (Arrow a (Fix (Arrow b c)))) v@(Fix (Power d s@(Fix (Size e))))
                | a == b && a == c && a == d && e /= 0 = Left $ power d s
                | otherwise = Right $ catMaybes [eqCheck a b, eqCheck a c, eqCheck a d] ++
                    [show v ++ " is an empty vector" | e == 0]
            reduceCheck a b = Right $ catMaybes [biLambdaCheck a, vectorCheck b]            

typecheckAlg (ZipWith a b c) =
    case partitionEithers [a, b, c] of
        ([ea, eb, ec], []) -> first (:< ZipWith ea eb ec) (zipCheck (getType ea) (getType eb) (getType ec))
        (_, errors) -> Right $ concat errors
        where
            zipCheck (Fix (Arrow a (Fix (Arrow b c)))) (Fix (Power d e)) (Fix (Power f g))
                | a == d && b == f && e == g = Left $ power c e
                | otherwise = Right $ catMaybes [eqCheck a d, eqCheck b f, eqCheck e g]
            zipCheck a b c = Right $ catMaybes [biLambdaCheck a, vectorCheck b, vectorCheck c]   

getType :: Expr_T -> Type
getType (t :< _) = t

eqCheck :: Type -> Type -> Maybe Error
eqCheck a b
    | a == b = Nothing
    | otherwise = Just $ show a ++ " != " ++ show b

scalarCheck :: Type -> Maybe Error
scalarCheck (Fix Double) = Nothing
scalarCheck a = Just $ show a ++ " not a scalar"

lambdaCheck :: Type -> Maybe Error
lambdaCheck (Fix (Arrow a b)) = Nothing
lambdaCheck a = Just $ show a ++ " not a lambda"

biLambdaCheck :: Type -> Maybe Error
biLambdaCheck (Fix (Arrow a (Fix (Arrow b c)))) = Nothing
biLambdaCheck a = Just $ show a ++ " not a binary lambda"

vectorCheck :: Type ->  Maybe Error
vectorCheck (Fix (Power a b)) = Nothing
vectorCheck a = Just $ show a ++ " not a vector"
