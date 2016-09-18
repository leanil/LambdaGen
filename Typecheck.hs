module Typecheck where

import Expr
import Recursion
import Type
import TypePrinter
import Utility (Fix(..))

import Data.Bifunctor --innen csak az Either-t?
import Data.Either
import Data.Maybe

type Error = String
type TypecheckT = Either Expr [Error]

typecheckAlg :: Algebra ExtF TypecheckT

typecheckAlg (ExtF (Scalar val) _) = Left $ Fix $ ExtF (Scalar val) double

typecheckAlg (ExtF (Variable id) t) = Left $ var id t

typecheckAlg (ExtF (VectorView id vSize) _) = Left $ Fix $ ExtF (VectorView id vSize) (power double (Type.size vSize))

typecheckAlg (ExtF (Vector elements) _) =
    case partitionEithers elements of
        (t, []) -> first (Fix . ExtF (Vector t)) (diffCheck $ map getType t)
        (_, errors) -> Right $ concat errors
        where
            diffCheck types = case filter (/= head types) (tail types) of
                []   -> Left $ power (head types) (size $ length types)
                diff -> Right $ mapMaybe (eqCheck (head types)) diff    

typecheckAlg (ExtF (Addition a b) _) =
    case partitionEithers [a, b] of
        ([ea, eb], []) -> first (Fix . ExtF (Addition ea eb)) (mulCheck (getType ea) (getType eb))
        (_, errors) -> Right $ concat errors
        where
            mulCheck (Fix Double) (Fix Double) = Left double
            mulCheck a b = Right $ mapMaybe scalarCheck (filter (/= double) [a, b])
                      
typecheckAlg (ExtF (Multiplication a b) _) =
    case partitionEithers [a, b] of
        ([ea, eb], []) -> first (Fix . ExtF (Multiplication ea eb)) (mulCheck (getType ea) (getType eb))
        (_, errors) -> Right $ concat errors
        where
            mulCheck (Fix Double) (Fix Double) = Left double
            mulCheck a b = Right $ mapMaybe scalarCheck (filter (/= double) [a, b])

typecheckAlg (ExtF (Apply a b) _) =
    case partitionEithers [a, b] of
        ([ea, eb], []) -> first (Fix . ExtF (Apply ea eb)) (applyCheck (getType ea) (getType eb))
        (_, errors) -> Right $ concat errors
        where
            applyCheck (Fix (Arrow a b)) c
                | a == c = Left b
                | otherwise = Right $ catMaybes [eqCheck a c]
            applyCheck a _ = Right $ catMaybes [lambdaCheck a]

typecheckAlg (ExtF (Lambda a b) t) =
    case partitionEithers [b] of
        ([eb], []) -> Left $ Fix $ ExtF (Lambda a eb) (arrow t (getType eb))
        (_, errors) -> Right $ concat errors
        
typecheckAlg (ExtF (Map a b) _) =
    case partitionEithers [a, b] of
        ([ea, eb], []) -> first (Fix . ExtF (Map ea eb)) (mapCheck (getType ea) (getType eb))
        (_, errors) -> Right $ concat errors
        where
            mapCheck (Fix (Arrow a b)) (Fix (Power c d))
                | a == c = Left $ power b d
                | otherwise = Right $ catMaybes [eqCheck a c]
            mapCheck a b = Right $ catMaybes [lambdaCheck a, vectorCheck b]

typecheckAlg (ExtF (Reduce a b) _) =
    case partitionEithers [a, b] of
        ([ea, eb], []) -> first (Fix . ExtF (Reduce ea eb)) (reduceCheck (getType ea) (getType eb))
        (_, errors) -> Right $ concat errors
        where
            reduceCheck (Fix (Arrow a (Fix (Arrow b c)))) v@(Fix (Power d s@(Fix (Size e))))
                | a == b && a == c && a == d && e /= 0 = Left $ power d s
                | otherwise = Right $ catMaybes [eqCheck a b, eqCheck a c, eqCheck a d] ++
                    if e == 0 then [show v ++ " is an empty vector"] else []
            reduceCheck a b = Right $ catMaybes [biLambdaCheck a, vectorCheck b]            

typecheckAlg (ExtF (ZipWith a b c) _) =
    case partitionEithers [a, b, c] of
        ([ea, eb, ec], []) -> first (Fix . ExtF (ZipWith ea eb ec)) (zipCheck (getType ea) (getType eb) (getType ec))
        (_, errors) -> Right $ concat errors
        where
            zipCheck (Fix (Arrow a (Fix (Arrow b c)))) (Fix (Power d e)) (Fix (Power f g))
                | a == d && b == f && e == g = Left $ power c e
                | otherwise = Right $ catMaybes [eqCheck a d, eqCheck b f, eqCheck e g]
            zipCheck a b c = Right $ catMaybes [biLambdaCheck a, vectorCheck b, vectorCheck c]   

getType :: Expr -> Type
getType = exprType . unFix

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
