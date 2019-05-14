{-# LANGUAGE DataKinds, FlexibleContexts, LambdaCase, TypeApplications, TypeOperators, ViewPatterns #-}

module Typecheck where

import Expr
import Print
import Recursion
import Type
import Control.Comonad (extract)
import Data.Either (lefts)
import Data.Functor.Foldable (cata)
import Data.List (intercalate, nub)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl (type (∈))

type Error = String
type TypecheckT = Either Type [Error]

allLeft :: [TypecheckT] -> Maybe [Type]
allLeft x = let l = lefts x in if length l == length x then Just l else Nothing

typecheckAlg :: Algebra (Expr fields) TypecheckT

typecheckAlg (_ ::< Scalar _) = Left double

typecheckAlg (_ ::< Variable _ t) = Left t

typecheckAlg (_ ::< View _ d _) = Left $ power double d

typecheckAlg (_ ::< ScalarOp _ (Left FDouble) (Left FDouble)) = Left double
typecheckAlg (_ ::< ScalarOp _ (Left a) (Left b)) = Right $ mapMaybe scalarCheck [a,b]

typecheckAlg (_ ::< Apply (Left (FArrow a b)) (allLeft -> Just c))
    | length c > length a = Right ["more arguments than lambda parameters in Apply"]
    | and (zipWith (==) a c) = Left $ if length a == length c then b else arrow (drop (length c) a) b
    | otherwise = Right $ catMaybes (zipWith eqCheck a c)       
typecheckAlg (_ ::< Apply (Left a) _) = Right $ [showT a ++ " not a lambda"]

typecheckAlg (_ ::< Lambda v (allLeft . map snd -> Just _) (Left b)) = Left $ arrow (map snd v) b

typecheckAlg (_ ::< RnZ (Left (FArrow [a,b] c)) (Left (FArrow d e)) (allLeft -> Just f)) =
    typeOrErrors a [eqCheck a b, eqCheck a c, eqCheck a e, eqVectorCheck f, varMatchCheck d f]
typecheckAlg (_ ::< RnZ (Left a) (Left b) (allLeft -> Just c)) = Right $ catMaybes [biLambdaCheck a, biLambdaCheck b, eqVectorCheck c]            

typecheckAlg (_ ::< ZipWithN (Left (FArrow a b)) (allLeft -> Just c)) =
    typeOrErrors (raiseToPower b $ size $ head c) [eqVectorCheck c, varMatchCheck a c]
typecheckAlg (_ ::< ZipWithN (Left a) (allLeft -> Just b)) = Right $ catMaybes [biLambdaCheck a, eqVectorCheck b]   

typecheckAlg (_ ::< Flip (i,j) (Left (FPower a b)))
    | min i j >= 0 && max i j < length b = Left $ power' a (swap i j b)
    | otherwise = Right ["flip index out of range"]

typecheckAlg (_ ::< Subdiv i block (Left (FPower a b)))
    | i < 0 || i >= length b = Right ["subdiv index out of range"]
    | mod (fst $ b !! i) block /= 0 = Right ["subdiv block size incompatible"]
    | otherwise = Left $ power' a $ 
                  take i b ++ [(div (fst $ b !! i) block, block*(snd $ b !! i)), (block,snd $ b !! i)] ++ drop (i+1) b

typecheckAlg (_ ::< Flatten i (Left (FPower a b)))
    | i < 0 || i >= (length b) - 1 = Right ["subdiv index out of range"]
    | otherwise = Left $ power' a $ 
                  take i b ++ (fst (b !! i) * fst (b !! (i+1)), snd (b !! (i+1))) : drop (i+2) b

typecheckAlg (_ ::< node) = Right $ foldMap (either (const []) id) node

typecheck :: Expr fields -> Either Error (Expr (TypecheckT ': fields))
typecheck (cata (annotate typecheckAlg) -> expr) = 
    case fieldVal @TypecheckT $ extract expr of
        (Left _) -> Right expr
        (Right err) -> Left $ intercalate "\n" err ++ "\n\n" ++ printExpr (Proxy :: Proxy (R '[TypecheckT])) expr

typecheck' :: Expr fields -> Expr (TypecheckT ': fields)
typecheck' (cata (annotate typecheckAlg) -> expr) = 
    case fieldVal @TypecheckT $ extract expr of
        (Left _) -> expr
        (Right err) -> error $ intercalate "\n" err ++ "\n\n" ++ printExpr (Proxy :: Proxy (R '[TypecheckT])) expr

swap :: Int -> Int -> [a] -> [a]
swap i j xs = [get k x | (k, x) <- zip [0..] xs]
    where get k x | k == i = xs !! j
                  | k == j = xs !! i
                  | otherwise = x

typeOrErrors :: Type -> [Maybe Error] -> TypecheckT
typeOrErrors t (catMaybes -> err) = case err of
    [] -> Left t
    xs -> Right xs

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

eqVectorCheck :: [Type] -> Maybe Error
eqVectorCheck (nub . map size -> [s]) | s > 0 = Nothing
eqVectorCheck a = Just $ show a ++ " sould be a non-empty list of equal sized (non-empy) vectors"

elemType :: Type -> Type
elemType (FPower a b@(_:_:_)) = power' a (tail b)
elemType (FPower a [_]) = a

varMatchCheck :: [Type] -> [Type] -> Maybe Error
varMatchCheck vs as 
    | length vs == length as =
        case catMaybes $ zipWith eqCheck vs (map elemType as) of
            [] -> Nothing
            xs -> Just $ "[" ++ intercalate "," xs ++ "]"
    | otherwise = Just $ show vs ++ " and " ++ show as ++ " shold have equal length"

vectorCheck :: Type ->  Maybe Error
vectorCheck (FPower _ _) = Nothing
vectorCheck a = Just $ showT a ++ " not a vector"

getType :: TypecheckT ∈ fields => R fields -> Type
getType (fieldVal @TypecheckT -> Left t) = t
getType _ = error "getType on untyped expression"