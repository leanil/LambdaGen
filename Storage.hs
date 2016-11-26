{-# LANGUAGE DataKinds, FlexibleContexts, TupleSections, TypeOperators, ViewPatterns #-}

module Storage where

import Expr
import Parallel
import Recursion
import Typecheck
import TypePrinter
import Control.Arrow
import Control.Comonad.Cofree (Cofree(..))
import Data.List (intercalate, unfoldr)
import Data.Vinyl
import Data.Vinyl.Functor (Identity(..))
import Data.Functor.Foldable (ana)
import System.Random

type AssignStgT = (StdGen, Bool)
data ResultId = Inherit | Implicit String | Prealloc Int
data Result = Std ResultId | Red (ResultId, ResultId)

getPrimary :: Result -> ResultId
getPrimary (Std x) = x
getPrimary (Red (x,_)) = x

assignStgAlg :: ParData ∈ fields => CoAlgebra (Cofree ExprF (R (Result ': fields))) ((Cofree ExprF (R fields)), AssignStgT)

assignStgAlg (r :< Scalar x, s) = (Identity (fst $ assignHelper s (Just $ show x)) :& r) ::< Scalar x

assignStgAlg (r :< VectorView id a b, s) = (Identity (fst $ assignHelper s (Just $ mkView id a b)) :& r) ::< VectorView id a b where
    mkView id d s = "View<double" ++ sizes d s ++ ">(bigVectors.at(\"" ++ id ++ "\"))"

assignStgAlg (r :< Vector elements, s) = (Identity s' :& r) ::< (Vector $ zip elements (map (,True) $ unfoldr (Just .split) g'))where
    (s', g') = assignHelper s Nothing

assignStgAlg (r :< Addition a b, s) = (Identity s' :& r) ::< Addition (a,(g1,True)) (b,(g2,True)) where
    (s', g') = assignHelper s Nothing
    (g1, g2) = split g'

assignStgAlg (r :< Multiplication a b, s) = (Identity s' :& r) ::< Multiplication (a,(g1,True)) (b,(g2,True)) where
    (s', g') = assignHelper s Nothing
    (g1, g2) = split g'

assignStgAlg (r :< Apply a b, s) = (Identity s' :& r) ::< Apply (a,(g1,False)) (b,(g2,True)) where
    (s', g') = assignHelper s Nothing
    (g1, g2) = split g'

assignStgAlg (r :< Lambda i t a, s) = (Identity (Std Inherit) :& r) ::< Lambda i t (a,s)

assignStgAlg (r :< Variable id t, s) = (Identity (fst $ assignHelper s (Just id)) :& r) ::< Variable id t

assignStgAlg (r :< Map a b, s) = (Identity s' :& r) ::< Map (a,(g1,False)) (b,(g2,True)) where
        (s', g') = assignHelper s Nothing
        (g1, g2) = split g'

assignStgAlg (r :< Reduce a b, s) = (Identity (mkResult r) :& r) ::< Reduce (a,(g1,False)) (b,(g2,True)) where
        (s'@(Std x), g') = assignHelper s Nothing
        (y, g'') = next g'
        (g1, g2) = split g''
        mkResult (snd . getParData -> Just _) = Red (x,Prealloc y)
        mkResult (snd . getParData -> Nothing) = s'

assignStgAlg (r :< ZipWith a b c, s) = (Identity s' :& r) ::< ZipWith (a,(g1,False)) (b,(g2,True)) (c,(g3,True)) where
        (s', g') = assignHelper s Nothing
        (g1, (g2, g3)) = fmap split $ split g'

assignHelper :: AssignStgT -> Maybe String -> (Result, StdGen)
assignHelper (g, False) _ = (Std Inherit, g)
assignHelper (g, True) id =
    let (x, g') = next g in
    case id of 
        Nothing -> (Std $ Prealloc x, g')
        Just s  -> (Std $ Implicit s, g')

assignStorage :: ParData ∈ fields => Cofree ExprF (R fields) -> Cofree ExprF (R (Result ': fields))
assignStorage e = ana assignStgAlg (e,(mkStdGen 0, True))

sizes :: [Int] -> [Int] -> String
sizes [] [] = ""
sizes d s = "," ++ intercalate "," (zipWith (\x y -> "Pair<" ++ show x ++ "," ++ show y ++ ">") d s)

type ResultPack = [ResultStg]
data ResultStg = ResultStg { id :: Int, tnum :: Int, dim :: [Int], stride :: [Int] }

collectStgAlg :: (Result ∈ fields, ParData ∈ fields, TypecheckT ∈ fields) => Algebra (Cofree ExprF (R fields)) ResultPack

collectStgAlg (r ::< Scalar{}) = getStgAndDims r

collectStgAlg (r ::< VectorView _ d s) = getStgAndDims r
    -- case (getPrimary . fieldVal ([] :: [Result]) &&& fst . getParData) r of
    -- (Nothing,_) -> []
    -- (Just x, t) -> [ResultStg x t d s]

collectStgAlg (r ::< Vector elements) = getStgAndDims r ++ concat elements

collectStgAlg (r ::< Addition a b) = getStgAndDims r ++ a ++ b

collectStgAlg (r ::< Multiplication a b) = getStgAndDims r ++  a ++ b

collectStgAlg (r ::< Apply a b) = getStgAndDims r ++ a ++ b

collectStgAlg (r ::< Lambda _ _ a) = getStgAndDims r ++ a

collectStgAlg (r ::< Variable{}) = getStgAndDims r

collectStgAlg (r ::< Map a b) = getStgAndDims r ++ a ++ b
    
collectStgAlg (r ::< Reduce a b) = getStgAndDims r ++ a ++ b

collectStgAlg (r ::< ZipWith a b c) = getStgAndDims r ++ a ++ b ++ c

getStgAndDims :: (Result ∈ fields, ParData ∈ fields, TypecheckT ∈ fields) => R (fields) -> ResultPack
getStgAndDims r = std r ++ temp r where
    std (getPrimary . fieldVal ([] :: [Result]) &&& fst . getParData -> (Prealloc x, t)) = [ResultStg x t ds $ defaultStrides ds]
    std (getPrimary . fieldVal ([] :: [Result]) -> _) = []
    temp (fieldVal ([] :: [Result]) &&& snd . getParData -> (Red (_,Prealloc x), Just t)) = [ResultStg x t ds $ defaultStrides ds]
    temp _ = []
    ds = countDims $ getType r