{-# LANGUAGE DataKinds, FlexibleContexts, TupleSections, TypeOperators, ViewPatterns #-}

module Storage where

import Expr
import Parallel
import Recursion
import Typecheck
import TypePrinter
import Control.Arrow
import Control.Comonad.Cofree (Cofree(..))
import Data.List (unfoldr)
import Data.Vinyl
import Data.Vinyl.Functor (Identity(..))
import Data.Functor.Foldable (ana)
import System.Random

type AssignStgT = (StdGen, Bool)
data ResultId = Std (Maybe Int) | Red (Maybe Int, Int)

getPrimary :: ResultId -> Maybe Int
getPrimary (Std x) = x
getPrimary (Red (x,_)) = x

assignStgAlg :: ParData ∈ fields => CoAlgebra (Cofree ExprF (R (ResultId ': fields))) ((Cofree ExprF (R fields)), AssignStgT)

assignStgAlg (r :< Scalar x, s) = (Identity (fst $ assignHelper s) :& r) ::< Scalar x

assignStgAlg (r :< VectorView a b c, s) = (Identity (fst $ assignHelper s) :& r) ::< VectorView a b c

assignStgAlg (r :< Vector elements, s) = (Identity s' :& r) ::< (Vector $ zip elements (map (,True) $ unfoldr (Just .split) g'))where
    (s', g') = assignHelper s

assignStgAlg (r :< Addition a b, s) = (Identity s' :& r) ::< Addition (a,(g1,True)) (b,(g2,True)) where
    (s', g') = assignHelper s
    (g1, g2) = split g'

assignStgAlg (r :< Multiplication a b, s) = (Identity s' :& r) ::< Multiplication (a,(g1,True)) (b,(g2,True)) where
    (s', g') = assignHelper s
    (g1, g2) = split g'

assignStgAlg (r :< Apply a b, s) = (Identity s' :& r) ::< Apply (a,(g1,False)) (b,(g2,True)) where
    (s', g') = assignHelper s
    (g1, g2) = split g'

assignStgAlg (r :< Lambda i t a, s) = (Identity (Std Nothing) :& r) ::< Lambda i t (a,s)

assignStgAlg (r :< Variable i t, s) = (Identity (fst $ assignHelper s) :& r) ::< Variable i t

assignStgAlg (r :< Map a b, s) = (Identity s' :& r) ::< Map (a,(g1,False)) (b,(g2,True)) where
        (s', g') = assignHelper s
        (g1, g2) = split g'

assignStgAlg (r :< Reduce a b, s) = (Identity (mkResultId r) :& r) ::< Reduce (a,(g1,False)) (b,(g2,True)) where
        (s'@(Std x), g') = assignHelper s
        (y, g'') = next g'
        (g1, g2) = split g''
        mkResultId (snd . getParData -> Just _) = Red (x,y)
        mkResultId (snd . getParData -> Nothing) = s'

assignStgAlg (r :< ZipWith a b c, s) = (Identity s' :& r) ::< ZipWith (a,(g1,False)) (b,(g2,True)) (c,(g3,True)) where
        (s', g') = assignHelper s
        (g1, (g2, g3)) = fmap split $ split g'

assignHelper :: AssignStgT -> (ResultId, StdGen)
assignHelper (g, False) = (Std Nothing, g)
assignHelper (g, True) = let (x, g') = next g in (Std $ Just x, g')

assignStorage :: (ParData ∈ fields, TypecheckT ∈ fields) => Cofree ExprF (R fields) -> Cofree ExprF (R (ResultId ': fields))
assignStorage e = ana assignStgAlg (e,(mkStdGen 0, True))

type ResultPack = [ResultStg]
data ResultStg = ResultStg { id :: Int, tnum :: Int, dim :: [Int], stride :: [Int] }

collectStgAlg :: (ResultId ∈ fields, ParData ∈ fields, TypecheckT ∈ fields) => Algebra (Cofree ExprF (R fields)) ResultPack

collectStgAlg (r ::< Scalar{}) = getStgAndDims r

collectStgAlg (r ::< VectorView{}) = getStgAndDims r

collectStgAlg (r ::< Vector elements) = getStgAndDims r ++ concat elements

collectStgAlg (r ::< Addition a b) = getStgAndDims r ++ a ++ b

collectStgAlg (r ::< Multiplication a b) = getStgAndDims r ++  a ++ b

collectStgAlg (r ::< Apply a b) = getStgAndDims r ++ a ++ b

collectStgAlg (r ::< Lambda _ _ a) = getStgAndDims r ++ a

collectStgAlg (r ::< Variable{}) = getStgAndDims r

collectStgAlg (r ::< Map a b) = getStgAndDims r ++ a ++ b
    
collectStgAlg (r ::< Reduce a b) = getStgAndDims r ++ a ++ b

collectStgAlg (r ::< ZipWith a b c) = getStgAndDims r ++ a ++ b ++ c

getStgAndDims :: (ResultId ∈ fields, ParData ∈ fields, TypecheckT ∈ fields) => R (fields) -> ResultPack
getStgAndDims r = std r ++ temp r where
    std (getPrimary . fieldVal ([] :: [ResultId]) -> Nothing) = []
    std (getPrimary . fieldVal ([] :: [ResultId]) &&& fst . getParData -> (Just x, t)) = [ResultStg x t ds $ defaultStrides ds]
    temp (fieldVal ([] :: [ResultId]) &&& snd . getParData -> (Red (_,x), Just t)) = [ResultStg x t ds $ defaultStrides ds]
    temp _ = []
    ds = countDims $ getType r