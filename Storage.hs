{-# LANGUAGE DataKinds, FlexibleContexts, TupleSections, TypeOperators, ViewPatterns #-}

module Storage where

import Expr
import Recursion
import Typecheck
import TypePrinter
import Control.Arrow
import Control.Comonad.Cofree
import Data.List (unfoldr)
import Data.Vinyl
import Data.Vinyl.Functor 
import Data.Functor.Foldable (ana)
import System.Random

type AssignStgT = (StdGen, Bool)
type ResultId = Maybe Int

assignStgAlg :: TypecheckT ∈ fields => CoAlgebra (Cofree ExprF (R (ResultId ': fields))) ((Cofree ExprF (R fields)), AssignStgT)

assignStgAlg (r :< Scalar s, _) = (Identity Nothing :& r) ::< Scalar s

assignStgAlg (r :< VectorView id d s, _) = (Identity Nothing :& r) ::< VectorView id d s

assignStgAlg (r :< Vector elements, (g,_)) = (Identity Nothing :& r) ::< (Vector $ zip elements (map (,True) $ unfoldr (Just . split) g))

assignStgAlg (r :< Addition a b, (g,_)) = (Identity Nothing :& r) ::< Addition (a,(g1,True)) (b,(g2,True)) where
    (g1, g2) = split g

assignStgAlg (r :< Multiplication a b, (g,_)) = (Identity Nothing :& r) ::< Multiplication (a,(g1,True)) (b,(g2,True)) where
    (g1, g2) = split g  

assignStgAlg (r :< Apply a b, (g,s)) = (Identity (if s then Just x else Nothing) :& r) ::< 
    Apply (a,(g1,False)) (b,(g2,True)) where
        (x,g') = next g
        (g1, g2) = split g'

assignStgAlg (r :< Lambda i t a, (g,s)) = (Identity Nothing :& r) ::< Lambda i t (a,(g,s))

assignStgAlg (r :< Variable i t, _) = (Identity Nothing :& r) ::< Variable i t

assignStgAlg (r :< Map a b, (g,s)) = (Identity (if s then Just x else Nothing) :& r) ::< 
    Map (a,(g1,False)) (b,(g2,True)) where
        (x,g') = next g
        (g1, g2) = split g'

assignStgAlg (r :< Reduce a b, (g,s)) = (Identity (if s then Just x else Nothing) :& r) ::< 
    Reduce (a,(g1,False)) (b,(g2,True)) where
        (x,g') = next g
        (g1, g2) = split g'

assignStgAlg (r :< ZipWith a b c, (g,s)) = (Identity (if s then Just x else Nothing) :& r) ::< 
    ZipWith (a,(g1,False)) (b,(g2,True)) (c,(g3,True)) where
        (x,g') = next g
        (g1, (g2, g3)) = fmap split $ split g'

assignStorage :: TypecheckT ∈ fields => Cofree ExprF (R fields) -> Cofree ExprF (R (ResultId ': fields))
assignStorage e = ana assignStgAlg (e,(mkStdGen 0, True))

type ResultPack = [(Int,[Int],[Int])]

collectStgAlg :: (ResultId ∈ fields, TypecheckT ∈ fields) => Algebra (Cofree ExprF (R fields)) ResultPack

collectStgAlg (_ ::< Scalar{}) = []

collectStgAlg (_ ::< VectorView{}) = []

collectStgAlg (_ ::< Vector elements) = concat elements

collectStgAlg (_ ::< Addition a b) = a ++ b

collectStgAlg (_ ::< Multiplication a b) = a ++ b

collectStgAlg ((fieldVal ([] :: [ResultId]) &&& countDims . getType -> (Just x, ds)) ::< Apply a b) =
     (x, ds, defaultStrides ds) : a ++ b

collectStgAlg (_ ::< Apply a b) = a ++ b

collectStgAlg (_ ::< Lambda _ _ a) = a

collectStgAlg (_ ::< Variable{}) = []

collectStgAlg ((fieldVal ([] :: [ResultId]) &&& countDims . getType -> (Just x, ds)) ::< Map a b) =
     (x, ds, defaultStrides ds) : a ++ b

collectStgAlg (_ ::< Map a b) = a ++ b

collectStgAlg ((fieldVal ([] :: [ResultId]) &&& countDims . getType -> (Just x, ds)) ::< Reduce a b) =
     (x, ds, defaultStrides ds) : a ++ b
    
collectStgAlg (_ ::< Reduce a b) = a ++ b

collectStgAlg ((fieldVal ([] :: [ResultId]) &&& countDims . getType -> (Just x, ds)) ::< ZipWith a b c) =
     (x, ds, defaultStrides ds) : a ++ b ++ c

collectStgAlg (_ ::< ZipWith a b c) = a ++ b ++ c

