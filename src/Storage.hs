{-# LANGUAGE DataKinds, DuplicateRecordFields, FlexibleContexts, TupleSections, TypeOperators, ViewPatterns #-}

module Storage where

import Expr
import Parallel
import Recursion
import Typecheck
import TypePrinter
import Control.Arrow
import Control.Comonad.Cofree (Cofree(..))
import Data.List (intercalate, unfoldr)
import Data.Monoid
import Data.Vinyl
import Data.Vinyl.Functor (Identity(..))
import Data.Functor.Foldable (ana)
import System.Random

type AssignStgT = (StdGen, Bool)
data ResultId = Inherit | Implicit String | Prealloc Int deriving Show
data Result = Std ResultId | Red (ResultId, ResultId) deriving Show

getPrimary :: Result -> ResultId
getPrimary (Std x) = x
getPrimary (Red (x,_)) = x

assignStgAlg :: ParData ∈ fields => CoAlgebra (Cofree ExprF (R (Result ': fields))) ((Cofree ExprF (R fields)), AssignStgT)

assignStgAlg (r :< Scalar x, s) = (Identity (fst $ assignHelper s (Just $ show x)) :& r) ::< Scalar x

assignStgAlg (r :< VectorView id a b, (g, True)) = (Identity (Std $ Implicit viewName) :& r) ::< VectorView id a b where
    viewName = id ++ show (fst $ next g)

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
assignStorage e = root $ ana assignStgAlg (e,(mkStdGen 0, True)) where
    root t@((getPrimary . fieldVal -> Prealloc _) :< _) = t
    root (r :< t) = rput (Identity $ Std $ Prealloc 0) r :< t

dims :: [Int] -> Int -> String
dims d 1 = concatMap (("," ++) . show) d
dims d tn = "," ++ show tn ++ dims d 1

strides :: MemStruct -> Int -> String
strides (_,s) 1 = "std::array<size_t," ++ show (length s) ++ ">{" ++ intercalate "," (map show s) ++ "}"
strides (d,s) tn = strides (d,product d : s) 1

viewType :: MemStruct -> Int -> String -> (String,String)
viewType mem@(d,_) tn ptrType = ("View<" ++ ptrType ++ ",double" ++ dims d tn ++ ">", strides mem tn)

newtype ResultPack = ResultPack ([ResultStg], [BigVector]) deriving Show -- collect vecView dimensions to allocate buffers for user data
data ResultStg = ResultStg { id :: Int, tnum :: Int, mem :: MemStruct } deriving (Eq, Show, Ord)
data BigVector = BigVector { id :: String, dataId :: String, mem :: MemStruct } deriving (Eq, Show, Ord)
type MemStruct = ([Int], [Int])

merge :: Ord a => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys)
    | x < y  = x : merge xs (y:ys)
    | x > y  = y : merge (x:xs) ys
    | x == y = x : merge xs ys

instance Monoid ResultPack where
    mappend (ResultPack (a, b)) (ResultPack (c, d)) = ResultPack (a ++ c, b ++ d)
    mempty = ResultPack ([], [])

collectStgAlg :: (Result ∈ fields, ParData ∈ fields, TypecheckT ∈ fields) => Algebra (Cofree ExprF (R fields)) ResultPack

collectStgAlg (r ::< Scalar{}) = getStgAndDims r

collectStgAlg ((fieldVal -> Std (Implicit id)) ::< VectorView dataId d s) = ResultPack([], [BigVector id dataId (d,s)])

collectStgAlg (r ::< Vector elements) = getStgAndDims r <> mconcat elements

collectStgAlg (r ::< Addition a b) = getStgAndDims r <> a <> b

collectStgAlg (r ::< Multiplication a b) = getStgAndDims r <>  a <> b

collectStgAlg (r ::< Apply a b) = getStgAndDims r <> a <> b

collectStgAlg (r ::< Lambda _ _ a) = getStgAndDims r <> a

collectStgAlg (r ::< Variable{}) = getStgAndDims r

collectStgAlg (r ::< Map a b) = getStgAndDims r <> a <> b
    
collectStgAlg (r ::< Reduce a b) = getStgAndDims r <> a <> b

collectStgAlg (r ::< ZipWith a b c) = getStgAndDims r <> a <> b <> c

getStgAndDims :: (Result ∈ fields, ParData ∈ fields, TypecheckT ∈ fields) => R (fields) -> ResultPack
getStgAndDims r = ResultPack (std r ++ temp r, []) where
    std (getPrimary . fieldVal &&& fst . getParData -> (Prealloc x, t)) = [ResultStg x t (ds, defaultStrides ds)]
    std (getPrimary . fieldVal -> _) = []
    temp (fieldVal &&& snd . getParData -> (Red (_,Prealloc x), Just t)) = [ResultStg x t (ds, defaultStrides ds)]
    temp _ = []
    ds = countDims $ getType r