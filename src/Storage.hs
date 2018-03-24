{-# LANGUAGE DataKinds, DuplicateRecordFields, FlexibleContexts, TupleSections, TypeOperators, ViewPatterns #-}

module Storage where

import Expr
import Parallel
import Recursion
import Type
import Typecheck
import Control.Arrow
import Control.Comonad.Cofree (Cofree(..))
import Data.List (intercalate)
import Data.Monoid
import Data.Vinyl
import Data.Vinyl.Functor (Identity(..))
import Data.Functor.Foldable (ana)
import System.Random

type AssignStgT = (StdGen, Bool) -- (id generator, has to allocate)
data ResultId = Inherit | Implicit String | Prealloc Int deriving Show
data Result = Std ResultId | Red (ResultId, ResultId) deriving Show

getPrimary :: Result -> ResultId
getPrimary (Std x) = x
getPrimary (Red (x,_)) = x

assignStgAlg :: ParData ∈ fields => CoAlgebra (Cofree ExprF Result) ((Cofree ExprF (R fields)), AssignStgT)

-- assignStgAlg (_ :< a, _) = Std Inherit ::< a
assignStgAlg (_ :< a@Scalar{}, (_,False)) = Std Inherit ::< castLeaf a
assignStgAlg (_ :< Scalar x, _) = Std (Implicit $ show x) ::< Scalar x

assignStgAlg (_ :< VectorView i a b, (g, True)) = Std (Implicit viewName) ::< VectorView i a b where
    viewName = i ++ show (fst $ next g)

-- assignStgAlg (r :< Vector elements, s) = s' ::< (Vector $ zip elements (map (,True) $ unfoldr (Just .split) g'))where
--     (s', g') = assignHelper s Nothing

assignStgAlg (_ :< n@Addition{}, s) = r ::< zipExprF (,) n (zip g [True, True]) where
    (r, g) = assignHelper s 2

assignStgAlg (_ :< Multiplication a b, s) = r ::< Multiplication (a,(g1,True)) (b,(g2,True)) where
    (r, [g1,g2]) = assignHelper s 2

assignStgAlg (_ :< n@(Apply _ b), s) = r ::< zipExprF (,) n (zip g (False:repeat True)) where
    (r, g) = assignHelper s (length b + 1)

assignStgAlg (_ :< Lambda v a, s) = Std Inherit ::< Lambda v (a,s)

assignStgAlg (_ :< Variable i t, (_,False)) = Std Inherit ::< Variable i t
assignStgAlg (_ :< Variable i t, _) = Std (Implicit i) ::< Variable i t

assignStgAlg (_ :< Map a b, s) = r ::< Map (a,(g1,False)) (b,(g2,True)) where
    (r, [g1,g2]) = assignHelper s 2

assignStgAlg (r :< Reduce a b, s) = mkResult r ::< Reduce (a,(g1,False)) (b,(g2,True)) where
    (s'@(Std x), [g',g2]) = assignHelper s 2
    (y, g1) = next g'
    mkResult (snd . getParData -> Just _) = Red (x,Prealloc y)
    mkResult (snd . getParData -> Nothing) = s'

assignStgAlg (_ :< ZipWith a b c, s) = r ::< ZipWith (a,(g1,False)) (b,(g2,True)) (c,(g3,True)) where
    (r, [g1,g2,g3]) = assignHelper s 3

assignStgAlg (_ :< Compose a b, (g,_)) = r ::< Compose (a,(g1,False)) (b,(g2,False)) where
    (r, [g1,g2]) = assignHelper (g,True) 2 

assignHelper :: AssignStgT -> Int -> (Result, [StdGen])
assignHelper (g, False) n = (Std Inherit, splitN n g)
assignHelper (g, True)  n = let (x, g') = next g in (Std $ Prealloc x, splitN n g')

splitN :: Int -> StdGen -> [StdGen]
splitN 1 g = [g]
splitN n g = let (g1,g2) = split g in g1 : splitN (n-1) g2

assignStorage :: ParData ∈ fields => Cofree ExprF (R fields) -> Cofree ExprF (R (Result ': fields))
assignStorage e = root $ ana (annotateAna assignStgAlg) (e,(mkStdGen 0, True)) where
    root t@((getPrimary . fieldVal -> Prealloc _) :< _) = t
    root (r :< t) = rput (Identity $ Std $ Prealloc 0) r :< t

dims :: [Int] -> Int -> String
dims d 1 = concatMap (("," ++) . show) d
dims d tn = "," ++ show tn ++ dims d 1

strides :: MemStruct -> Int -> String
strides (_,s) 1 = "std::array<size_t," ++ show (length s) ++ ">{" ++ intercalate "," (map show s) ++ "}"
strides (d,s) _ = strides (d,product d : s) 1

viewType :: MemStruct -> Int -> String -> (String,String)
viewType m@(d,_) tn ptrType = ("View<" ++ ptrType ++ ",double" ++ dims d tn ++ ">", strides m tn)

newtype ResultPack = ResultPack ([ResultStg], [BigVector]) deriving Show -- collect vecView dimensions to allocate buffers for user data
data ResultStg = ResultStg { id :: Int, tnum :: Int, getMem :: MemStruct } deriving (Eq, Show, Ord)
data BigVector = BigVector { id :: String, dataId :: String, getMem :: MemStruct } deriving (Eq, Show, Ord)
type MemStruct = ([Int], [Int])

defaultMem :: (Result ∈ fields, ParData ∈ fields, TypecheckT ∈ fields) => R (fields) -> MemStruct
defaultMem (countDims . getType -> ds) = (ds, defaultStrides ds)

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

-- deriving instance Foldable f => Foldable (Cofree f)

collectStgAlg :: (Result ∈ fields, ParData ∈ fields, TypecheckT ∈ fields) => Algebra (Cofree ExprF (R fields)) ResultPack

collectStgAlg ((fieldVal -> Std (Implicit i)) ::< VectorView di d s) = ResultPack([], [BigVector i di (d,s)])

-- collectStgAlg (r ::< Vector elements) = getStgAndDims r <> mconcat elements

-- getStgAndDims r <> all children

-- foldmap 

collectStgAlg (r ::< Scalar{}) = getStgAndDims r

collectStgAlg (r ::< Addition a b) = getStgAndDims r <> a <> b

collectStgAlg (r ::< Multiplication a b) = getStgAndDims r <>  a <> b

collectStgAlg (r ::< Apply a b) = getStgAndDims r <> a <> mconcat b

collectStgAlg (r ::< Lambda _ a) = getStgAndDims r <> a

collectStgAlg (r ::< Variable{}) = getStgAndDims r

collectStgAlg (r ::< Map a b) = getStgAndDims r <> a <> b
    
collectStgAlg (r ::< Reduce a b) = getStgAndDims r <> a <> b

collectStgAlg (r ::< ZipWith a b c) = getStgAndDims r <> a <> b <> c

collectStgAlg (r@(snd . getParData -> Just t) ::< Compose a b) = 
    ResultPack ([ResultStg x t (defaultMem r)],[]) <> a <> b where
        Prealloc x = getPrimary $ fieldVal r
collectStgAlg (r ::< Compose a b) = getStgAndDims r <> a <> b

getStgAndDims :: (Result ∈ fields, ParData ∈ fields, TypecheckT ∈ fields) => R (fields) -> ResultPack
getStgAndDims r = ResultPack (std r ++ temp r, []) where
    std (getPrimary . fieldVal &&& fst . getParData -> (Prealloc x, t)) = [ResultStg x t (defaultMem r)]
    std (getPrimary . fieldVal -> _) = []
    temp (fieldVal &&& snd . getParData -> (Red (_,Prealloc x), Just t)) = [ResultStg x t (defaultMem r)]
    temp _ = []