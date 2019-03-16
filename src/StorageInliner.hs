{-# LANGUAGE DataKinds, DuplicateRecordFields, FlexibleContexts, TupleSections, TypeOperators, ViewPatterns #-}

module StorageInliner where

import Expr
import Metrics
import Parallel
import Recursion
import Type
import Typecheck
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree(..))
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Monoid ((<>))
import Data.Vinyl (rput, type (∈))
import Data.Vinyl.Functor (Identity(..))
import Data.Foldable (fold)
import Data.Functor.Foldable (ana, cata)

type AssignStgT = Maybe String -- (parent storage if the child can use it)
data Result = Std { getName :: String, getInherit :: Bool, getAssign :: Bool }
            | WithTmp { getName :: String, getTmpName :: String, getInherit :: Bool, getAssign :: Bool }
             deriving Show

getPrimary :: Result -> String
getPrimary (Std x _ _) = x
getPrimary (WithTmp x _ _ _) = x

assignStgAlg :: (NodeId ∈ fields, ParData ∈ fields) => CoAlgebra (Cofree ExprF Result) (Expr fields, AssignStgT)

assignStgAlg (_ :< Scalar x, s) = makeIdLeaf (show x) s ::< Scalar x

-- It's possible to use the same user data multiple times with different types
assignStgAlg (r :< View i a b, s) = makeIdLeaf (i ++ "_" ++ show (getNodeId r)) s ::< View i a b

assignStgAlg (_ :< Variable i t, s) = makeIdLeaf i s ::< Variable i t

assignStgAlg (r :< n@ScalarOp{}, s) = fst (makeId r s) ::< zipExprF n [Nothing, Nothing]

assignStgAlg (r :< n@Apply{}, s) = result ::< zipExprF n (Just myId:repeat Nothing) where
    (result, myId) = makeId r s

assignStgAlg (r :< Lambda vs bs a, s) = result ::< Lambda vs (map (fmap (,Nothing)) bs) (a, Just myId) where
    (result, myId) = makeId r s

assignStgAlg (r :< RnZ a b c, s) = result ::< RnZ (a,Just myId) (b,Just $ tmpId) (map (,Nothing) c) where
    (result, myId) = makeId r s
    tmpId = "tmp_" ++ show (getNodeId r)

assignStgAlg (r :< ZipWithN a b, s) = result ::< ZipWithN (a,Just (myId ++ idx)) (map (,Nothing) b) where
    (result, myId) = makeId r s
    idx = "[" ++ makeHofIdx r ++ "]"

assignStgAlg (r :< Flip a b, s) = Std x y False ::< Flip a (b,Nothing) where
    Std x y _ = fst $ makeId r s

assignStgAlg (r :< Subdiv a b c, s) = Std x y False ::< Subdiv a b (c,Nothing) where
    Std x y _ = fst $ makeId r s

assignStgAlg (r :< Flatten a b, s) = Std x y False ::< Flatten a (b,Nothing) where
    Std x y _ = fst $ makeId r s

makeId :: NodeId ∈ fields => R fields -> Maybe String -> (Result, String)
makeId (getNodeId -> nodeId) inherit = (Std myId (isJust inherit) (isNothing inherit), myId) where
    myId = fromMaybe ("v_" ++ show nodeId) inherit

makeIdLeaf :: String -> Maybe String -> Result
makeIdLeaf name inherit = Std (fromMaybe name inherit) (isJust inherit) False

makeHofIdx :: NodeId ∈ fields => R fields -> String
makeHofIdx r = "idx_" ++ show (getNodeId r)

assignStorage :: (NodeId ∈ fields, ParData ∈ fields) => Expr fields -> Expr (Result ': fields)
assignStorage e = ana (annotateAna assignStgAlg) (e,Just "result")

dims :: [Int] -> Int -> String
dims d 1 = concatMap (("," ++) . show) d
dims d tn = "," ++ show tn ++ dims d 1

strides :: MemStruct -> Int -> String
strides (_,s) 1 = "std::array<size_t," ++ show (length s) ++ ">{" ++ intercalate "," (map show s) ++ "}"
strides (d,s) _ = strides (d,product d : s) 1

viewType :: MemStruct -> Int -> String -> (String,String)
viewType m@(d,_) tn ptrType = ("View<" ++ ptrType ++ ",double" ++ dims d tn ++ ">", strides m tn)

newtype ResultPack = ResultPack ([ResultStg], [BigVector]) deriving Show -- collect vecView dimensions to allocate buffers for user data
data ResultStg = ResultStg { getName :: String, getThreadNum :: Int, getMem :: MemStruct } deriving (Eq, Show, Ord)
data BigVector = BigVector { getName :: String, getDataId :: String, getMem :: MemStruct } deriving (Eq, Show, Ord)
type MemStruct = ([Int], [Int]) -- (extents, strides)

memStruct :: TypecheckT ∈ fields => R (fields) -> MemStruct
memStruct (getType -> FDouble)    = ([1],[1])
memStruct (getType -> FPower _ a) = unzip a
memStruct _                       = ([],[])

merge :: Ord a => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys)
    | x < y  = x : merge xs (y:ys)
    | x > y  = y : merge (x:xs) ys
    | x == y = x : merge xs ys

instance Semigroup ResultPack where
    (ResultPack (a, b)) <> (ResultPack (c, d)) = ResultPack (a ++ c, b ++ d)
instance Monoid ResultPack where
    mempty = ResultPack ([], [])

collectStgAlg :: (NodeId ∈ fields, Result ∈ fields, ParData ∈ fields, TypecheckT ∈ fields) => Algebra (Cofree ExprF (R fields)) ResultPack

collectStgAlg ((fieldVal -> Std name _ False) ::< View dataId d s) = ResultPack([], [BigVector name dataId (d,s)])

collectStgAlg (r ::< node@RnZ{}) = getStgAndDims r <> ResultPack([tmpResult], []) <> fold node where
    tmpResult = ResultStg ("tmp_" ++ show (getNodeId r)) (fst $ getParData r) (memStruct r)

-- The memory layout of Flip and Subdiv is a modified version of their child's
-- collectStgAlg (r@(fieldVal -> Std (x,True)) ::< node@(Flip (i,j) (ResultPack ((ResultStg _ _ (m1,m2):_), _)))) =
--     ResultPack ([ResultStg x (fst $ getParData r) (swap i j m1, swap i j m2)], []) <> fold node

-- collectStgAlg (r@(fieldVal -> Std (x,True)) ::< node@(Subdiv i b (ResultPack ((ResultStg _ _ m:_), _)))) =
--     ResultPack ([ResultStg x (fst $ getParData r) (subdivMem i b m)], []) <> fold node

collectStgAlg (r ::< node) = getStgAndDims r <> fold node

getStgAndDims :: (Result ∈ fields, ParData ∈ fields, TypecheckT ∈ fields) => R (fields) -> ResultPack
getStgAndDims r@(fieldVal -> Std x _ True) = ResultPack ([ResultStg x (fst $ getParData r) (memStruct r)], [])
getStgAndDims _ = ResultPack ([], [])

collectStorage :: (NodeId ∈ fields, Result ∈ fields, ParData ∈ fields, TypecheckT ∈ fields) => Expr fields -> Expr (ResultPack ': fields)
collectStorage expr = (rput (Identity $ returnResult <> p) r :< node)  where
    (r@(fieldVal -> p@ResultPack{}) :< node) = cata (annotate collectStgAlg) expr
    returnResult = ResultPack ([ResultStg "result" 1 (memStruct $ extract expr)],[])
