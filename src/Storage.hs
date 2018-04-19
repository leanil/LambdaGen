{-# LANGUAGE DataKinds, DuplicateRecordFields, FlexibleContexts, TupleSections, TypeOperators, ViewPatterns #-}

module Storage where

import Expr
import Metrics
import Parallel
import Recursion
import Type
import Typecheck
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree(..))
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import Data.Vinyl (rput, type (∈))
import Data.Vinyl.Functor (Identity(..))
import Data.Foldable (fold)
import Data.Functor.Foldable (ana, cata)

type AssignStgT = Maybe String -- (parent storage if the child can use it)
data Result = Std (String,Bool) | WithTmp (String, String, Bool) deriving Show -- (storage id, is new allocation)

getPrimary :: Result -> String
getPrimary (Std (x,_)) = x
getPrimary (WithTmp (x,_,_)) = x

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
    -- (s'@(Std x), [g',g2]) = assignHelper s 2
    -- (y, g1) = next g'
    -- mkResult (snd . getParData -> Just _) = WithTmp (x,Prealloc y)
    -- mkResult (snd . getParData -> Nothing) = s'

assignStgAlg (r :< ZipWithN a b, s) = result ::< ZipWithN (a,Just (myId ++ idx)) (map (,Nothing) b) where
    (result, myId) = makeId r s
    idx = "[" ++ makeHofIdx r ++ "]"

assignStgAlg (r :< Flip a b, s) = fst (makeId r s) ::< Flip a (b,Nothing)

assignStgAlg (r :< Subdiv a b c, s) = fst (makeId r s) ::< Subdiv a b (c,Nothing)

makeId :: NodeId ∈ fields => R fields -> Maybe String -> (Result, String)
makeId (getNodeId -> nodeId) inherit = (Std (myId, isNothing inherit), myId) where
    myId = fromMaybe ("v_" ++ show nodeId) inherit

makeIdLeaf :: String -> Maybe String -> Result
makeIdLeaf name inherit = Std (fromMaybe name inherit, isNothing inherit)

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

defaultMem :: TypecheckT ∈ fields => R (fields) -> MemStruct
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

collectStgAlg :: (NodeId ∈ fields, Result ∈ fields, ParData ∈ fields, TypecheckT ∈ fields) => Algebra (Cofree ExprF (R fields)) ResultPack

collectStgAlg ((fieldVal -> Std (name,_)) ::< View dataId d s) = ResultPack([], [BigVector name dataId (d,s)])

collectStgAlg (r ::< node@RnZ{}) = getStgAndDims r <> ResultPack([tmpResult], []) <> fold node where
    tmpResult = ResultStg ("tmp_" ++ show (getNodeId r)) (fst $ getParData r) (defaultMem r)

-- The memory layout of Flip and Subdiv is a modified version of their child's
collectStgAlg (r@(fieldVal -> Std (x,True)) ::< node@(Flip (i,j) (ResultPack ((ResultStg _ _ (m1,m2):_), _)))) =
    ResultPack ([ResultStg x (fst $ getParData r) (swap i j m1, swap i j m2)], []) <> fold node

collectStgAlg (r@(fieldVal -> Std (x,True)) ::< node@(Subdiv i b (ResultPack ((ResultStg _ _ m:_), _)))) =
    ResultPack ([ResultStg x (fst $ getParData r) (subdiv i b m)], []) <> fold node

-- Scalar, View and Variable nodes never allocate, but we can't set the Result to const False,
-- because it indicates whether they should assign.
collectStgAlg (r ::< node) = (if isLeafNode node then mempty else getStgAndDims r) <> fold node

getStgAndDims :: (Result ∈ fields, ParData ∈ fields, TypecheckT ∈ fields) => R (fields) -> ResultPack
getStgAndDims r@(fieldVal -> Std (x,True)) = ResultPack ([ResultStg x (fst $ getParData r) (defaultMem r)], [])
getStgAndDims _ = ResultPack ([], [])

collectStorage :: (NodeId ∈ fields, Result ∈ fields, ParData ∈ fields, TypecheckT ∈ fields) => Expr fields -> Expr (ResultPack ': fields)
collectStorage expr = (rput (Identity $ returnResult <> p) r :< node)  where
    (r@(fieldVal -> p@ResultPack{}) :< node) = cata (annotate collectStgAlg) expr
    returnResult = ResultPack ([ResultStg "result" 1 (defaultMem $ extract expr)],[])

subdiv :: Int -> Int -> ([Int],[Int]) -> ([Int],[Int])
subdiv i b (e,s) = (take i e ++ [div (e !! i) b, b] ++ drop (i+1) e, take i s ++ [b*(s !! i)] ++ drop i s)