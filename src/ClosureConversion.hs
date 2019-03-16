{-# LANGUAGE DataKinds, FlexibleContexts, TupleSections, TypeOperators, ViewPatterns #-}

module ClosureConversion where 

import Expr
import Metrics
import Recursion
import Type
import Typecheck
import Control.Comonad.Cofree (Cofree(..))
import Control.Monad.State (State, evalState, get, modify)
import Data.Foldable (fold)
import Data.Functor.Foldable (ana, cata)
import Data.Map.Strict (Map, empty, findWithDefault, insert, keysSet, union, singleton, toList, withoutKeys)
import Data.Monoid (mconcat)
import qualified Data.Set as S (Set, empty, fromList, member)
import Data.Vinyl (type (∈))

data ClosureT = ClosureT { getClosure :: Map String Type, getClosureList :: [(String, Map String Type)] }
instance Show ClosureT where 
    show (ClosureT symbolMap closureList) = "ClosureT " ++ show closureList -- ++ show (toList symbolMap)

instance Semigroup ClosureT where
    (ClosureT clA listA) <> (ClosureT clB listB) = ClosureT (union clA clB) (listA ++ listB)
instance Monoid ClosureT where 
    mempty = ClosureT empty []

closureConvAlg :: (TypecheckT ∈ fields, NodeId ∈ fields) => Algebra (Expr fields) ClosureT

closureConvAlg (_ ::< Variable name ty) = ClosureT (singleton name ty) []

closureConvAlg (r ::< node@(Lambda params bindings body)) = ClosureT filtered closure where
    closure = ("_cl" ++ show (getNodeId r), filtered) : getClosureList merged
    filtered = withoutKeys (getClosure merged) $ S.fromList (map fst params ++ map fst bindings)
    merged = fold node

closureConvAlg (_ ::< node) = fold node

type BoundNestedFun = Map String [Int]
type Callee = (Maybe Int, [Int])

calleeAlg :: (NodeId ∈ fields, LetId ∈ fields) => MAlgebra (Expr fields) (State BoundNestedFun) Callee
calleeAlg (r ::< node) = do
    st <- get
    let retval = case node of
                    Apply (_,x:xs) _ -> (Just x, xs)
                    Lambda _ _ (_,fs) -> (Nothing, getNodeId r : fs)
                    Variable name _ -> (Nothing, findWithDefault [] name st)
                    _ -> (Nothing, [])
        mod = case (fieldVal r, retval) of
                    (LetId (Just name), (_,xs)) -> insert name xs 
                    _ -> id in do
        modify mod
        return retval

newtype IsFreeVar = IsFreeVar Bool deriving Show

isFreeVarAlg :: ClosureT ∈ fields => CoAlgebra (Cofree ExprF IsFreeVar) (Expr fields, S.Set String)

isFreeVarAlg (_ :< Variable name ty, closure) = IsFreeVar (S.member name closure) ::< Variable name ty

isFreeVarAlg (r :< node, closure) = IsFreeVar False ::< fmap (,newClosure) node where 
    newClosure = case node of
        Lambda{} -> extractClosure r
        otherwise -> closure
    extractClosure (fieldVal -> ClosureT clMap _) = keysSet clMap

closureConversion :: (TypecheckT ∈ fields, NodeId ∈ fields, LetId ∈ fields) => Expr fields -> Expr (IsFreeVar ': Callee ': ClosureT ': fields)
closureConversion = ana (annotateAna isFreeVarAlg) . (,S.empty) . (`evalState` empty) . cataM (annotateM calleeAlg) . cata (annotate closureConvAlg)