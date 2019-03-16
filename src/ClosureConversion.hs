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
import Data.Map.Strict (Map, empty, findWithDefault, fromList, keysSet, union, singleton, toList, withoutKeys)
import Data.Monoid (mconcat)
import qualified Data.Set as S (Set, empty, fromList, member)
import Data.Vinyl (type (∈))

import Debug.Trace

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

traceMonad :: (Show a, Monad m) => String -> a -> m a
traceMonad msg x = trace (msg ++ ": " ++ show x) (return x)

calleeAlg :: NodeId ∈ fields => MAlgebra (Expr fields) (State BoundNestedFun) Callee
calleeAlg (_ ::< Apply (_,x:xs) _) = return (Just x, xs)
calleeAlg (r ::< Lambda _ binds (_,fs)) = do
        x <- get
        traceMonad ("lam1 " ++ show (getNodeId r)) x
        modify (union $ fromList $ map (fmap snd) binds)
        x <- get
        traceMonad  ("lam2 " ++ show (getNodeId r)) x
        return (Nothing, getNodeId r : fs)
calleeAlg (r ::< Variable name _) = do
    x <- get
    traceMonad  ("var " ++ show (getNodeId r)) x
    funMap <- get
    return (Nothing, findWithDefault [] name funMap)
calleeAlg _ = return (Nothing, [])

newtype IsFreeVar = IsFreeVar Bool deriving Show

isFreeVarAlg :: ClosureT ∈ fields => CoAlgebra (Cofree ExprF IsFreeVar) (Expr fields, S.Set String)

isFreeVarAlg (_ :< Variable name ty, closure) = IsFreeVar (S.member name closure) ::< Variable name ty

isFreeVarAlg (r :< node, closure) = IsFreeVar False ::< fmap (,newClosure) node where 
    newClosure = case node of
        Lambda{} -> extractClosure r
        otherwise -> closure
    extractClosure (fieldVal -> ClosureT clMap _) = keysSet clMap

closureConversion :: (TypecheckT ∈ fields, NodeId ∈ fields) => Expr fields -> Expr (IsFreeVar ': Callee ': ClosureT ': fields)
closureConversion = ana (annotateAna isFreeVarAlg) . (,S.empty) . (`evalState` empty) . cataM (annotateM calleeAlg) . cata (annotate closureConvAlg)