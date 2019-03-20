{-# LANGUAGE DataKinds, FlexibleContexts, ScopedTypeVariables, TupleSections, TypeOperators, ViewPatterns #-}

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

type BoundNestedFun = Map String [Int]
data Callee = Callee { getCallee :: Maybe Int, getFunStack :: [Int] } deriving Show

calleeAlg :: (NodeId ∈ fields, LetId ∈ fields) => MAlgebra (Expr fields) (State BoundNestedFun) Callee
calleeAlg (r ::< node) = do
    st <- get
    let retval = case node of
                    Apply (Callee _ (x:xs)) _ -> Callee (Just x) xs
                    Lambda _ _ (Callee _ fs) -> Callee Nothing $ getNodeId r : fs
                    Variable name _ -> Callee Nothing $ findWithDefault [] name st
                    _ -> Callee Nothing []
        mod = case (fieldVal r, retval) of
                    (LetId (Just name), Callee _ xs) -> insert name xs 
                    _ -> id in do
        modify mod
        return retval

-- Id of a lambda or type of a tensor
type ExType = Either Int Type
data ClosureT = ClosureT { getClosure :: Map String ExType, getClosureList :: [(Int, Map String ExType)] }

instance Show ClosureT where 
    show (ClosureT symbolMap closureList) = "ClosureT " ++ show closureList -- ++ show (toList symbolMap)

instance Semigroup ClosureT where
    (ClosureT clA listA) <> (ClosureT clB listB) = ClosureT (union clA clB) (listA ++ listB)
instance Monoid ClosureT where 
    mempty = ClosureT empty []

closureConvAlg :: (TypecheckT ∈ fields, NodeId ∈ fields, Callee ∈ fields) => Algebra (Expr fields) ClosureT

closureConvAlg ((getFunStack . fieldVal -> x:_) ::< Variable name FArrow{}) = ClosureT (singleton name $ Left x) []
closureConvAlg (_ ::< Variable name ty) = ClosureT (singleton name $ Right ty) []

closureConvAlg (r ::< node@(Lambda params bindings body)) = ClosureT filtered closure where
    closure = (getNodeId r, filtered) : getClosureList merged
    filtered = withoutKeys (getClosure merged) $ S.fromList (map fst params ++ map fst bindings)
    merged = fold node

closureConvAlg (_ ::< node) = fold node

getExType :: (TypecheckT ∈ fields, Callee ∈ fields) => R fields -> ExType
getExType r = case (getType r) of
    FArrow{}  -> Left $ head $ getFunStack $ fieldVal r
    otherwise -> Right $ getType r

newtype IsFreeVar = IsFreeVar Bool deriving Show

isFreeVarAlg :: ClosureT ∈ fields => CoAlgebra (Cofree ExprF IsFreeVar) (Expr fields, S.Set String)

isFreeVarAlg (_ :< Variable name ty, closure) = IsFreeVar (S.member name closure) ::< Variable name ty

isFreeVarAlg (r :< node, closure) = IsFreeVar False ::< fmap (,newClosure) node where 
    newClosure = case node of
        Lambda{} -> extractClosure r
        otherwise -> closure
    extractClosure (fieldVal -> ClosureT clMap _) = keysSet clMap

newtype ParamSet = ParamSet (S.Set String) deriving Show

paramSetAlg :: CoAlgebra (Cofree ExprF ParamSet) (Expr fields, ParamSet)
paramSetAlg (_ :< node, paramSet) = paramSet ::< fmap (,paramSet') node where
    paramSet' = case node of
        Lambda params binds _ -> ParamSet (S.fromList $ map fst params ++ map fst binds)
        otherwise             -> paramSet

getParamSet :: ParamSet ∈ fields => R fields -> S.Set String
getParamSet (fieldVal -> ParamSet params) = params

closureConversion :: (TypecheckT ∈ fields, NodeId ∈ fields, LetId ∈ fields) => Expr fields -> Expr (ParamSet ': IsFreeVar ': ClosureT ': Callee ': fields)
closureConversion = ana (annotateAna paramSetAlg) . (,ParamSet S.empty) .
                    ana (annotateAna isFreeVarAlg) . (,S.empty) . 
                    cata (annotate closureConvAlg) .
                    (`evalState` empty) . cataM (annotateM calleeAlg)
                    