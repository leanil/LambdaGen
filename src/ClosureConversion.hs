{-# LANGUAGE DataKinds, FlexibleContexts, ScopedTypeVariables, TupleSections, TypeOperators, ViewPatterns #-}

module ClosureConversion where 

import Expr
import Metrics
import Recursion
import Type hiding (size)
import Typecheck
import Control.Comonad.Cofree (Cofree(..))
import Control.Monad.State (State, evalState, get, modify, runState)
import Data.Foldable (fold)
import Data.Functor.Foldable (ana, cata)
import Data.Map.Strict (Map, (!?), empty, findWithDefault, insert, keysSet, union, singleton, size, toList, withoutKeys)
import Data.Monoid (mconcat)
import Data.Set (Set, fromList, member)
import qualified Data.Set as S (empty, singleton, union)
import Data.Vinyl (type (∈))

type BoundNestedFun = Map String [Int]
data Callee = Callee { getCallee :: [Int], getFunStack :: [Int] } deriving Show

calleeAlg :: (NodeId ∈ fields, LetId ∈ fields) => MAlgebra (Expr fields) (State BoundNestedFun) Callee
calleeAlg (r ::< node) = do
    st <- get
    let retval = case node of
                    Apply (Callee _ (x:xs)) _ -> Callee [x] xs
                    Lambda _ _ (Callee _ fs) -> Callee [] $ getNodeId r : fs
                    Variable name _ -> Callee [] $ findWithDefault [] name st
                    RnZ (Callee _ [x]) (Callee _ [y]) _ -> Callee [x,y] []
                    ZipWithN (Callee _ [x]) _ -> Callee [x] []
                    _ -> Callee [] []
        mod = case (fieldVal r, retval) of
                    (LetId (Just name), Callee _ x@(_:_)) -> insert name x 
                    _ -> id in do
        modify mod
        return retval

-- Id of a lambda or type of a tensor
type ExType = Either Int Type
data ClosureT = ClosureT { getClosure :: Map String ExType, getClosureList :: [(Int, Map String ExType)] }
instance Show ClosureT where show (ClosureT symbolMap closureList) = "ClosureT " ++ show closureList -- ++ show (toList symbolMap)
instance Semigroup ClosureT where (ClosureT clA listA) <> (ClosureT clB listB) = ClosureT (union clA clB) (listA ++ listB)
instance Monoid ClosureT where mempty = ClosureT empty []

closureConvAlg :: (TypecheckT ∈ fields, NodeId ∈ fields, Callee ∈ fields) => Algebra (Expr fields) ClosureT

closureConvAlg ((getFunStack . fieldVal -> x:_) ::< Variable name FArrow{}) = ClosureT (singleton name $ Left x) []
closureConvAlg (_ ::< Variable name ty) = ClosureT (singleton name $ Right ty) []

closureConvAlg (r ::< node@(Lambda params bindings body)) = ClosureT filtered closure where
    closure = (getNodeId r, filtered) : getClosureList merged
    filtered = withoutKeys (getClosure merged) $ fromList (map fst params ++ map fst bindings)
    merged = fold node

closureConvAlg (_ ::< node) = fold node

getExType :: (TypecheckT ∈ fields, Callee ∈ fields) => R fields -> ExType
getExType r = case (getType r) of
    FArrow{}  -> Left $ head $ getFunStack $ fieldVal r
    _         -> Right $ getType r

newtype IsFreeVar = IsFreeVar { isFreeVar :: Bool } deriving Show

isFreeVarAlg :: ClosureT ∈ fields => CoAlgebra (Cofree ExprF IsFreeVar) (Expr fields, Set String)

isFreeVarAlg (_ :< Variable name ty, closure) = IsFreeVar (member name closure) ::< Variable name ty

isFreeVarAlg (r :< node, closure) = IsFreeVar False ::< fmap (,newClosure) node where 
    newClosure = case node of
        Lambda{} -> extractClosure r
        _        -> closure
    extractClosure (fieldVal -> ClosureT clMap _) = keysSet clMap

-- | The set of names bound in the directly enclosing lambda.
newtype ParamSet = ParamSet (Set String) deriving Show

paramSetAlg :: CoAlgebra (Cofree ExprF ParamSet) (Expr fields, ParamSet)
paramSetAlg (_ :< node, paramSet) = paramSet ::< fmap (,paramSet') node where
    paramSet' = case node of
        Lambda params binds _ -> ParamSet (fromList $ map fst params ++ map fst binds)
        _                     -> paramSet

getParamSet :: ParamSet ∈ fields => R fields -> Set String
getParamSet (fieldVal -> ParamSet params) = params

newtype HofSpecId = HofSpecId { getHofSpec :: Maybe Int }
data HofSpec = HofSpec { rnzSpec :: Map (Int,Int) (Type,Int), zipSpec :: Map Int (Type,Int) }
instance Semigroup HofSpec where (<>) (HofSpec ra za) (HofSpec rb zb) = HofSpec (union ra rb) (union za zb)
instance Monoid HofSpec where mempty = HofSpec empty empty

hofSpecAlg :: (TypecheckT ∈ fields, Callee ∈ fields) => MAlgebra (Expr fields) (State HofSpec) HofSpecId
hofSpecAlg (r ::< node) = do
    (HofSpec rs zs) <- get
    let (specId, update) = case (getCallee $ fieldVal r, node) of
                           ([x,y], RnZ{})    -> helper rs (x,y) (flip HofSpec $ empty)
                           ([x], ZipWithN{}) -> helper zs x (HofSpec empty)
                           _                 -> (Nothing, mempty)
                           where helper spec key embed = case spec !? key of
                                    Just (_,specId) -> (Just specId, embed empty)
                                    Nothing -> (Just specId, embed $ singleton key (getType r,specId)) where 
                                        specId = size spec
    modify $ mappend $ update
    return $ HofSpecId $ specId

hofSpec :: (TypecheckT ∈ fields, Callee ∈ fields) => Expr fields -> (Expr (HofSpecId ': fields), HofSpec)
hofSpec = (`runState` mempty) . cataM (annotateM hofSpecAlg)

closureConversion :: (TypecheckT ∈ fields, NodeId ∈ fields, LetId ∈ fields) => Expr fields -> (Expr (HofSpecId ': ParamSet ': IsFreeVar ': ClosureT ': Callee ': fields), HofSpec)
closureConversion = hofSpec .
                    ana (annotateAna paramSetAlg) . (,ParamSet S.empty) .
                    ana (annotateAna isFreeVarAlg) . (,S.empty) . 
                    cata (annotate closureConvAlg) .
                    (`evalState` empty) . cataM (annotateM calleeAlg)
                    