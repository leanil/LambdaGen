{-# LANGUAGE DataKinds, DeriveFunctor, PatternSynonyms, TypeOperators #-}

module Replace where

import Expr
import Recursion
import Type
import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Functor.Foldable
import qualified Data.Map.Strict as M
import Data.Vinyl

data PExprF a
    = PScalar String
    | PAddition a a
    | PMultiplication a a
    | PVectorView String
    | PApply a a
    | PLambda String a
    | PVariable String
    | PMap a a
    | PReduce a a
    | PZipWith a a a
    | PCompose a a
    | PStar String
    deriving (Eq, Functor, Show)

type PExpr = Fix PExprF

pattern FPScalar n           = Fix (PScalar n)
pattern FPAddition a b       = Fix (PAddition a b)
pattern FPMultiplication a b = Fix (PMultiplication a b)
pattern FPVectorView n       = Fix (PVectorView n)
pattern FPApply lam val      = Fix (PApply lam val)
pattern FPLambda n body      = Fix (PLambda n body)
pattern FPVariable n         = Fix (PVariable n)
pattern FPMap lam v          = Fix (PMap lam v)
pattern FPReduce lam v       = Fix (PReduce lam v)
pattern FPZipWith lam v1 v2  = Fix (PZipWith lam v1 v2)
pattern FPCompose a b        = Fix (PCompose a b)
pattern FPStar id            = Fix (PStar id)

pScl n           = Fix $ PScalar n
pAdd a b         = Fix $ PAddition a b
pMul a b         = Fix $ PMultiplication a b
pVecView n       = Fix $ PVectorView n
pApp l v         = Fix $ PApply l v
pLam n body      = Fix $ PLambda n body
pVar n           = Fix $ PVariable n
pMap l v         = Fix $ PMap l v
pReduce l v      = Fix $ PReduce l v
pZipWith l v1 v2 = Fix $ PZipWith l v1 v2
pComp a b        = Fix $ PCompose a b
pStar n          = Fix $ PStar n

data MatchVal
    = Expr { getExpr  :: Expr0 }
    | Node { getNode  :: ExprF () }
    
type Match = M.Map String MatchVal

mGetExpr match id = getExpr $ match M.! id
mGetNode match id = getNode $ match M.! id

mSaveNode :: String -> ExprF Expr0 -> Maybe Match
mSaveNode id n = Just $ M.singleton id $ Node $ fmap (const ()) n

makeComp :: (Expr0, PExpr) -> Either (Maybe Match) (CofreeF ExprF (Maybe Match) (Expr0, PExpr))
makeComp (_ :< n@Scalar{}, FPScalar id)                  = Right $ mSaveNode id n ::< castLeaf n
makeComp (_ :< n@Addition{}, FPAddition c d)             = Right $ Nothing ::< zipExprF (,) n [c,d]
makeComp (_ :< n@Multiplication{}, FPMultiplication c d) = Right $ Nothing ::< zipExprF (,) n [c,d]
makeComp (_ :< n@VectorView{}, FPVectorView id)          = Right $ mSaveNode id n ::< castLeaf n
makeComp (_ :< n@Apply{}, FPApply c d)                   = Right $ Nothing ::< zipExprF (,) n [c,d]
makeComp (_ :< n@Lambda{}, FPLambda id d)                = Right $ mSaveNode id n ::< zipExprF (,) n [d]
makeComp (_ :< n@Variable{}, FPVariable id)              = Right $ mSaveNode id n ::< castLeaf n
makeComp (_ :< n@Map{}, FPMap c d)                       = Right $ Nothing ::< zipExprF (,) n [c,d]
makeComp (_ :< n@Reduce{}, FPReduce c d)                 = Right $ Nothing ::< zipExprF (,) n [c,d]
makeComp (_ :< n@ZipWith{}, FPZipWith d e f)             = Right $ Nothing ::< zipExprF (,) n [d,e,f]
makeComp (_ :< n@Compose{}, FPCompose c d)               = Right $ Nothing ::< zipExprF (,) n [c,d]
makeComp (subTree, FPStar id)                            = Left $ Just $ M.singleton id (Expr subTree)
makeComp _                                               = Left Nothing

evalComp :: CofreeF ExprF (Maybe Match) (Maybe Match) -> Maybe Match
evalComp (m ::< Scalar{})                           = m
evalComp (_ ::< Addition (Just a) (Just b))         = Just $ M.union a b
evalComp (_ ::< Multiplication (Just a) (Just b))   = Just $ M.union a b
evalComp (m ::< VectorView{})                       = m
evalComp (_ ::< Apply (Just a) (Just b))            = Just $ M.union a b
evalComp (Just m ::< Lambda _ _ (Just c))           = Just $ M.union m c
evalComp (m ::< Variable{})                         = m
evalComp (_ ::< Map (Just a) (Just b))              = Just $ M.union a b
evalComp (_ ::< Reduce (Just a) (Just b))           = Just $ M.union a b
evalComp (_ ::< ZipWith (Just a) (Just b) (Just c)) = Just $ M.unions [a,b,c]
evalComp (_ ::< Compose (Just a) (Just b))          = Just $ M.union a b
evalComp _                                          = Nothing
    
fillReplacement :: Match -> PExpr -> Expr0
fillReplacement match = cata alg where
    alg :: Algebra PExpr Expr0
    alg (PScalar n)           = RNil :< castLeaf (mGetNode match n)
    alg (PAddition a b)       = RNil :< Addition a b
    alg (PMultiplication a b) = RNil :< Multiplication a b
    alg (PVectorView n)       = RNil :< castLeaf (mGetNode match n)
    alg (PApply a b)          = RNil :< Apply a b
    alg (PLambda n a)         = RNil :< zipExprF (flip const) (mGetNode match n) [a]
    alg (PVariable n)         = RNil :< castLeaf (mGetNode match n)
    alg (PMap l v)            = RNil :< Map l v
    alg (PReduce l v)         = RNil :< Reduce l v
    alg (PZipWith l v1 v2)    = RNil :< ZipWith l v1 v2
    alg (PCompose a b)        = RNil :< Compose a b
    alg (PStar id)            = mGetExpr match id

replaceAll :: PExpr -> PExpr -> Expr fields -> Expr0
replaceAll pat rep = cata replace where
    replace :: Algebra (Expr fields) Expr0
    replace (_ ::< node) =
        case elgot evalComp makeComp (RNil :< node, pat) of
            (Just m) -> fillReplacement m rep
            Nothing  -> RNil :< node
