{-# LANGUAGE DeriveFunctor #-}

module Replace where

import Expr
import Data.Functor.Foldable
import qualified Data.Map.Strict as M

data PExprF a
    = PFunc String
    | PAdd a a
    | PMul a a
    | PDiff a
    | PStar String -- The wildcard node.
    deriving Functor

type PExpr = Fix PExprF

pFunc s  = Fix $ PFunc s
pAdd a b = Fix $ PAdd a b
pMul a b = Fix $ PMul a b
pDiff a  = Fix $ PDiff a
pStar s  = Fix $ PStar s
    
type Match = M.Map String Expr

makeComp :: (Expr, PExpr) -> Either (Maybe Match) (ExprF (Expr, PExpr))
makeComp (Fix (Func s1), Fix (PFunc s2)) | s1 == s2 = Right $ Func s1
makeComp (Fix (Add a1 b1), Fix (PAdd a2 b2))        = Right $ Add (a1,a2) (b1,b2)
makeComp (Fix (Mul a1 b1), Fix (PMul a2 b2))        = Right $ Mul (a1,a2) (b1,b2)
makeComp (Fix (Diff a1), Fix (PDiff a2))            = Right $ Diff (a1,a2)
makeComp (subTree, Fix (PStar id))                  = Left $ Just $ M.singleton id subTree
makeComp _                                          = Left Nothing

evalComp :: ExprF (Maybe Match) -> Maybe Match
evalComp (Func _)                = Just M.empty
evalComp (Add (Just a) (Just b)) = Just $ M.union a b
evalComp (Mul (Just a) (Just b)) = Just $ M.union a b
evalComp (Diff a)                = a
evalComp _                       = Nothing
    
fillReplacement :: Match -> PExpr -> Expr
fillReplacement match = cata fillAlg where
    fillAlg (PFunc s)  = func s
    fillAlg (PAdd a b) = add a b
    fillAlg (PMul a b) = mul a b
    fillAlg (PDiff a)  = diff a
    fillAlg (PStar id) = match M.! id

replaceAll :: PExpr -> PExpr -> Expr -> Expr
replaceAll pat rep = cata replace where
    replace node =
        case elgot evalComp makeComp (Fix node, pat) of
            (Just m) -> fillReplacement m rep
            Nothing  -> Fix node