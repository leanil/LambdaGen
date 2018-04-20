{-# LANGUAGE DataKinds, DeriveFunctor, PatternSynonyms, TypeOperators, ViewPatterns #-}

module Replace where

import Expr
import Recursion
import Control.Comonad.Cofree (Cofree ((:<)), unwrap)
import Data.Functor.Foldable
import qualified Data.Map.Strict as M
import Data.Vinyl

data PExprF a
    = PScalar String
    | PScalarOp Char a a
    | PView String
    | PApply String a
    | PLambda String a
    | PVariable String
    | PRnZ String a a
    | PZipWithN String a
    | PFlip String a
    | PSubdiv String a
    | PStar String
    deriving (Eq, Functor, Show)

type PExpr = Fix PExprF

pattern MScl n          = Fix (PScalar n)
pattern MSclOp a b c    = Fix (PScalarOp a b c)
pattern MAdd a b        = MSclOp '+' a b
pattern MMul a b        = MSclOp '*' a b
pattern MView n         = Fix (PView n)
pattern MApp n lam      = Fix (PApply n lam)
pattern MLam n a        = Fix (PLambda n a)
pattern MVar n          = Fix (PVariable n)
pattern MRnZ n red zip  = Fix (PRnZ n red zip)
pattern MZipWithN n lam = Fix (PZipWithN n lam)
pattern MFlip n a       = Fix (PFlip n a)
pattern MSubdiv n a     = Fix (PSubdiv n a)
pattern MStar id        = Fix (PStar id)

-- data MatchVal
--     = Expr { getExpr  :: Expr0 }
--     | Node { getNode  :: ExprF () }
    
type MatchVal = Expr0
type Match = M.Map String MatchVal

mGetExpr :: Ord k => M.Map k MatchVal -> k -> Expr0
mGetExpr match key = match M.! key

mGetNode :: Ord k => M.Map k MatchVal -> k -> ExprF Expr0
mGetNode match key = unwrap $ match M.! key

mSaveNode :: String -> ExprF Expr0 -> Match
mSaveNode key node = M.singleton key (RNil :< node)

makeComp :: (Expr0, PExpr) -> Either (Maybe Match) (CofreeF ExprF Match (Expr0, PExpr))
makeComp (_ :< n@Scalar{}, MScl i)                        = Right $ mSaveNode i n ::< castLeaf n
makeComp (_ :< n@(ScalarOp a _ _), MSclOp b c d) | a == b = Right $ M.empty ::< zipExprF n [c,d]
makeComp (_ :< n@View{}, MView i)                         = Right $ mSaveNode i n ::< castLeaf n
makeComp (_ :< n@Apply{}, MApp i a)                     = Right $ mSaveNode i n ::< zipExprF n [a]
makeComp (_ :< n@(Lambda _ _ a), MLam i b)                      = Right $ mSaveNode i n ::< Lambda [] [] (a,b)
makeComp (_ :< n@Variable{}, MVar i)                    = Right $ mSaveNode i n ::< castLeaf n
makeComp (_ :< n@RnZ{}, MRnZ i a b)                          = Right $ mSaveNode i n ::< zipExprF n [a,b]
makeComp (_ :< n@ZipWithN{}, MZipWithN i a)                  = Right $ mSaveNode i n ::< zipExprF n [a]
makeComp (_ :< n@Flip{}, MFlip i a)                          = Right $ mSaveNode i n ::< zipExprF n [a]
makeComp (_ :< n@Subdiv{}, MSubdiv i a)                      = Right $ mSaveNode i n ::< zipExprF n [a]
makeComp (subTree, MStar i)                                  = Left $ Just $ M.singleton i subTree
makeComp _                                                    = Left Nothing

evalComp :: CofreeF ExprF Match (Maybe Match) -> Maybe Match
evalComp (m ::< node) = foldr maybeUnion (Just m) node where
    maybeUnion (Just a) (Just b) = Just $ M.union a b
    maybeUnion _ _ = Nothing
    
fillReplacement :: Match -> PExpr -> Expr0
fillReplacement match = cata alg where
    alg :: Algebra PExpr Expr0
    alg (PScalar n)           = RNil :< castLeaf (mGetNode match n)
    alg (PScalarOp o a b)       = RNil :< ScalarOp o a b
    alg (PView n)       = RNil :< castLeaf (mGetNode match n)
    alg (PApply n a)          = RNil :< (mGetNode match n) { getLambda = a}
    alg (PLambda n a)         = RNil :< (mGetNode match n) { getLambdaBody = a}
    alg (PVariable n)         = RNil :< castLeaf (mGetNode match n)
    alg (PRnZ n r z)         = RNil :< (mGetNode match n) { getReducer=r, getZipper=z }
    alg (PZipWithN n l)    = RNil :< (mGetNode match n) { getLambda = l}
    alg (PStar n)             = match M.! n

-- Maybe a single (Maybe Match -> Maybe Match) function would be better than constraint + transform
type RepConstraint = (Match -> Bool)
type RepTransform = (Match -> Match)
replaceAll :: PExpr -> PExpr -> RepConstraint -> RepTransform -> Expr fields -> Expr0
replaceAll pat rep constraint transform = cata replace where
    replace :: Algebra (Expr fields) Expr0
    replace (_ ::< node) =
        case elgot evalComp makeComp (RNil :< node, pat) of
            (Just m@(constraint -> True)) -> fillReplacement (transform m) rep
            _ -> RNil :< node
