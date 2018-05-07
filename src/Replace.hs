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
    | PScalarOp String Char a a
    | PView String
    | PApply String a
    | PLambda String a
    | PVariable String
    | PRnZ String a a
    | PZipWithN String a
    | PFlip String (Int,Int) a
    | PSubdiv String Int Int a
    | PStar String
    deriving (Eq, Functor, Show)

type PExpr = Fix PExprF

pattern MScl n          = Fix (PScalar n)
pattern MSclOp n a b c  = Fix (PScalarOp n a b c)
pattern MAdd n a b      = MSclOp n '+' a b
pattern MMul n a b      = MSclOp n '*' a b
pattern MView n         = Fix (PView n)
pattern MApp n lam      = Fix (PApply n lam)
pattern MLam n a        = Fix (PLambda n a)
pattern MVar n          = Fix (PVariable n)
pattern MRnZ n red zip  = Fix (PRnZ n red zip)
pattern MZipWithN n lam = Fix (PZipWithN n lam)
pattern MFlip n a b     = Fix (PFlip n a b)
pattern MSubdiv n a b c = Fix (PSubdiv n a b c)
pattern MStar id        = Fix (PStar id)
    
type Match fields = M.Map String (Expr fields)

mGetExpr :: Ord k => M.Map k (Expr fields) -> k -> Expr fields
mGetExpr match key = match M.! key

mGetNode :: Ord k => M.Map k (Expr fields) -> k -> ExprF (Expr fields)
mGetNode match key = unwrap $ match M.! key

makeComp :: (Expr fields, PExpr) -> Either (Maybe (Match fields)) (CofreeF ExprF (Match fields) (Expr fields, PExpr))
makeComp (n@(_ :< m@Scalar{}), MScl i)                          = Right $ M.singleton i n ::< castLeaf m
makeComp (n@(_ :< m@(ScalarOp a _ _)), MSclOp i b c d) | a == b = Right $ M.singleton i n ::< zipExprF m [c,d]
makeComp (n@(_ :< m@View{}), MView i)                           = Right $ M.singleton i n ::< castLeaf m
makeComp (n@(_ :< m@Apply{}), MApp i a)                         = Right $ M.singleton i n ::< zipExprF m [a]
makeComp (n@(_ :< (Lambda _ _ a)), MLam i b)                    = Right $ M.singleton i n ::< Lambda [] [] (a,b)
makeComp (n@(_ :< m@Variable{}), MVar i)                        = Right $ M.singleton i n ::< castLeaf m
makeComp (n@(_ :< m@RnZ{}), MRnZ i a b)                         = Right $ M.singleton i n ::< zipExprF m [a,b]
makeComp (n@(_ :< m@ZipWithN{}), MZipWithN i a)                 = Right $ M.singleton i n ::< zipExprF m [a]
makeComp (n@(_ :< m@Flip{}), MFlip i _ a)                       = Right $ M.singleton i n ::< zipExprF m [a]
makeComp (n@(_ :< m@Subdiv{}), MSubdiv i _ _ a)                 = Right $ M.singleton i n ::< zipExprF m [a]
makeComp (subTree, MStar i)                                     = Left $ Just $ M.singleton i subTree
makeComp _                                                      = Left Nothing
  
evalComp :: CofreeF ExprF (Match fields) (Maybe (Match fields)) -> Maybe (Match fields)
evalComp (m ::< node) = foldr maybeUnion (Just m) node where
    maybeUnion (Just a) (Just b) = Just $ M.union a b
    maybeUnion _ _ = Nothing
    
stripExpr :: Expr fields -> ExprF Expr0
stripExpr = unwrap . cata (\(_ ::< node) -> (RNil :< node))

fillReplacement :: Match fields -> PExpr -> Expr0
fillReplacement match = cata alg where
    alg :: Algebra PExpr Expr0
    alg (PScalar n)         = RNil :< castLeaf (mGetNode match n)
    alg (PScalarOp _ o a b) = RNil :< ScalarOp o a b
    alg (PView n)           = RNil :< castLeaf (mGetNode match n)
    alg (PApply n a)        = RNil :< (stripExpr $ mGetExpr match n) { getLambda = a}
    alg (PLambda n a)       = RNil :< (stripExpr $ mGetExpr match n) { getLambdaBody = a}
    alg (PVariable n)       = RNil :< castLeaf (mGetNode match n)
    alg (PRnZ n r z)        = RNil :< (stripExpr $ mGetExpr match n) { getReducer=r, getZipper=z }
    alg (PZipWithN n l)     = RNil :< (stripExpr $ mGetExpr match n) { getLambda = l}
    alg (PFlip _ a b)       = RNil :< Flip a b
    alg (PSubdiv _ a b c)   = RNil :< Subdiv a b c
    alg (PStar n)           = RNil :< stripExpr (match M.! n)
  
type RepPattern = (PExpr,PExpr)
type RepTransform fields = (Match fields -> Maybe (Match fields))

replaceAll :: RepPattern -> RepTransform '[] -> Expr fields -> Expr0
replaceAll (pat,rep) transform = cata replace where
    replace :: Algebra (Expr fields) Expr0
    replace (_ ::< node) =
        case elgot evalComp makeComp (RNil :< node, pat) >>= transform of
            (Just m) -> fillReplacement m rep
            _ -> RNil :< node

replace1TopDown :: RepPattern -> RepTransform fields -> Expr fields -> Expr0
replace1TopDown (pat,rep) transform = elgot rebuild match where
    rebuild = (RNil :<)
    match expr =
        case elgot evalComp makeComp (expr, pat) >>= transform of
            (Just m) -> Left $ fillReplacement m rep
            _ -> Right $ unwrap expr
