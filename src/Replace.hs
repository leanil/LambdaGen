{-# LANGUAGE DataKinds, DeriveFunctor, PatternSynonyms, TypeOperators, ViewPatterns #-}

module Replace where

import Expr
import Recursion
import Control.Arrow ((&&&))
import Control.Comonad.Cofree (Cofree ((:<)), unwrap)
import Data.Functor.Foldable
import Data.Map.Strict (Map, (!), findWithDefault, insert, union, singleton)
import Data.Maybe (fromJust)
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
    | PFlip String a
    | PSubdiv String a
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
pattern MFlip n a       = Fix (PFlip n a)
pattern MSubdiv n a     = Fix (PSubdiv n a)
pattern MStar id        = Fix (PStar id)

type ExprOpt fields = Cofree ExprF (Maybe (R fields))

data MatchVal fields
    = Subtree { getSubtree  :: ExprOpt fields }
    | Args { getArgList  :: [ExprOpt fields] }
    | Node { getInfo :: Maybe (R fields), getNode  :: ExprF () }

instance Show (MatchVal fields) where
    show (Subtree (_ :< a)) = "Subtree " ++ show (fmap (const ()) a)
    show (Args a) = "Args " ++ show (map (fmap (const ()) . unwrap) a)
    show (Node _ a) = "Node " ++ show a
    
type Match fields = Map String (MatchVal fields)

mGetSubtree :: Match fields -> String -> ExprOpt fields
mGetSubtree match = getSubtree . (match !)

mGetArgList :: Match fields -> String -> [ExprOpt fields]
mGetArgList match key = getArgList $ match ! (key ++ "__args")

mGetNode :: Match fields -> String -> ExprF ()
mGetNode match = getNode . (match !)

getAnnotedNode :: Match fields -> String -> (R fields ,ExprF ())
getAnnotedNode match = ((fromJust . getInfo) &&& getNode) . (match !)

saveNode :: String -> Expr fields -> Match fields
saveNode key (info :< node) = singleton key $ Node (Just info) (fmap (const ()) node)

saveWithArgs :: String -> Expr fields -> [Expr fields] -> Match fields
saveWithArgs key expr args = insert (key ++ "__args") (Args $ map makeAnnotationOptional args) (saveNode key expr)

makeAnnotationOptional :: Expr fields -> ExprOpt fields
makeAnnotationOptional = cata (\(info ::< node) -> (Just info :< node))

makeComp :: (Expr fields, PExpr) -> Either (Maybe (Match fields)) (CofreeF ExprF (Match fields) (Expr fields, PExpr))
-- NOTE: save plain nodes as well, because we may need their annotation
makeComp (n@(_ :< m@(ScalarOp a _ _)), MSclOp i b c d) | a == b = Right $ saveNode i n ::< zipExprF m [c,d]
makeComp (n@(_ :< m@Flip{}), MFlip i a)                         = Right $ saveNode i n ::< zipExprF m [a]
makeComp (n@(_ :< m@Subdiv{}), MSubdiv i a)                     = Right $ saveNode i n ::< zipExprF m [a]
makeComp (n@(_ :< m@Scalar{}), MScl i)                          = Right $ saveNode i n ::< castLeaf m
makeComp (n@(_ :< m@View{}), MView i)                           = Right $ saveNode i n ::< castLeaf m
makeComp (n@(_ :< m@Variable{}), MVar i)                        = Right $ saveNode i n ::< castLeaf m
makeComp (n@(_ :< m@(Apply _ args)), MApp i a)                  = Right $ saveWithArgs i n args ::< zipExprF m [a]
makeComp (n@(_ :< (Lambda _ binds a)), MLam i b)                = Right $ saveWithArgs i n (map snd binds) ::< Lambda [] [] (a,b)
makeComp (n@(_ :< m@(RnZ _ _ args)), MRnZ i a b)                = Right $ saveWithArgs i n args ::< zipExprF m [a,b]
makeComp (n@(_ :< m@(ZipWithN _ args)), MZipWithN i a)          = Right $ saveWithArgs i n args ::< zipExprF m [a]
makeComp (subTree, MStar i)                                     = Left $ Just $ singleton i $ Subtree $ makeAnnotationOptional subTree
makeComp _                                                      = Left Nothing
  
evalComp :: CofreeF ExprF (Match fields) (Maybe (Match fields)) -> Maybe (Match fields)
evalComp (m ::< node) = foldr maybeUnion (Just m) node where
    maybeUnion (Just a) (Just b) = Just $ union a b
    maybeUnion _ _ = Nothing
    
stripExpr :: Cofree ExprF a -> Expr0
stripExpr = cata (\(_ ::< node) -> (RNil :< node))

fillReplacement :: Match fields -> PExpr -> Expr0
fillReplacement match = cata alg where
    alg :: Algebra PExpr Expr0
    alg (PScalar n)         = RNil :< castLeaf (mGetNode match n)
    alg (PView n)           = RNil :< castLeaf (mGetNode match n)
    alg (PVariable n)       = RNil :< castLeaf (mGetNode match n)
    -- NOTE: inserted plain nodes aren't stored in the match, so construct them instead of querying
    alg (PScalarOp _ o a b) = RNil :< ScalarOp o a b
    alg (PApply n a)        = RNil :< (Apply a $ map stripExpr $ mGetArgList match n)
    alg (PRnZ n r z)        = RNil :< (RnZ r z $ map stripExpr $ mGetArgList match n)
    alg (PZipWithN n l)     = RNil :< (ZipWithN l $ map stripExpr $ mGetArgList match n)
    alg (PFlip n a)         = RNil :< zipWithExprF (flip const) (mGetNode match n) [a]
    alg (PSubdiv n a)       = RNil :< zipWithExprF (flip const) (mGetNode match n) [a]
    alg (PLambda n a)       = RNil :< zipWithExprF (flip const) (mGetNode match n) 
                                      (map stripExpr (getArgList $ findWithDefault (Args []) (n ++ "__args") match) ++ [a])   
    alg (PStar n)           = stripExpr (mGetSubtree match n)
  
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
