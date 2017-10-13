{-# LANGUAGE DataKinds, DeriveFunctor, PatternSynonyms, ScopedTypeVariables, TypeOperators #-}

import Expr
import Recursion
import Type
import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Functor.Foldable
import qualified Data.Map.Strict as M
import Data.Vinyl
import Data.Vinyl.Functor 

data PExprF a
    = PScalar String (String,Maybe Double)
    | PAddition String a a
    | PMultiplication String a a
    | PVectorView String (String,Maybe String) (String,Maybe [Int]) (String,Maybe [Int])
    | PApply String a a
    | PLambda String (String,Maybe String) (String,Maybe Type) a
    | PVariable String (String,Maybe String) (String,Maybe Type)
    | PMap String a a
    | PReduce String a a
    | PZipWith String a a a
    | PStar String
    deriving (Eq, Functor, Show)

type PExpr = Fix PExprF

pattern FPScalar n a           = Fix (PScalar n a)
pattern FPAddition n a b       = Fix (PAddition n a b)
pattern FPMultiplication n a b = Fix (PMultiplication n a b)
pattern FPVectorView n a b c   = Fix (PVectorView n a b c)
pattern FPApply n lam val      = Fix (PApply n lam val)
pattern FPLambda n a b body    = Fix (PLambda n a b body)
pattern FPVariable n a b       = Fix (PVariable n a b)
pattern FPMap n lam v          = Fix (PMap n lam v)
pattern FPReduce n lam v       = Fix (PReduce n lam v)
pattern FPZipWith n lam v1 v2  = Fix (PZipWith n lam v1 v2)
pattern FPStar id              = Fix (PStar id)

pScl n a           = Fix $ PScalar n (a,Nothing)
pAdd n a b         = Fix $ PAddition n a b
pMul n a b         = Fix $ PMultiplication n a b
pVecView n a b c   = Fix $ PVectorView n (a,Nothing) (b,Nothing) (c,Nothing)
pApp n l v         = Fix $ PApply n l v
pLam n a b body    = Fix $ PLambda n (a,Nothing) (b,Nothing) body
pVar n a b         = Fix $ PVariable n (a,Nothing) (b,Nothing)
pMap n l v         = Fix $ PMap n l v
pReduce n l v      = Fix $ PReduce n l v
pZipWith n l v1 v2 = Fix $ PZipWith n l v1 v2

data MatchVal fields
    = Expr  { getExpr  :: Expr fields }
    | Type  { getType  :: Type }
    | Str   { getStr   :: String }
    | Val   { getVal   :: Double }
    | List  { getList  :: [Int] }
    | Annot { getAnnot :: R fields }

type Match fields = M.Map String (MatchVal fields)

mGetExpr match (id,Nothing) = getExpr $ match M.! id
mGetExpr match (_,Just a)   = a
mGetType match (id,Nothing) = getType $ match M.! id
mGetType match (_,Just a)   = a
mGetStr  match (id,Nothing) = getStr $ match M.! id
mGetStr  match (_,Just a)   = a
mGetVal  match (id,Nothing) = getVal $ match M.! id
mGetVal  match (_,Just a)   = a
mGetList match (id,Nothing) = getList $ match M.! id
mGetList match (_,Just a)   = a
mGetAnnot match = getAnnot . (match M.!)

makeComp :: (Expr fields, PExpr) -> Either (Maybe (Match fields)) (CofreeF PExprF (R fields) (Expr fields, PExpr))
makeComp (FScalar r i, FPScalar n (id,_))                              = Right $ r ::< PScalar n (id,Just i)
makeComp (FAddition r a b, FPAddition n c d)                           = Right $ r ::< PAddition n (a,c) (b,d)
makeComp (FMultiplication r a b, FPMultiplication n c d)               = Right $ r ::< PMultiplication n (a,c) (b,d)
makeComp (FVectorView r a b c, FPVectorView n (id1,_) (id2,_) (id3,_)) = Right $ r ::< PVectorView n (id1,Just a) (id2,Just b) (id3,Just c)
makeComp (FApply r a b, FPApply n c d)                                 = Right $ r ::< PApply n (a,c) (b,d)
makeComp (FLambda r a b c, FPLambda n (id1,_) (id2,_) d)               = Right $ r ::< PLambda n (id1,Just a) (id2,Just b) (c,d)
makeComp (FVariable r a b, FPVariable n (id1,_) (id2,_))               = Right $ r ::< PVariable n (id1,Just a) (id2,Just b)
makeComp (FMap r a b, FPMap n c d)                                     = Right $ r ::< PMap n (a,c) (b,d)
makeComp (FReduce r a b, FPReduce n c d)                               = Right $ r ::< PReduce n (a,c) (b,d)
makeComp (FZipWith r a b c, FPZipWith n d e f)                         = Right $ r ::< PZipWith n (a,d) (b,e) (c,f)
makeComp (subTree, FPStar id)                                          = Left $ Just $ M.singleton id (Expr subTree)
makeComp _                                                             = Left Nothing

evalComp :: CofreeF PExprF (R fields) (Maybe (Match fields)) -> (Maybe (Match fields))
evalComp (r ::< PScalar n (id,Just a))                                = Just $ M.fromList [(n,Annot r),(id,Val a)]
evalComp (r ::< PAddition n (Just a) (Just b))                        = Just $ M.insert n (Annot r) $ M.union a b
evalComp (r ::< PMultiplication n (Just a) (Just b))                  = Just $ M.insert n (Annot r) $ M.union a b
evalComp (r ::< PVectorView n (id1,Just a) (id2,Just b) (id3,Just c)) = Just $ M.fromList [(n,Annot r),(id1,Str a),(id2,List b),(id3,List c)]
evalComp (r ::< PApply n (Just a) (Just b))                           = Just $ M.insert n (Annot r) $ M.union a b
evalComp (r ::< PLambda n (id1,Just a) (id2,Just b) (Just c))         = Just $ M.union c $ M.fromList [(n,Annot r),(id1,Str a),(id2,Type b)]
evalComp (r ::< PVariable n (id1,Just a) (id2,Just b))                = Just $ M.fromList [(n,Annot r),(id1,Str a),(id2,Type b)]
evalComp (r ::< PMap n (Just a) (Just b))                             = Just $ M.insert n (Annot r) $ M.union a b
evalComp (r ::< PReduce n (Just a) (Just b))                          = Just $ M.insert n (Annot r) $ M.union a b
evalComp (r ::< PZipWith n (Just a) (Just b) (Just c))                = Just $ M.insert n (Annot r) $ M.unions [a,b,c]
evalComp _                                                            = Nothing
    

fillReplacement :: forall fields. (Match fields) -> PExpr -> Expr fields
fillReplacement match = cata alg where
    alg :: Algebra PExpr (Expr fields)
    alg (PScalar n id)              = mGetAnnot match n :< Scalar (mGetVal match id)
    alg (PAddition n a b)           = mGetAnnot match n :< Addition a b
    alg (PMultiplication n a b)     = mGetAnnot match n :< Multiplication a b
    alg (PVectorView n id dms strd) = mGetAnnot match n :< VectorView (mGetStr match id) (mGetList match dms) (mGetList match strd)
    alg (PApply n a b)              = mGetAnnot match n :< Apply a b
    alg (PLambda n id t a)          = mGetAnnot match n :< Lambda (mGetStr match id) (mGetType match t) a
    alg (PVariable n id t)          = mGetAnnot match n :< Variable (mGetStr match id) (mGetType match t)
    alg (PMap n l v)                = mGetAnnot match n :< Map l v
    alg (PReduce n l v)             = mGetAnnot match n :< Reduce l v
    alg (PZipWith n l v1 v2)        = mGetAnnot match n :< ZipWith l v1 v2

replaceAll :: PExpr -> PExpr -> Expr fields -> Expr fields
replaceAll pat rep = cata replace where
    replace :: Algebra (Expr fields) (Expr fields)
    replace subTree = case elgot evalComp makeComp (embed subTree, pat) of
                        (Just m) -> fillReplacement m rep
                        Nothing  -> embed subTree
