{-# LANGUAGE DataKinds, DeriveFunctor, DeriveFoldable, DeriveTraversable, LambdaCase, FlexibleInstances, PatternSynonyms, TemplateHaskell, TypeSynonymInstances, ViewPatterns #-}

module Expr where

import Type
import Control.Comonad.Cofree
import Data.Eq.Deriving (deriveEq1)
import Data.List
import Data.Vinyl
import Text.Show.Deriving (deriveShow1)

data ExprF a
    = Scalar { getSclVal :: Double }
    | ScalarOp { getOp :: Char, getOpLeft :: a, getOpRight :: a }
    | View { getName :: String, getShape :: [(Int,Int)] }
    | Apply { getLambda :: a, getArgs :: [a]}
    | Lambda { getParams :: [(String,Type)], getBindings :: [(String,a)], getLambdaBody :: a }
    | Variable { getName :: String, getVarType :: Type }
    | RnZ { getReducer :: a, getZipper :: a, getVecs :: [a] }
    | ZipWithN { getLambda :: a, getVecs :: [a] }
    | LayoutOp { getLayoutOp :: LayoutOp, getBaseExpr :: a}
    deriving (Functor, Foldable, Traversable, Show)

data LayoutOp
    = FlipOp { getDims :: (Int,Int) } -- TODO: support non-adjacent flips on C++ side
    | SubdivOp { getDim :: Int, getBlockSize :: Int }
    | FlattenOp { getDim :: Int }
    deriving (Eq, Show)

pattern Flip a b     = LayoutOp (FlipOp a) b
pattern Subdiv a b c = LayoutOp (SubdivOp a b) c
pattern Flatten a b = LayoutOp (FlattenOp a) b

deriveEq1 ''ExprF
deriveShow1 ''ExprF

castLeaf :: ExprF a -> ExprF b
castLeaf = fmap undefined

zipWithExprF :: (a -> b -> c) -> ExprF a -> [b] -> ExprF c
zipWithExprF f (ScalarOp op a b) (x:y:_) = ScalarOp op (f a x) (f b y)
zipWithExprF f (Apply a b) (x:xs) = Apply (f a x) $ zipWith f b xs
zipWithExprF f (Lambda a b c) (x:xs) = Lambda a (zipWith (\(s,p) q -> (s,f p q)) b xs) (f c x)
zipWithExprF f (RnZ a b c) (x:y:xs) = RnZ (f a x) (f b y) (zipWith f c xs)
zipWithExprF f (ZipWithN a b) (x:xs) = ZipWithN (f a x) (zipWith f b xs)
zipWithExprF f (LayoutOp op a) (x:_) = LayoutOp op (f a x)
zipWithExprF _ a _ = castLeaf a

zipExprF :: ExprF a -> [b] -> ExprF (a,b)
zipExprF = zipWithExprF (,)

type Expr fields = Cofree ExprF (HList fields)
type Expr0 = Expr '[]

pattern FScalar r d               = (r :< Scalar d)
pattern FScalarOp r op a b        = (r :< ScalarOp op a b)
pattern FView r id shape       = (r :< View id shape)
pattern FApply r lam vals         = (r :< Apply lam vals)
pattern FLambda r vars binds body = (r :< Lambda vars binds body)
pattern FVariable r id t          = (r :< Variable id t)
pattern FRnZ r l1 l2 v            = (r :< RnZ l1 l2 v)
pattern FZipWithN r lam v         = (r :< ZipWithN lam v)

wrapExprF :: ExprF Expr0 -> Expr0
wrapExprF = (RNil :<)

scl :: Double -> Expr0
scl = wrapExprF . Scalar

add :: Expr0 -> Expr0 -> Expr0
add x y = wrapExprF $ ScalarOp '+' x y

mul :: Expr0 -> Expr0 -> Expr0
mul x y = wrapExprF $ ScalarOp '*' x y

vecView :: String -> [Int] -> Expr0
vecView i d = wrapExprF $ View i (defaultStrides d)

vecView' :: String -> [(Int,Int)] ->Expr0
vecView' i shape = wrapExprF $ View i shape

transpose :: [Int] -> Expr0 -> Expr0
transpose p (FView _ i shape) = wrapExprF $ View i (perm p shape) where
    perm p' l = map snd $ sort $ zip p' l

app :: Expr0 -> [Expr0] -> Expr0
app l v = wrapExprF $ Apply l v

-- bind :: Expr0 -> Expr0 -> Expr0 -> Expr0
-- bind (FVariable _ n _) v e = wrapExprF $ Let n v e

lamBind :: [Expr0] -> [(Expr0,Expr0)] -> Expr0 -> Expr0
lamBind v bind body = wrapExprF $ Lambda 
    (map (\(FVariable _ i t) -> (i, t)) v)
    (map (\(FVariable _ i _, e) -> (i,e)) bind) body

lam :: [Expr0] -> Expr0 -> Expr0
lam v body = lamBind v [] body

lam' :: [(String,Type)] -> [(String,Expr0)] -> Expr0 -> Expr0
lam' v bind body = wrapExprF $ Lambda v bind body

var :: String -> Type -> Expr0
var i t = wrapExprF $ Variable i t

mkMap :: Expr0 -> Expr0 -> Expr0
mkMap l v = wrapExprF $ ZipWithN l [v]

mkReduce :: Expr0 -> Expr0 -> Expr0 -> Expr0
mkReduce l v vec = wrapExprF $ RnZ l (lam [v] v) [vec]

mkRnZ :: Expr0 -> Expr0 -> [Expr0] -> Expr0
mkRnZ r z vs = wrapExprF $ RnZ r z vs

mkZipWith :: Expr0 -> Expr0 -> Expr0 -> Expr0
mkZipWith l v1 v2 = mkZipWithN l [v1,v2]

mkZipWithN :: Expr0 -> [Expr0] -> Expr0
mkZipWithN l vs = wrapExprF $ ZipWithN l vs

mkFlip :: (Int,Int) -> Expr0 -> Expr0
mkFlip a b = wrapExprF $ Flip a b

subdiv :: Int -> Int -> Expr0 -> Expr0
subdiv a b c = wrapExprF $ Subdiv a b c

flatten :: Int -> Expr0 -> Expr0
flatten a b = wrapExprF $ Flatten a b

isLeafNode :: ExprF a -> Bool
isLeafNode (Scalar{}) = True
isLeafNode (View{}) = True
isLeafNode (Variable{}) = True
isLeafNode _ = False