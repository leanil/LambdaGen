{-# LANGUAGE DataKinds, DeriveFunctor, DeriveFoldable, DeriveTraversable, LambdaCase, FlexibleInstances, PatternSynonyms, TypeSynonymInstances #-}

module Expr where

import Type
import Control.Comonad.Cofree
import Data.List
import Data.Vinyl

data ExprF a
    = Scalar { getValue :: Double }
    | Addition { left :: a, right :: a }
    | Multiplication { left :: a, right :: a }
    | VectorView { name :: String, dms :: [Int], strd :: [Int] }
    | Vector { elements :: [a] }
    | Apply { lambda :: a, values :: [a]}
    | Let { name :: String, value :: a, inExpr :: a }
    | Lambda { vars :: [(String,Type)], getBody :: a }
    | Variable { name :: String, tp :: Type }
    | Map { lambda :: a, vector :: a }
    | Reduce { lambda :: a, vector :: a }
    | ZipWith { lambda :: a, vector1 :: a, vector2 :: a }
    -- | NZipWith { lambda :: a, vector1 :: a, vector2 :: a }
    | Compose a a
    deriving (Functor, Foldable, Traversable, Show)

castLeaf :: ExprF a -> ExprF b
castLeaf = fmap (const undefined)

zipExprF :: (a -> b -> c) -> ExprF a -> [b] -> ExprF c
zipExprF f (Addition a b) (x:y:_) = Addition (f a x) (f b y)
zipExprF f (Multiplication a b) (x:y:_) = Multiplication (f a x) (f b y)
zipExprF f (Apply a b) (x:xs) = Apply (f a x) $ zipWith f b xs
zipExprF f (Let n a b) (x:y:_) = Let n (f a x) (f b y)
zipExprF f (Lambda a b) (x:_) = Lambda a (f b x)
zipExprF f (Map a b) (x:y:_) = Map (f a x) (f b y)
zipExprF f (Reduce a b) (x:y:_) = Reduce (f a x) (f b y)
zipExprF f (ZipWith a b c) (x:y:z:_) = ZipWith (f a x) (f b y) (f c z)
zipExprF _ a _ = castLeaf a

type Expr fields = Cofree ExprF (HList fields)
type Expr0 = Expr '[]

pattern FScalar r d               = (r :< Scalar d)
pattern FAddition r a b           = (r :< Addition a b)
pattern FMultiplication r a b     = (r :< Multiplication a b)
pattern FVectorView r id dms strd = (r :< VectorView id dms strd)
pattern FApply r lam vals         = (r :< Apply lam vals)
pattern FLet r n v e              = (r :< Let n v e)
pattern FLambda r vars body       = (r :< Lambda vars body)
pattern FVariable r id t          = (r :< Variable id t)
pattern FMap r lam v              = (r :< Map lam v)
pattern FReduce r lam v           = (r :< Reduce lam v)
pattern FZipWith r lam v1 v2      = (r :< ZipWith lam v1 v2)
pattern FCompose r a b            = (r :< Compose a b)

wrapExprF :: ExprF Expr0 -> Expr0
wrapExprF = (RNil :<)

scl :: Double -> Expr0
scl = wrapExprF . Scalar

add :: Expr0 -> Expr0 -> Expr0
add x y = wrapExprF $ Addition x y

mul :: Expr0 -> Expr0 -> Expr0
mul x y = wrapExprF $ Multiplication x y

vecView :: String -> [Int] -> Expr0
vecView i d = wrapExprF $ VectorView i d (defaultStrides d)

vecView' :: String -> [Int] -> [Int] ->Expr0
vecView' i d s = wrapExprF $ VectorView i d s

transpose :: [Int] -> Expr0 -> Expr0
transpose p (FVectorView _ i d s) = wrapExprF $ VectorView i (perm p d) (perm p s) where
    perm p' l = map snd $ sort $ zip p' l
    
vec :: [Expr0] -> Expr0
vec x = wrapExprF $ Vector x

app :: Expr0 -> [Expr0] -> Expr0
app l v = wrapExprF $ Apply l v

bind :: Expr0 -> Expr0 -> Expr0 -> Expr0
bind (FVariable _ n _) v e = wrapExprF $ Let n v e

lam :: [Expr0] -> Expr0 -> Expr0
lam v b = wrapExprF $ Lambda (map (\case FVariable _ i t -> (i, t)) v) b

lam' :: [(String,Type)] -> Expr0 -> Expr0
lam' v b = wrapExprF $ Lambda v b

var :: String -> Type -> Expr0
var i t = wrapExprF $ Variable i t

mkMap :: Expr0 -> Expr0 -> Expr0
mkMap l v = wrapExprF $ Map l v

mkReduce :: Expr0 -> Expr0 -> Expr0
mkReduce l v = wrapExprF $ Reduce l v

mkZipWith :: Expr0 -> Expr0 -> Expr0 -> Expr0
mkZipWith l v1 v2 = wrapExprF $ ZipWith l v1 v2

comp :: Expr0 -> Expr0 -> Expr0
comp a b = wrapExprF $ Compose a b

defaultStrides :: [Int] -> [Int]
defaultStrides = tail . scanr (*) 1
