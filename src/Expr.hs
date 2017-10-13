{-# LANGUAGE DataKinds, DeriveFunctor, FlexibleInstances, PatternSynonyms, TypeSynonymInstances #-}

module Expr where

import Type
import Control.Comonad.Cofree
import Data.Functor.Foldable
import Data.List
import Data.Vinyl

data ExprF a
    = Scalar { getValue :: Double }
    | Addition { left :: a, right :: a }
    | Subtraction { left :: a, right :: a }
    | Multiplication { left :: a, right :: a }
    | Division { left :: a, right :: a }
    | VectorView { id :: String, dms :: [Int], strd :: [Int] }
    | Vector { elements :: [a] }
    | Apply { lambda :: a, value :: a}
    | Lambda { varID :: String, varType :: Type, body :: a }
    | Variable { id :: String, tp :: Type }
    | Map { lambda :: a, vector :: a }
    | Reduce { lambda :: a, vector :: a }
    | ZipWith { lambda :: a, vector1 :: a, vector2 :: a }
    deriving (Functor, Show)

type Expr fields = Cofree ExprF (HList fields)
type Expr0 = Expr '[]

pattern FScalar r d               =  (r :< Scalar d)
pattern FAddition r a b           =  (r :< Addition a b)
pattern FMultiplication r a b     =  (r :< Multiplication a b)
pattern FVectorView r id dms strd =  (r :< VectorView id dms strd)
pattern FApply r lam val          =  (r :< Apply lam val)
pattern FLambda r id t body       =  (r :< Lambda id t body)
pattern FVariable r id t          =  (r :< Variable id t)
pattern FMap r lam v              =  (r :< Map lam v)
pattern FReduce r lam v           =  (r :< Reduce lam v)
pattern FZipWith r lam v1 v2      =  (r :< ZipWith lam v1 v2)

wrapExprF :: ExprF Expr0 -> Expr0
wrapExprF = (RNil :<)

scl :: Double -> Expr0
scl = wrapExprF . Scalar

add :: Expr0 -> Expr0 -> Expr0
add x y = wrapExprF $ Addition x y

sub :: Expr0 -> Expr0 -> Expr0
sub x y = wrapExprF $ Subtraction x y

mul :: Expr0 -> Expr0 -> Expr0
mul x y = wrapExprF $ Multiplication x y

div :: Expr0 -> Expr0 -> Expr0
div x y = wrapExprF $ Division x y

vecView :: String -> [Int] -> Expr0
vecView i d = wrapExprF $ VectorView i d (defaultStrides d)

vecView' :: String -> [Int] -> [Int] ->Expr0
vecView' i d s = wrapExprF $ VectorView i d s

transpose :: [Int] -> Expr0 -> Expr0
transpose p (_ :< VectorView i d s) = wrapExprF $ VectorView i (perm p d) (perm p s) where
    perm p l = map snd $ sort $ zip p l
    
vec :: [Expr0] -> Expr0
vec x = wrapExprF $ Vector x

app :: Expr0 -> Expr0 -> Expr0
app l i = wrapExprF $ Apply l i

lam :: Expr0 -> Expr0 -> Expr0
lam (_ :< Variable id t) b = wrapExprF $ Lambda id t b

lam' :: String -> Type -> Expr0 -> Expr0
lam' id t b = wrapExprF $ Lambda id t b

var :: String -> Type -> Expr0
var i t = wrapExprF $ Variable i t

mkMap :: Expr0 -> Expr0 -> Expr0
mkMap l v = wrapExprF $ Map l v

mkReduce :: Expr0 -> Expr0 -> Expr0
mkReduce l v = wrapExprF $ Reduce l v

mkZipWith :: Expr0 -> Expr0 -> Expr0 -> Expr0
mkZipWith l v1 v2 = wrapExprF $ ZipWith l v1 v2

defaultStrides :: [Int] -> [Int]
defaultStrides = tail . scanr (*) 1