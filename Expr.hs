{-# LANGUAGE DataKinds, DeriveFunctor, FlexibleInstances, TypeSynonymInstances #-}

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
    | VectorView { id :: String, dimensions :: [Int], strides :: [Int] }
    | Vector { elements :: [a] }
    | Apply { lambda :: a, input :: a}
    | Lambda { varID :: String, varType :: Type, body :: a }
    | Variable { id :: String, tp :: Type }
    | Map { lambda :: a, vector :: a }
    | Reduce { lambda :: a, vector :: a }
    | ZipWith { lambda :: a, vector1 :: a, vector2 :: a }
    deriving (Functor, Show)

type Expr = Cofree ExprF (HList '[])

wrapExprF :: ExprF Expr -> Expr
wrapExprF = (RNil :<)

scl :: Double -> Expr
scl = wrapExprF . Scalar

add :: Expr -> Expr -> Expr
add x y = wrapExprF $ Addition x y

sub :: Expr -> Expr -> Expr
sub x y = wrapExprF $ Subtraction x y

mul :: Expr -> Expr -> Expr
mul x y = wrapExprF $ Multiplication x y

div :: Expr -> Expr -> Expr
div x y = wrapExprF $ Division x y

vecView :: String -> [Int] -> Expr
vecView i d = wrapExprF $ VectorView i d (defaultStrides d)

transpose :: [Int] -> Expr -> Expr
transpose p (_ :< VectorView i d s) = wrapExprF $ VectorView i (perm p d) (perm p s) where
    perm p l = map snd $ sort $ zip p l
    
vec :: [Expr] -> Expr
vec x = wrapExprF $ Vector x

app :: Expr -> Expr -> Expr
app l i = wrapExprF $ Apply l i

lam :: Expr -> Expr -> Expr
lam (_ :< Variable id t) b = wrapExprF $ Lambda id t b

var :: String -> Type -> Expr
var i t = wrapExprF $ Variable i t

mkMap :: Expr -> Expr -> Expr
mkMap l v = wrapExprF $ Map l v

mkReduce :: Expr -> Expr -> Expr
mkReduce l v = wrapExprF $ Reduce l v

mkZipWith :: Expr -> Expr -> Expr -> Expr
mkZipWith l v1 v2 = wrapExprF $ ZipWith l v1 v2

defaultStrides :: [Int] -> [Int]
defaultStrides = tail . scanr (*) 1