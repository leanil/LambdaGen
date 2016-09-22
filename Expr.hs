{-# LANGUAGE DeriveFunctor, FlexibleInstances, TypeSynonymInstances #-} --ezek mik?

module Expr where

import Utility
import Type

import Control.Comonad.Cofree

data ExprF a
    = Scalar { getValue :: Double }
    | VectorView { id :: String, getSize :: Int }
    | Vector { elements :: [a] }
    | Addition { left :: a, right :: a }
    | Subtraction { left :: a, right :: a }
    | Multiplication { left :: a, right :: a }
    | Division { left :: a, right :: a }
    | Apply { lambda :: a, input :: a}
    | Lambda { varID :: String, varType :: Type, body :: a }
    | Variable { id :: String, tp :: Type }
    | Map { lambda :: a, vector :: a }
    | Reduce { lambda :: a, vector :: a }
    | ZipWith { lambda :: a, vector1 :: a, vector2 :: a }
    deriving (Functor)

--data ExtF a = ExtF { operation :: ExprF a, exprType :: Type }
--instance Functor ExtF where
--    fmap f (ExtF o t) = ExtF (fmap f o) t

type Expr = Fix ExprF

scl :: Double -> Expr
scl = fix . Scalar

vecView :: String -> Int -> Expr
vecView i s = fix $ VectorView i s

vec :: [Expr] -> Expr
vec x = fix $ Vector x

add :: Expr -> Expr -> Expr
add x y = fix $ Addition x y

sub :: Expr -> Expr -> Expr
sub x y = fix $ Subtraction x y

mul :: Expr -> Expr -> Expr
mul x y = fix $ Multiplication x y

div :: Expr -> Expr -> Expr
div x y = fix $ Division x y

app :: Expr -> Expr -> Expr
app l i = fix $ Apply l i

lam :: Expr -> Expr -> Expr
lam (() :< Variable id t) b = fix $ Lambda id t b

var :: String -> Type -> Expr
var i t = fix $ Variable i t

mkMap :: Expr -> Expr -> Expr
mkMap l v = fix $ Map l v

mkReduce :: Expr -> Expr -> Expr
mkReduce l v = fix $ Reduce l v

mkZipWith :: Expr -> Expr -> Expr -> Expr
mkZipWith l v1 v2 = fix $ ZipWith l v1 v2

