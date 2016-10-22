{-# LANGUAGE DeriveFunctor, FlexibleInstances, TypeSynonymInstances #-} --ezek mik?

module Expr where

import Type
import Control.Comonad.Cofree
import Data.Functor.Foldable
import Data.List

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

type Expr = Fix ExprF

scl :: Double -> Expr
scl = Fix . Scalar

add :: Expr -> Expr -> Expr
add x y = Fix $ Addition x y

sub :: Expr -> Expr -> Expr
sub x y = Fix $ Subtraction x y

mul :: Expr -> Expr -> Expr
mul x y = Fix $ Multiplication x y

div :: Expr -> Expr -> Expr
div x y = Fix $ Division x y

vecView :: String -> [Int] -> Expr
vecView i d = Fix $ VectorView i d (tail $ scanr (*) 1 d)

transpose :: [Int] -> Expr -> Expr
transpose p (Fix (VectorView i d s)) = Fix $ VectorView i (perm p d) (perm p s) where
    perm p l = map snd $ sort $ zip p l
    
vec :: [Expr] -> Expr
vec x = Fix $ Vector x

app :: Expr -> Expr -> Expr
app l i = Fix $ Apply l i

lam :: Expr -> Expr -> Expr
lam (Fix (Variable id t)) b = Fix $ Lambda id t b

var :: String -> Type -> Expr
var i t = Fix $ Variable i t

mkMap :: Expr -> Expr -> Expr
mkMap l v = Fix $ Map l v

mkReduce :: Expr -> Expr -> Expr
mkReduce l v = Fix $ Reduce l v

mkZipWith :: Expr -> Expr -> Expr -> Expr
mkZipWith l v1 v2 = Fix $ ZipWith l v1 v2

