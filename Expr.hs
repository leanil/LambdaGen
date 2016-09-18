{-# LANGUAGE DeriveFunctor, FlexibleInstances, TypeSynonymInstances #-} --ezek mik?

module Expr where

import Utility (Fix(..))
import Type (Type)
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
    | Lambda { variable :: String, body :: a }
    | Variable { id :: String }
    | Map { lambda :: a, vector :: a }
    | Reduce { lambda :: a, vector :: a }
    | ZipWith { lambda :: a, vector1 :: a, vector2 :: a }
    deriving (Functor)

data ExtF a = ExtF { operation :: ExprF a, exprType :: Type }
instance Functor ExtF where
    fmap f (ExtF o t) = ExtF (fmap f o) t

type Expr = Fix ExtF

fop :: ExprF Expr -> Expr
fop e = Fix ExtF { operation = e }

scl :: Double -> Expr
scl = fop . Scalar

vecView :: String -> Int -> Expr
vecView i s = fop $ VectorView i s

vec :: [Expr] -> Expr
vec = fop . Vector

add :: Expr -> Expr -> Expr
add x y = fop $ Addition x y

sub :: Expr -> Expr -> Expr
sub x y = fop $ Subtraction x y

mul :: Expr -> Expr -> Expr
mul x y = fop $ Multiplication x y

div :: Expr -> Expr -> Expr
div x y = fop $ Division x y

app :: Expr -> Expr -> Expr
app l i = fop $ Apply l i

lam :: Expr -> Expr -> Expr
lam (Fix (ExtF (Variable id) t)) b = Fix $ ExtF (Lambda id b) t

var :: String -> Type -> Expr
var i t = Fix ExtF { operation = Variable i, exprType = t }

mkMap :: Expr -> Expr -> Expr
mkMap l v = fop $ Map l v

mkReduce :: Expr -> Expr -> Expr
mkReduce l v = fop $ Reduce l v

mkZipWith :: Expr -> Expr -> Expr -> Expr
mkZipWith l v1 v2 = fop $ ZipWith l v1 v2

