{-# LANGUAGE DeriveFunctor #-}

module Type where

import Data.Functor.Classes
import Data.Functor.Foldable

data TypeF a
    = Double
    | Size { value :: Int }
    | Power { left :: a, right :: a }
    | Arrow { left :: a, right :: a }
    deriving (Eq, Show, Functor)

instance Eq1 TypeF where eq1 = (==)
instance Show1 TypeF where showsPrec1 = showsPrec

type Type = Fix TypeF

double :: Type
double = Fix Double

size :: Int -> Type
size = Fix . Size

power :: Type -> Type -> Type
power x y = Fix $ Power x y

arrow :: Type -> Type -> Type
arrow x y = Fix $ Arrow x y

