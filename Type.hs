{-# LANGUAGE DeriveFunctor #-}

module Type where

import Utility

data TypeF a
    = Double
    | Size { value :: Int }
    | Power { left :: a, right :: a }
    | Arrow { left :: a, right :: a }
    deriving (Eq, Functor)
    
type Type = Fix TypeF

double :: Type
double = fix Double

size :: Int -> Type
size = fix . Size

power :: Type -> Type -> Type
power x y = fix $ Power x y

arrow :: Type -> Type -> Type
arrow x y = fix $ Arrow x y

