{-# LANGUAGE DeriveFunctor #-}

module Type where

import Utility (Fix(..))

data TypeF a
    = Double
    | Size { value :: Int }
    | Power { left :: a, right :: a }
    | Arrow { left :: a, right :: a }
    deriving (Eq, Functor)
    
type Type = Fix TypeF

double :: Type
double = Fix Double

size :: Int -> Type
size = Fix . Size

power :: Type -> Type -> Type
power x y = Fix $ Power x y

arrow :: Type -> Type -> Type
arrow x y = Fix $ Arrow x y

