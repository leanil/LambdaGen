{-# LANGUAGE DeriveFunctor, PatternSynonyms #-}

module Type where

import Recursion
import Data.Functor.Classes
import Data.Functor.Foldable

data TypeF a
    = Double
    | Dim { value :: Int }
    | Power { left :: a, right :: a }
    | Arrow { left :: a, right :: a }
    deriving (Eq, Show, Functor)

instance Eq1 TypeF where
    liftEq _ Double Double = True
    liftEq _ (Dim x) (Dim y) = x == y
    liftEq eq (Power a b) (Power c d) = eq a c && eq b d
    liftEq eq (Arrow a b) (Arrow c d) = eq a c && eq b d
    liftEq _ _ _ = False

instance Show1 TypeF where
    liftShowsPrec sp _ d Double = showString "Double"
    liftShowsPrec sp _ d (Dim x) = showString (show x)
    liftShowsPrec sp _ d (Power a b) = sp d a . showString "^" . sp d b
    liftShowsPrec sp _ d (Arrow a b) = sp d a . showString "->" . sp d b

type Type = Fix TypeF

double :: Type
double = Fix Double

dim :: Int -> Type
dim = Fix . Dim

power :: Type -> Type -> Type
power x y = Fix $ Power x y

arrow :: Type -> Type -> Type
arrow x y = Fix $ Arrow x y

pattern FDouble    = Fix Double
pattern FDim a     = Fix (Dim a)
pattern FPower a b = Fix (Power a b)
pattern FArrow a b = Fix (Arrow a b)

dimCounterAlg :: Algebra Type [Int]

dimCounterAlg (Dim s) = [s]

dimCounterAlg (Power a b) = head b : a

dimCounterAlg _ = []

countDims :: Type -> [Int]
countDims = cata dimCounterAlg
