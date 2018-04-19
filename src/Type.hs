{-# LANGUAGE DeriveFunctor, PatternSynonyms #-}

module Type where

import Recursion
import Data.Functor.Classes (Eq1(..), Show1(..))
import Data.Functor.Foldable (cata, Fix(..))
import Data.List (intercalate)

data TypeF a
    = Double
    | Power { getBase :: a, getExtents :: [Int] }
    | Arrow { from :: [a], to :: a }
    deriving (Eq, Show, Functor)

instance Eq1 TypeF where
    liftEq _ Double Double = True
    liftEq eq (Power a b) (Power c d) = eq a c && b == d
    liftEq eq (Arrow a b) (Arrow c d) = and (zipWith eq a c) && eq b d
    liftEq _ _ _ = False

instance Show1 TypeF where
    liftShowsPrec _ _ _  Double = showString "Double"
    liftShowsPrec sp _ d (Power a b) = sp d a . showString ("^" ++ show b)
    liftShowsPrec sp l d (Arrow a b) =  l a . showString "->" . sp d b

type Type = Fix TypeF

double :: Type
double = Fix Double

power :: Type -> [Int] -> Type
power x y = Fix $ Power x y

arrow :: [Type] -> Type -> Type
arrow x y = Fix $ Arrow x y

pattern FDouble    = Fix Double
pattern FPower a b = Fix (Power a b)
pattern FArrow a b = Fix (Arrow a b)

typePrinterAlg :: Algebra Type String
typePrinterAlg Double = "double"
typePrinterAlg (Power a b) = "(" ++ a ++ ")^" ++ show b
typePrinterAlg (Arrow a b) = "(" ++ intercalate " " a ++ ")->(" ++ b ++ ")"

showT :: Type -> String
showT = cata typePrinterAlg

countDims :: Type -> [Int]
countDims FDouble = [1]
countDims (FPower _ d) = d
countDims _ = []

raiseToPower :: Type -> Int -> Type
raiseToPower (FPower t b) a = power t (a:b)
raiseToPower t a = power t [a]