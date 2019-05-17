{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleInstances, PatternSynonyms, StandaloneDeriving, TypeSynonymInstances #-}

module Type where

import Recursion
import Data.Functor.Classes (Eq1(..), Show1(..))
import Data.Functor.Foldable (cata, Fix(..))
import Data.List (intercalate)
import GHC.Generics (Generic)

data TypeF a
    = Double
    | Power { getBase :: a, getDims :: [(Int,Int)] }
    | Arrow { from :: [a], to :: a }
    deriving (Eq, Functor, Show)

instance Eq1 TypeF where
    liftEq _ Double Double = True
    liftEq eq (Power a b) (Power c d) = eq a c && (map fst b) == (map fst d)
    liftEq eq (Arrow a b) (Arrow c d) = and (zipWith eq a c) && eq b d
    liftEq _ _ _ = False

instance Show1 TypeF where
    liftShowsPrec _ _ _  Double = showString "Double"
    liftShowsPrec sp _ d (Power a b) = sp d a . showString ("^" ++ show b)
    liftShowsPrec sp l d (Arrow a b) =  l a . showString "->" . sp d b

type Type = Fix TypeF

deriving instance Generic Type
deriving instance Generic (TypeF Type)

double :: Type
double = Fix Double

power :: Type -> [Int] -> Type
power x y = power' x $ zip y $ tail $ scanr (*) 1 y

power' :: Type -> [(Int,Int)] -> Type
power' x y = Fix $ Power x y

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

raiseToPower :: Type -> Int -> Type
raiseToPower (FPower t shape) a = power' t ((a,stride):shape) where
    stride = product $ map fst shape
raiseToPower t a = power t [a]

size :: Type -> Int
size (FPower _ ((s,_):_)) = s
size a = error $ "size on non-array type " ++ (showT a)