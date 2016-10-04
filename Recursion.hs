{-# LANGUAGE TypeFamilies #-}

module Recursion where

import Control.Comonad.Cofree
import Data.Functor.Foldable

newtype MyPair b f a = MyPair (b, f a)
instance Functor f => Functor (MyPair b f) where
    fmap g (MyPair (b, a)) = MyPair (b, fmap g a)

type instance Base (Cofree f b) = MyPair b f

instance Functor f => Recursive (Cofree f b) where
    project (b :< a) = MyPair(b, a)

type Algebra f a = f a -> a

attribute :: Functor f => (f (Fix f, a) -> a) -> (f (Fix f, Cofree f a) -> Cofree f a)
attribute alg e = (alg $ fmap (fmap geta) e) :< fmap snd e where
    geta (a :< _) = a