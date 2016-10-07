{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, TypeFamilies #-}

module Recursion where

import Control.Comonad.Cofree
import Data.Functor.Foldable

newtype MyPair b f a = MyPair (b, f a) deriving Show
instance Functor f => Functor (MyPair b f) where
    fmap g (MyPair (b, a)) = MyPair (b, fmap g a)

type instance Base (Cofree f b) = MyPair b f

instance Functor f => Recursive (Cofree f b) where
    project (b :< a) = MyPair(b, a)

type Algebra t a = Base t a -> a
type RAlgebra t a = Base t (t, a) -> a

attrCata :: Functor f => (f a -> a) -> f (Cofree f a) -> (Cofree f a)
--attrCata :: Functor (Base t) => Algebra t a -> Algebra t (Cofree (Base t) a)
attrCata alg e = alg (fmap geta e) :< e where
    geta (a :< _) = a

attrPara :: Functor f => (f (Fix f, a) -> a) -> (f (Fix f, Cofree f a) -> Cofree f a)
attrPara alg e = alg (fmap (fmap geta) e) :< fmap snd e where
    geta (a :< _) = a

attrCofCata :: Functor f => (Base (Cofree f a) b -> b) ->
              Base (Cofree f a) (Cofree f (a, b)) -> Cofree f (a, b)
attrCofCata alg e@(MyPair (a, f)) = (a, alg (fmap getb e)) :< f where
    getb ((_,b) :< _) = b

attrCofPara :: Functor f => (Base (Cofree f a) (Cofree f a, b) -> b) ->
              Base (Cofree f a) (Cofree f a, Cofree f (a, b)) -> Cofree f (a, b)
attrCofPara alg e@(MyPair (a, f)) = (a, alg (fmap (fmap getb) e)) :< fmap snd f where
    getb ((_,b) :< _) = b