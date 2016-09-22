module Recursion where

import Utility(Fix(..))

import Control.Comonad.Cofree

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Cofree f i -> a
cata alg = alg . fmap (cata alg) . unCofree

--type RAlgebra f a = f (Fix f, a) -> a

--para :: Functor f => RAlgebra f a -> Fix f -> a  
--para rAlg = rAlg . fmap (\t -> (t, para rAlg t)) . unwrap

unCofree :: Cofree f a -> f (Cofree f a)
unCofree (_ :< e) = e