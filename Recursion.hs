module Recursion where

import Utility(Fix(..))

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

type RAlgebra f a = f (Fix f, a) -> a

para :: Functor f => RAlgebra f a -> Fix f -> a  
para rAlg = rAlg . fmap (\t -> (t, para rAlg t)) . unFix