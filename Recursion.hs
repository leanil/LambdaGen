{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, PatternSynonyms, TypeFamilies, TypeOperators #-}

module Recursion where

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree ((:<)))
import qualified Control.Comonad.Trans.Cofree as CofreeT (CofreeF ((:<)))
import Data.Functor.Foldable
import Data.Vinyl
import Data.Vinyl.Functor 

type CofreeF = CofreeT.CofreeF
pattern a ::< b = a CofreeT.:< b
type R = HList

type Algebra t a = Base t a -> a
type RAlgebra t a = Base t (t, a) -> a
type CoAlgebra t a = a -> Base t a

annotate :: Functor f => 
            Algebra (Cofree f (R old)) new ->
            Algebra (Cofree f (R old)) (Cofree f (R (new ': old)))
annotate alg (old ::< f) = (Identity (alg (old ::< fmap (getIdentity . rget ([] :: [new]) . extract) f)) :& old) :< f

annotatePara :: Functor f =>
                RAlgebra (Cofree f (R old)) new ->
                RAlgebra (Cofree f (R old)) (Cofree f (R (new ': old)))
annotatePara alg (old ::< f) = (Identity (alg (old ::< fmap (fmap (getIdentity . rget ([] :: [new]) . extract)) f)) :& old) :< fmap snd f

fieldVal :: a ∈ fields => sing a -> R fields -> a
fieldVal a = getIdentity . rget a