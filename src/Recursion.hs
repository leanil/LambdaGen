{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, PatternSynonyms, TypeFamilies, TypeOperators #-}

module Recursion where

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree ((:<)))
import qualified Control.Comonad.Trans.Cofree as CofreeT (CofreeF ((:<)))
import Control.Comonad.Trans.Cofree (headF, tailF)
import Data.Functor.Foldable
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl
import Data.Vinyl.Functor 

type CofreeF = CofreeT.CofreeF
pattern (::<) :: a -> f b -> CofreeF f a b
pattern a ::< b = a CofreeT.:< b
type R = HList

type Algebra t a = Base t a -> a
type RAlgebra t a = Base t (t, a) -> a
type CoAlgebra t a = a -> Base t a

annotate :: Functor f => 
            Algebra (Cofree f (R old)) new ->
            Algebra (Cofree f (R old)) (Cofree f (R (new ': old)))
annotate alg (old ::< f) = (Identity (alg (old ::< fmap (fieldVal . extract) f)) :& old) :< f

annotatePara :: Functor f =>
                RAlgebra (Cofree f (R old)) new ->
                RAlgebra (Cofree f (R old)) (Cofree f (R (new ': old)))
annotatePara alg (old ::< f) = (Identity (alg (old ::< fmap (fmap (fieldVal . extract)) f)) :& old) :< fmap snd f

annotateAna :: Functor f =>
               CoAlgebra (Cofree f new) (Cofree f (R old), seed) -> 
               CoAlgebra (Cofree f (R (new ': old))) (Cofree f (R old), seed)
annotateAna alg a@(old :< _,_) = (Identity (headF tmp) :& old) ::< tailF tmp where tmp = alg a

fieldVal :: a ∈ fields => R fields -> a
fieldVal = getIdentity . rget (Proxy :: Proxy a)