{-# LANGUAGE FlexibleContexts, StandaloneDeriving, UndecidableInstances #-} -- ezek mik?

module Utility where

import Control.Comonad.Cofree

type Fix f = Cofree f ()

fix :: f (Fix f) -> Fix f
fix = (() :<)

--deriving instance Eq (f (Fix f)) => Eq (Fix f) --ezt miért kellett külön?