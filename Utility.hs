{-# LANGUAGE FlexibleContexts, StandaloneDeriving, UndecidableInstances #-} -- ezek mik?

module Utility where

newtype Fix f = Fix { unFix :: f (Fix f) }
deriving instance Eq (f (Fix f)) => Eq (Fix f) --ezt miért kellett külön?