{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module TypePrinter where
    
import Data.Functor.Foldable

import Recursion
import Type

typePrinterAlg :: Algebra TypeF String

typePrinterAlg Double = "double"

typePrinterAlg (Size s) = show s

typePrinterAlg (Power a b) = "(" ++ a ++ ")^(" ++ b ++ ")"

typePrinterAlg (Arrow a b) = "(" ++ a ++ ")->(" ++ b ++ ")"

showT :: Type -> String -- a sima Show overlap-el a Cofree Show-jával
showT = cata typePrinterAlg