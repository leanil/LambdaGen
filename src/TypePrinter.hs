{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module TypePrinter where

import Recursion
import Type    
import Data.Functor.Foldable
import Data.List (intercalate)

typePrinterAlg :: Algebra Type String

typePrinterAlg Double = "double"

typePrinterAlg (Dim s) = show s

typePrinterAlg (Power a b) = "(" ++ a ++ ")^(" ++ b ++ ")"

typePrinterAlg (Arrow a b) = "(" ++ intercalate " " a ++ ")->(" ++ b ++ ")"

showT :: Type -> String -- a sima Show overlap-el a Cofree Show-jával
showT = cata typePrinterAlg
