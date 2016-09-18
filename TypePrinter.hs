{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module TypePrinter where

import Recursion
import Type

typePrinterAlg :: Algebra TypeF String

typePrinterAlg Double = "double"

typePrinterAlg (Size s) = show s

typePrinterAlg (Power a b) = "(" ++ a ++ ")^(" ++ b ++ ")"

typePrinterAlg (Arrow a b) = "(" ++ a ++ ")->(" ++ b ++ ")"

instance Show Type where
    show = cata typePrinterAlg