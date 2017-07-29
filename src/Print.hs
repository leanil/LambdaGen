{-# LANGUAGE DataKinds, FlexibleContexts, ScopedTypeVariables, TypeOperators, ViewPatterns #-}

module Print where

import Expr
import Recursion
import Control.Comonad.Cofree (Cofree(..))
import Data.Functor.Foldable (ana, cata)
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl
import Data.Vinyl.Functor (Identity(..))
import Data.Vinyl.TypeLevel (RecAll)

newtype IndentT = IndentT Int deriving Show

inc :: IndentT -> IndentT
inc (IndentT i) = IndentT $ i+1

getIndent :: IndentT ∈ fields => R fields -> Int
getIndent (fieldVal -> IndentT i) = i

indent :: Cofree ExprF (R fields) -> Cofree ExprF (R (IndentT ': fields))
indent e = ana indentAlg (e,IndentT 0)

indentAlg :: CoAlgebra (Cofree ExprF (R (IndentT ': fields))) ((Cofree ExprF (R fields)), IndentT)

indentAlg (r :< Scalar x, i) = (Identity i :& r) ::< Scalar x

indentAlg (r :< Vector elements, i) = (Identity i :& r) ::< (Vector $ zip elements $ repeat $ inc i)

indentAlg (r :< VectorView id a b, i) = (Identity i :& r) ::< VectorView id a b

indentAlg (r :< Addition a b, i) = (Identity i :& r) ::< Addition (a, inc i) (b, inc i)

indentAlg (r :< Multiplication a b, i) = (Identity i :& r) ::< Multiplication (a, inc i) (b, inc i)

indentAlg (r :< Apply a b, i) = (Identity i :& r) ::< Apply (a, inc i) (b, inc i)

indentAlg (r :< Lambda id t a, i) = (Identity i :& r) ::< Lambda id t (a, inc i)

indentAlg (r :< Variable id t, i) = (Identity i :& r) ::< Variable id t

indentAlg (r :< Map a b, i) = (Identity i :& r) ::< Map (a, inc i) (b, inc i)

indentAlg (r :< Reduce a b, i) = (Identity i :& r) ::< Reduce (a, inc i) (b, inc i)

indentAlg (r :< ZipWith a b c, i) = (Identity i :& r) ::< ZipWith (a, inc i) (b, inc i) (c, inc i)

printerAlg :: forall fields select . (IndentT ∈ fields, select ⊆ fields, RecAll Identity select Show) =>
    Proxy (R select) -> Algebra (Cofree ExprF (R fields)) String

printerAlg _ (r ::< Scalar x) = mkTabs r ++ "Scalar " ++ show x ++ ": " ++ show (rcast r :: R select) ++ "\n"

printerAlg _ (r ::< Vector elements) = mkTabs r ++ "Vector: " ++ show (rcast r :: R select) ++ "\n" ++ concat elements

printerAlg _ (r ::< VectorView id _ _) = mkTabs r ++ "VectorView " ++ id ++ ": " ++ show (rcast r :: R select) ++ "\n"

printerAlg _ (r ::< Addition a b) = mkTabs r ++ "Addition: " ++ show (rcast r :: R select) ++ "\n" ++ a ++ b

printerAlg _ (r ::< Multiplication a b) = mkTabs r ++ "Multiplication: " ++ show (rcast r :: R select) ++ "\n" ++ a ++ b

printerAlg _ (r ::< Apply a b) = mkTabs r ++ "Apply: " ++ show (rcast r :: R select) ++ "\n" ++ a ++ b

printerAlg _ (r ::< Lambda id _ a) = mkTabs r ++ "Lambda " ++ id ++ ": " ++ show (rcast r :: R select) ++ "\n" ++ a

printerAlg _ (r ::< Variable id _) = mkTabs r ++ "Variable " ++ id ++ ": " ++ show (rcast r :: R select) ++ "\n"

printerAlg _ (r ::< Map a b) = mkTabs r ++ "Map: " ++ show (rcast r :: R select) ++ "\n" ++ a ++ b

printerAlg _ (r ::< Reduce a b) = mkTabs r ++ "Reduce: " ++ show (rcast r :: R select) ++ "\n" ++ a ++ b

printerAlg _ (r ::< ZipWith a b c) = mkTabs r ++ "ZipWith: " ++ show (rcast r :: R select) ++ "\n" ++ a ++ b ++ c

printExpr :: (select ⊆ (IndentT : fields), RecAll Identity select Show) =>
     Proxy (R select) -> Cofree ExprF (R fields) ->  String
printExpr s e = cata (printerAlg s) $ indent e

mkTabs :: IndentT ∈ fields => R fields -> String
mkTabs r = replicate (getIndent r) '\t'