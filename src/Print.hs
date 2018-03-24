{-# LANGUAGE DataKinds, FlexibleContexts, ScopedTypeVariables, TupleSections, TypeOperators, ViewPatterns #-}

module Print where

import Expr
import Recursion
import Control.Comonad.Cofree (Cofree(..))
import Data.Functor.Foldable (ana, cata)
import Data.Proxy (Proxy)
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
indentAlg (r :< node, i) = (Identity i :& r) ::< fmap (,inc i) node

printerAlg :: forall fields select . (IndentT ∈ fields, select ⊆ fields, RecAll Identity select Show) =>
    Proxy (R select) -> Algebra (Cofree ExprF (R fields)) String

printerAlg _ (r ::< Scalar x) = mkTabs r ++ "Scalar " ++ show x ++ ": " ++ show (rcast r :: R select) ++ "\n"

printerAlg _ (r ::< Vector e) = mkTabs r ++ "Vector: " ++ show (rcast r :: R select) ++ "\n" ++ concat e

printerAlg _ (r ::< VectorView i _ _) = mkTabs r ++ "VectorView " ++ i ++ ": " ++ show (rcast r :: R select) ++ "\n"

printerAlg _ (r ::< Addition a b) = mkTabs r ++ "Addition: " ++ show (rcast r :: R select) ++ "\n" ++ a ++ b

printerAlg _ (r ::< Multiplication a b) = mkTabs r ++ "Multiplication: " ++ show (rcast r :: R select) ++ "\n" ++ a ++ b

printerAlg _ (r ::< Apply a b) = mkTabs r ++ "Apply: " ++ show (rcast r :: R select) ++ "\n" ++ a ++ concat b

printerAlg _ (r ::< Lambda v a) = mkTabs r ++ "Lambda" ++ (concatMap ((' ':) . fst) v) ++ ": " ++ show (rcast r :: R select) ++ "\n" ++ a

printerAlg _ (r ::< Variable i _) = mkTabs r ++ "Variable " ++ i ++ ": " ++ show (rcast r :: R select) ++ "\n"

printerAlg _ (r ::< Map a b) = mkTabs r ++ "Map: " ++ show (rcast r :: R select) ++ "\n" ++ a ++ b

printerAlg _ (r ::< Reduce a b) = mkTabs r ++ "Reduce: " ++ show (rcast r :: R select) ++ "\n" ++ a ++ b

printerAlg _ (r ::< ZipWith a b c) = mkTabs r ++ "ZipWith: " ++ show (rcast r :: R select) ++ "\n" ++ a ++ b ++ c

printerAlg _ (r ::< Compose a b) = mkTabs r ++ "Compose: " ++ show (rcast r :: R select) ++ "\n" ++ a ++ b

printExpr :: (select ⊆ (IndentT : fields), RecAll Identity select Show) =>
     Proxy (R select) -> Cofree ExprF (R fields) ->  String
printExpr s e = cata (printerAlg s) $ indent e

mkTabs :: IndentT ∈ fields => R fields -> String
mkTabs r = replicate (getIndent r) '\t'