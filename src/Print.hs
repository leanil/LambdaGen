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

indent :: Cofree ExprF (R fields) -> Cofree ExprF (R (IndentT ': fields))
indent e = ana indentAlg (e,IndentT 0)

indentAlg :: CoAlgebra (Cofree ExprF (R (IndentT ': fields))) ((Cofree ExprF (R fields)), IndentT)
indentAlg (r :< node, i) = (Identity i :& r) ::< fmap (,inc i) node where
    inc (IndentT ind) = IndentT $ ind+1

printerAlg :: forall fields select . (IndentT ∈ fields, select ⊆ fields, RecAll Identity select Show) =>
    Proxy (R select) -> Algebra (Cofree ExprF (R fields)) String

printerAlg _ (r ::< Scalar x) = mkTabs r ++ "Scalar " ++ show x ++ ": " ++ show (rcast r :: R select) ++ "\n"

printerAlg _ (r ::< View i _ _) = mkTabs r ++ "VectorView " ++ i ++ ": " ++ show (rcast r :: R select) ++ "\n"

printerAlg _ (r ::< ScalarOp op a b) = mkTabs r ++ "ScalarOp(" ++ op:") " ++ show (rcast r :: R select) ++ "\n" ++ a ++ b

printerAlg _ (r ::< Apply a b) = mkTabs r ++ "Apply: " ++ show (rcast r :: R select) ++ "\n" ++ a ++ concat b

-- TODO: fix indentation of let names (and use templates overall)
printerAlg _ (r ::< Lambda vs b a) = mkTabs r ++ "Lambda" ++ (concatMap ((' ':) . fst) vs) ++ ": " ++ show (rcast r :: R select) ++ "\n" ++
                                    concatMap (\(n,v) -> n ++ " = " ++ v) b ++ a

printerAlg _ (r ::< Variable i _) = mkTabs r ++ "Variable " ++ i ++ ": " ++ show (rcast r :: R select) ++ "\n"

printerAlg _ (r ::< RnZ a b c) = mkTabs r ++ "RnZ: " ++ show (rcast r :: R select) ++ "\n" ++ a ++ b ++ concat c

printerAlg _ (r ::< ZipWithN a b) = mkTabs r ++ "ZipWithN: " ++ show (rcast r :: R select) ++ "\n" ++ a ++ concat b

printerAlg _ (r ::< Flip a b) = mkTabs r ++ "Flip " ++ show a ++ ": " ++ show (rcast r :: R select) ++ "\n" ++ b

printerAlg _ (r ::< Subdiv a b c) = mkTabs r ++ "Subdiv " ++ show a ++ " " ++ show b ++ ": " ++ show (rcast r :: R select) ++ "\n" ++ c

printerAlg _ (r ::< Flatten a b) = mkTabs r ++ "Flatten " ++ show a ++ ": " ++ show (rcast r :: R select) ++ "\n" ++ b

printExpr :: (select ⊆ (IndentT : fields), RecAll Identity select Show) =>
     Proxy (R select) -> Cofree ExprF (R fields) ->  String
printExpr s e = cata (printerAlg s) $ indent e

mkTabs :: IndentT ∈ fields => R fields -> String
mkTabs (fieldVal -> IndentT i) = replicate i '\t'