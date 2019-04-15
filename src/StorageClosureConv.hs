{-# LANGUAGE DataKinds, DuplicateRecordFields, FlexibleContexts, TupleSections, TypeOperators, ViewPatterns #-}

module StorageClosureConv where

import Expr
import Metrics
import Naming
import Parallel
import Recursion
import Type
import Typecheck
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree(..))
import Control.Monad.Writer.Lazy (Writer, runWriter, tell)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Monoid ((<>))
import Data.Vinyl (rput, type (∈))
import Data.Vinyl.Functor (Identity(..))
import Data.Foldable (fold)
import Data.Functor.Foldable (ana, cata, project, unfix)

data Storage = Storage { memId :: String, shape :: Type } deriving Show
newtype OwnStorage = OwnStorage { ownStorage :: Bool }

storageAlg :: (TypecheckT ∈ fields, NodeId ∈ fields, LetId ∈ fields) => MCoAlgebra (Cofree ExprF Bool) (Writer [Storage]) (Expr fields, Bool)

storageAlg (_ :< node, tag) | isLeafNode node = return $ tag ::< castLeaf node
storageAlg (_ :< node@ScalarOp{}, _) = return $ True ::< fmap (,True) node
storageAlg (r :< node, tag) = do
    tell $ case (getType r, tag) of 
        (ty@FPower{}, True) -> [Storage (getMemId r) ty]
        otherwise           -> []
    case node of
        Apply{} -> return $ tag ::< zipExprF node (False : repeat True)
        Lambda{} -> return $ True ::< zipExprF node (False : repeat True)
        ZipWithN{} -> return $ tag ::< zipExprF node (False : repeat True)
        RnZ _ zipper _ -> do
            tell $ [Storage (getMemId r ++ "_tmp") (to $ unfix $ getType $ extract zipper)]
            return $ tag ::< zipExprF node (False : False : repeat True)

getMemId :: (NodeId ∈ fields, LetId ∈ fields) => R fields -> String
getMemId r = case fieldVal r of
    LetId (Just name) -> name
    LetId Nothing -> resultTensor $ getNodeId r

assignStorage :: (TypecheckT ∈ fields, NodeId ∈ fields, LetId ∈ fields) => Expr fields -> (Expr (OwnStorage ': fields), [Storage])
assignStorage = runWriter . anaM (annotateAnaM2 OwnStorage storageAlg) . (,True)