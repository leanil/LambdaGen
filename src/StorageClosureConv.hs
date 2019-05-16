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
import Data.Text (Text, append, pack)

data Storage = Storage { memId :: Text, shape :: Type } deriving Show
newtype OwnStorage = OwnStorage { ownStorage :: Bool }

storageAlg :: (TypecheckT ∈ fields, NodeId ∈ fields, LetId ∈ fields) => MCoAlgebra (Cofree ExprF Bool) (Writer [Storage]) (Expr fields, Bool)

storageAlg (_ :< node, tag) | isLeafNode node = return $ tag ::< castLeaf node
storageAlg (r :< node, tag) = do
    tell $ case (getType r, tag) of 
        (ty@FPower{}, True) -> [Storage (getMemId r) ty]
        otherwise           -> []
    case node of
        ScalarOp{} -> return $ tag ::< zipExprF node (repeat True)
        Apply{} -> return $ tag ::< zipExprF node (True : repeat True)
        Lambda{} -> return $ True ::< zipExprF node (curried : repeat True) where
            curried = case getType r of FArrow _ FArrow{} -> True; _ -> False
        ZipWithN{} -> return $ tag ::< zipExprF node (repeat True)
        RnZ _ _ vecs -> do
            tell $ [Storage (getMemId r `append` tmpSuffix) (raiseToPower (getType r) tmpSize)] 
            return $ tag ::< zipExprF node (repeat True) where
                tmpSize = size $ getType $ extract $ head vecs

getMemId :: (NodeId ∈ fields, LetId ∈ fields) => R fields -> Text
getMemId r = case fieldVal r of
    LetId (Just name) -> pack name
    LetId Nothing -> tResultTensor $ getNodeId r

assignStorage :: (TypecheckT ∈ fields, NodeId ∈ fields, LetId ∈ fields) => Expr fields -> (Expr (OwnStorage ': fields), [Storage])
assignStorage = runWriter . anaM (annotateAnaM2 OwnStorage storageAlg) . (,True)