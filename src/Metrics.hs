{-# LANGUAGE DataKinds, FlexibleContexts, GeneralizedNewtypeDeriving, TypeOperators, ViewPatterns #-}

module Metrics where

import Expr
import Recursion
import Control.Comonad.Cofree (Cofree(..))
import Data.Foldable (fold, foldMap)
import Data.Functor.Foldable (ana, cata)
import Data.Monoid (Sum(..), (<>))
import Data.Vinyl (type (∈))

newtype SubtreeSize = SubtreeSize (Sum Int) deriving (Monoid, Show)

subtreeSizeAlg :: Algebra (Expr fields) SubtreeSize

subtreeSizeAlg (_ ::< node) = SubtreeSize (Sum 1) <> fold node

getSubtreeSize :: SubtreeSize ∈ fields => R fields -> Int
getSubtreeSize (fieldVal -> SubtreeSize (Sum x)) = x

newtype NodeId = NodeId Int deriving Show

nodeIdAlg :: SubtreeSize ∈ fields => CoAlgebra (Cofree ExprF NodeId) ((Expr fields), Int)

nodeIdAlg (_ :< node, x) = NodeId x ::< zipExprF node xs where
    xs = map (+(x+1)) $ scanl (+) 0 $ foldMap (\(r :< _) -> [getSubtreeSize r]) node

getNodeId :: NodeId ∈ fields => R fields -> Int
getNodeId (fieldVal -> NodeId x) = x

assignNodeId :: Expr fields -> Expr (NodeId ': SubtreeSize ': fields)
assignNodeId expr = ana (annotateAna nodeIdAlg) $ (cata (annotate subtreeSizeAlg) expr, 1)