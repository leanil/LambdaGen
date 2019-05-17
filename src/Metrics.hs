{-# LANGUAGE DataKinds, FlexibleContexts, GeneralizedNewtypeDeriving, TupleSections, TypeOperators, ViewPatterns #-}

module Metrics where

import Expr
import Recursion
import Control.Comonad.Cofree (Cofree(..))
import Control.Monad.State (State, evalState, get, modify)
import Data.Foldable (fold, foldl', foldMap)
import Data.Functor.Foldable (ana, cata)
import Data.Map.Strict (Map, (!), empty, insert)
import Data.Monoid (Sum(..), (<>))
import Data.Vinyl (type (∈))

-- newtype SubtreeSize = SubtreeSize (Sum Int) deriving (Show)

-- instance Semigroup SubtreeSize where
--     (SubtreeSize a) <> (SubtreeSize b) = SubtreeSize (a <> b)
-- instance Monoid SubtreeSize where
--     mempty = SubtreeSize (Sum 0)

-- subtreeSizeAlg :: Algebra (Expr fields) SubtreeSize
-- subtreeSizeAlg (_ ::< node) = SubtreeSize (Sum 1) <> fold node

-- getSubtreeSize :: SubtreeSize ∈ fields => R fields -> Int
-- getSubtreeSize (fieldVal -> SubtreeSize (Sum x)) = x

newtype NodeId = NodeId Int deriving Show

getNodeId :: NodeId ∈ fields => R fields -> Int
getNodeId (fieldVal -> NodeId x) = x

assignNodeId :: Expr fields -> Expr (NodeId ': fields)
assignNodeId = (`evalState` 0) . cataM (annotateM alg) where
    alg node = do
        nodeId <- get
        modify (+1)
        return $ NodeId nodeId

makeSymbolsUnique :: Expr fields -> Expr (NodeId ': fields)
makeSymbolsUnique = ana alg . (,empty) . assignNodeId where
    alg :: NodeId ∈ fields => CoAlgebra (Expr fields) (Expr fields,Map String String)
    alg (r :< Lambda (unzip -> (vs,ts)) (unzip -> (bs,es)) body, m) = r ::< Lambda vars' binds' (body, m') where
        i = show $ getNodeId r
        m' = foldl' (\a b -> insert b (b ++ i) a) m (vs ++ bs)
        vars' = zip (map (++i) vs) ts
        binds' = zip (map (++i) bs) (map (,m') es)
    alg (r :< Variable name t, m) = r ::< Variable (m ! name) t
    alg (r :< node,m) = r ::< zipExprF node (repeat m)

newtype LetId = LetId { getLetId :: Maybe String } deriving Show
assignLetId :: Expr fields -> Expr (LetId ': fields)
assignLetId = ana (annotateAna alg) . (,Nothing) where
    alg (_ :< Lambda params binds body, name) = LetId name ::< Lambda params (map (\(name, exp) -> (name, (exp, Just name))) binds) (body, Nothing)
    alg (_ :< node, name) = LetId name ::< fmap (,Nothing) node