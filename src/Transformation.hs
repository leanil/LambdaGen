{-# LANGUAGE DataKinds, FlexibleContexts, LambdaCase, ScopedTypeVariables, TupleSections, TypeOperators, ViewPatterns #-}

module Transformation where

import Expr
import Recursion
import Replace
import Type
import Typecheck
import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Either (partitionEithers)
import Data.Foldable (fold)
import Data.Functor.Foldable (cata)
import Data.List (foldl', sort, sortBy)
import Data.Map.Strict ((!), insert)
import Data.Monoid (Any(..))
import Data.Vinyl (Rec(RNil), type (∈))

partialApp :: RepPattern
partialApp = (MApp "app" (MLam "lam" (MStar "f")), MLam "lam" (MStar "f"))

partialAppTrans :: RepTransform '[]
partialAppTrans match = if length args >= length (getParams oldLam) then Nothing
    else Just $ insert "lam" (RNil :< newLam) match where
        args = getArgs $ mGetNode match "app"
        oldLam = mGetNode match "lam"
        newLam = moveArgs oldLam args
        moveArgs l [] = l
        moveArgs (Lambda ((v,_):ps) bs b) (a:as) = moveArgs (Lambda ps ((v,a):bs) b) as

zipZipSwap :: RepPattern
zipZipSwap = (MZipWithN "z1" (MLam "l1" (MZipWithN "z2" (MLam "l2" (MStar "f")))),
              MFlip "_" (0,1) (MZipWithN "z2" (MLam "l2" (MZipWithN "z1" (MLam "l1" (MStar "f"))))))

zipZipSwapTrans :: TypecheckT ∈ fields => RepTransform fields
zipZipSwapTrans match = if varOccur then Nothing else Just $ foldl' (flip ($)) match inserts where
    (rz1 :< z1) = match ! "z1"
    (rl1 :< l1) = match ! "l1"
    (rz2 :< z2) = match ! "z2"
    (rl2 :< l2) = match ! "l2"
    inserts = zipWith insert ["l1","z1","l2","z2"] $
        [(rl1 :< l1 {getParams = cv}), (rz1 :< z1 {getVecs = cm}), (rl2 :< l2 {getParams = cx}), (rz2 :< z2 {getVecs = cw})]
    ((matchVars,rest),cv,cm,cx,cw) = makeChanges (getParams l1) (getVecs z1) (getParams l2) (getVecs z2)
    varOccur = getAny $ fold $ findVarsInSubtrees matchVars (match ! "f" : map snd (getBindings l1)) ++ 
                               findVarsInSubtrees (map fst $ getParams l1) rest
    findVarsInSubtrees vars trees = concatMap (\v -> map (findVarInSubtree v) trees) vars

findVarInSubtree :: String -> Expr fields -> Any
findVarInSubtree v = cata alg where
    alg :: Algebra (Expr fields) Any
    alg (_ ::< Variable n _) = Any (v == n)
    alg (_ ::< node) = fold node

-- higher lambda vars, higher arguments, lower vars, lower args -> ((re-occuring vars,rest of w), lists of changes for each input list)
makeChanges :: forall fields. TypecheckT ∈ fields => [(String,Type)] -> [Expr fields] -> [(String,Type)] -> [Expr fields] -> 
                                      (([String],[Expr fields]),[(String,Type)],[Expr fields],[(String,Type)],[Expr fields])
makeChanges v m x w =
    let (a,b) = matches
        (cv,cm,cx,cw) = create b in
        (a,applyChanges cv v, applyChanges cm m, applyChanges cx x, applyChanges cw w) where
    create :: [(Int,Int)] -> ([(Int,(String,Type))],[(Int,Expr fields)],[(Int,(String,Type))],[(Int,Expr fields)])
    create [] = ([],[],[],[])
    create ((i,j):xs) = let (a,b,c,d) = create xs
                            (FPower t (d0:_:ds)) = getType $ getAnnot $ m !! i
                            v' = (fst (v !! i), power' t (d0:ds))
                            dummyAnnotation = getAnnot $ head m in
                        ((i,x !! j):a, (i,dummyAnnotation :< Variable (fst v') (snd v')):b, (j,v'):c, (j, dummyAnnotation :< Flip (0,1) (m !! i)):d)
    matches :: (([String],[Expr fields]),[(Int,Int)])
    matches = let v' = sort $ zip (map fst v) [0..]
                  w' = partitionEithers $ map (\case _ :< Variable n _ -> Left n
                                                     a -> Right a) w
              in mapPair (,snd w') id $ findMatches v' $ sort $ zip (fst w') [0..]
    findMatches :: [(String,Int)] -> [(String,Int)] -> ([String],[(Int,Int)])
    findMatches ((a,i):xs) ((b,j):ys) | a == b = mapPair (a:) ((i,j):) $ findMatches xs ys
                                      | otherwise = findMatches xs ys
    findMatches _ _ = ([],[])
    mapPair :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
    mapPair f g (a,b) = (f a, g b)
    applyChanges :: [(Int,a)] -> [a] -> [a]
    applyChanges change = applyHelper 0 (sortBy (\(a,_) (b,_) -> compare a b) change) where
        applyHelper _ [] a = a
        applyHelper z y@((z',a):xs) (b:bs)
            | z == z' = a : applyHelper (z+1) xs bs
            | otherwise = b : applyHelper (z+1) y bs

-- mapFusePat, mapFuseRep :: PExpr
-- mapFusePat = pMap (pStar "l1") (pMap (pStar "l2") (pStar "v"))
-- mapFuseRep = pMap (pComp (pStar "l1") (pStar "l2")) (pStar "v")