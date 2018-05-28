{-# LANGUAGE DataKinds, FlexibleContexts, LambdaCase, ScopedTypeVariables, TupleSections, TypeOperators, ViewPatterns #-}

module Transformation where

import Expr
import Recursion
import Replace
import Type
import Typecheck
import Utility (intersectWithIndex, setByIndex, takeByIndex, dropByIndex)
import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Foldable (fold)
import Data.Functor.Foldable (cata)
import Data.List (foldl')
import Data.Map.Strict (adjust, insert)
import Data.Maybe (fromJust, mapMaybe)
import Data.Monoid (Any(..))
import Data.Vinyl (type (∈))

partialApp :: RepPattern
partialApp = (MApp "app" (MLam "lam" (MStar "f")), MLam "lam" (MStar "f"))

partialAppTrans :: RepTransform '[]
partialAppTrans match = if x >= length (getParams l) then Nothing else Just $ foldl' (flip ($)) match updates where
    args = mGetArgList match "app"
    x = length args
    l = mGetNode match "lam"
    updates = [ insert "app__args" (Args []),
                insert "lam" (Node Nothing (Lambda par bind ())),
                insert "lam__args" (Args $ mGetArgList match "lam" ++ args) ]
    (par',par) = splitAt x (getParams l)
    bind = getBindings l ++ map (fmap $ const ()) (take x par')

zipZipSwap :: RepPattern
zipZipSwap = (MZipWithN "z1" (MLam "l1" (MZipWithN "z2" (MLam "l2" (MStar "f")))),
              MFlip "flip" (MZipWithN "z2" (MLam "l2" (MZipWithN "z1" (MLam "l1" (MStar "f"))))))

zipZipSwapTrans :: TypecheckT ∈ fields => RepTransform fields
zipZipSwapTrans = fmap (\x -> foldl' (flip ($)) x updates) . swapBaseCase ("z1","l1","z2","l2") ["f"] where
    updates = [ insert "flip" (Node Nothing $ Flip (0,1) ()) ]

zipRnZSwap :: RepPattern
zipRnZSwap = (MZipWithN "zip" (MLam "l1" (MRnZ "red" (MLam "lr" (MStar "f")) (MLam "lz" (MStar "g")))),
              MRnZ "red" (MLam "lu" (MZipWithN "zf" (MLam "lr" (MStar "f"))))
                         (MLam "lz" (MZipWithN "zip" (MLam "l1" (MStar "g")))))

zipRnZSwapTrans :: TypecheckT ∈ fields => RepTransform fields
zipRnZSwapTrans match = fmap (\x -> foldl' (flip ($)) x updates) $ swapBaseCase ("zip","l1","red","lz") ["f","g"] match where
    updates = [ insert "lu" (Node Nothing $ Lambda [("u1",ut),("u2",ut)] [] ()),
                insert "zf__args" (Args (u1:u2:freeM)),
                insert "lr" (Node Nothing lr {getParams = getParams lr ++ freeVars,
                                              getBindings = getBindings lr ++ getBindings l1}),
                adjust (\(Args x) -> Args (x ++ mGetArgList match "l1")) "lr__args" ]
    l1 = mGetNode match "l1"
    lr = mGetNode match "lr"
    ut = getType $ fst $ getAnnotedNode match "zip"
    (u1,u2) = (Nothing :< Variable "u1" ut, Nothing :< Variable "u2" ut)
    matches = findVarsInArgs (getParams l1) (mGetArgList match "red")
    freeM = dropByIndex (map fst matches) (mGetArgList match "zip")
    freeVars = dropByIndex (map fst matches) (getParams l1)

swapBaseCase :: TypecheckT ∈ fields => (String,String,String,String) -> [String] -> Match fields -> Maybe (Match fields)
swapBaseCase (o1,l1',o2,l2') f match = if varOccur then Nothing else Just $ foldl' (flip ($)) match updates where
    l1 = mGetNode match l1'
    l2 = mGetNode match l2'
    l1_params = getParams l1
    o2_args = mGetArgList match o2
    updates = [ insert l1' (Node Nothing l1 {getParams = cv}),
                insert l2' (Node Nothing l2 {getParams = cx}),
                insert (o1 ++ "__args") (Args cm),
                insert (o2 ++ "__args") (Args cw) ]
    matches = findVarsInArgs l1_params o2_args
    (cv,cm,cx,cw) = makeChanges matches l1_params (mGetArgList match o1) (getParams l2) o2_args
    matchVars = takeByIndex (map fst matches) (map fst l1_params)
    rest = dropByIndex (map snd matches) o2_args
    varOccur = getAny $ fold $ findVarsInSubtrees matchVars (map (mGetSubtree match) f ++ mGetArgList match l1') ++
                               findVarsInSubtrees (map fst $ l1_params) rest

findVarInSubtree :: String -> ExprOpt fields -> Any
findVarInSubtree v = cata alg where
    alg :: Algebra (ExprOpt fields) Any
    alg (_ ::< Variable n _) = Any (v == n)
    alg (_ ::< node) = fold node

findVarsInSubtrees :: [String] -> [ExprOpt fields] -> [Any]
findVarsInSubtrees vars trees = concatMap (\v -> map (findVarInSubtree v) trees) vars

findVarsInArgs :: [(String,Type)] -> [ExprOpt fields] -> [(Int,Int)]
findVarsInArgs (map fst -> v) w' = let w = mapMaybe (\case _ :< Variable n _ -> Just n
                                                           _ -> Nothing) w'
                                   in intersectWithIndex v w

-- higher lambda vars, higher arguments, lower vars, lower args -> ((re-occuring vars,rest of w), lists of changes for each input list)
makeChanges :: forall fields. TypecheckT ∈ fields => [(Int,Int)] -> [(String,Type)] -> [ExprOpt fields] -> [(String,Type)] -> [ExprOpt fields] -> 
                                      ([(String,Type)],[ExprOpt fields],[(String,Type)],[ExprOpt fields])
makeChanges matches v m x w =
    let (cv,cm,cx,cw) = helper matches in
        (setByIndex cv v, setByIndex cm m, setByIndex cx x, setByIndex cw w) where
    helper :: [(Int,Int)] -> ([(Int,(String,Type))],[(Int,ExprOpt fields)],[(Int,(String,Type))],[(Int,ExprOpt fields)])
    helper [] = ([],[],[],[])
    helper ((i,j):xs) = let (a,b,c,d) = helper xs
                            (FPower t (d0:_:ds)) = getType $ fromJust $ getAnnot $ m !! i
                            v' = (fst (v !! i), power' t (d0:ds)) in
                        ((i,x !! j):a, (i,Nothing :< Variable (fst v') (snd v')):b, (j,v'):c, (j, Nothing :< Flip (0,1) (m !! i)):d)

-- mapFusePat, mapFuseRep :: PExpr
-- mapFusePat = pMap (pStar "l1") (pMap (pStar "l2") (pStar "v"))
-- mapFuseRep = pMap (pComp (pStar "l1") (pStar "l2")) (pStar "v")