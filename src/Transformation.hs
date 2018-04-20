module Transformation where

import Expr
import Replace
import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Map.Strict (insert)
import Data.Vinyl (Rec(RNil))

partialAppPat, partialAppRep :: PExpr
partialAppPat = MApp "app" (MLam "lam" (MStar "f"))
partialAppRep = MLam "lam" (MStar "f")
partialAppConstraint :: RepConstraint
partialAppConstraint match = argCnt < paramCnt where
    argCnt = length $ getArgs $ mGetNode match "app"
    paramCnt = length $ getParams $ mGetNode match "lam"
partialAppTransform :: RepTransform
partialAppTransform match = insert "lam" (RNil :< newLam) match where
    newLam = moveArgs (mGetNode match "lam") (getArgs $ mGetNode match "app")
    moveArgs l [] = l
    moveArgs (Lambda ((v,_):ps) bs b) (a:as) = moveArgs (Lambda ps ((v,a):bs) b) as

-- mapFusePat, mapFuseRep :: PExpr
-- mapFusePat = pMap (pStar "l1") (pMap (pStar "l2") (pStar "v"))
-- mapFuseRep = pMap (pComp (pStar "l1") (pStar "l2")) (pStar "v")