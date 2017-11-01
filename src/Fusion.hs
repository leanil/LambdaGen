module Fusion where

import Replace

mapFusePat = pMap (pStar "l1") (pMap (pStar "l2") (pStar "v"))

mapFuseRep = pMap (pComp (pStar "l1") (pStar "l2")) (pStar "v")