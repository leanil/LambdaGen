module Fusion where

import Replace

mapFusePat = pMap "m1" (pStar "l1") (pMap "m2" (pStar "m2") (pStar "v"))

-- mapFuseRep = 