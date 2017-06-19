module Input where

import Expr
import LinAlg
import Type

input =
    mkMap
        (app (dotProd 3) (vecView "vec" [3]))
        (vecView "mat" [2,3])