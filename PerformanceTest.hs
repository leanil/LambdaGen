module PerformanceTest where

import Expr
import LinAlg
import Type

matVecMul =
    let a = 16
        b = 10^7 in
    mkMap
        (app (dotProd b) (vecView "vec" [b]))
        (vecView "mat" [a,b])
    
vecSum =
    let a = 10^8 in
    mkReduce
        sclAdd
        (vecView "vec" [a])