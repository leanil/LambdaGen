module PerformanceTest where

import Expr
import LinAlg
import Replace
import Transformation
import Type
import Typecheck
import Utility

matVecMul, vecSum, tensorProd, matMatMul, matMatMulTrans :: Expr0
matVecMul =
    let a = 2^13
        b = 2^13 in
    mkMap
        (app (dotProd b) [vecView "vec" [b]])
        (vecView "mat" [a,b])
    
vecSum =
    let a = 10^8 in
    mkReduce
        sclAdd
        (var "x" double)
        (vecView "a" [a])

tensorProd =
    let m    = var "m" (power double [400,300])
        prod = lam [m] (mkRnZ (matAdd 300 200) (outerProd 300 200) [m,vecView "big_mat" [400,200]])
    in  mkMap prod (transpose [1,3,2] $ vecView "big_tens" [200,300,400])

matMatMul =
    let size = 512
        u = var "u" (power double [size])
        v = var "v" (power double [size]) in
    mkMap
        (lam [u]
            (mkMap
                (lam [v]
                    (mkRnZ sclAdd sclMul [u,v]))
                (mkFlip (0,1) (vecView "M2" [size,size]))))
        (vecView "M1" [size,size])

matMatMulTrans = replace1TopDown zipRnZSwap zipRnZSwapTrans $ typecheck' matMatMul