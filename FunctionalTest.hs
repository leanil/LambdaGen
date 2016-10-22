module FunctionalTest where

import Expr
import LinAlg
import Type

x = var "x" double
y = var "y" double

funcTest1 = mul (add (scl 2) (scl 3)) (scl 4)

funcTest2 = 
    app
        (app
            (lam x (lam y (add x y )))
            (scl 5))
        (mul (scl 4) (scl 3))

funcTest3 =
    app
        (lam y (mul y y))
        (app
            (lam x (add x x))
            (scl 2))

funcTest4 = vec [funcTest1, funcTest2, funcTest3]

funcTest5 =
    mkMap
        (app
            (lam x (lam y (add x y)))
            (scl 5))
        funcTest4

funcTest6 =
    mkMap
        (app (dotProd 3) (vecView "vec" [3]))
        (vecView "mat" [2,3])

funcTest6T =
    mkReduce
        (vecAdd 2)
        (mkZipWith
            (sclVecMul 2)
            (vecView "vec" [3])
            (transpose [2,1] $ vecView "mat" [2,3]))

a = vecView "a" [3]
b = vecView "b" [3]

funcTest7 =
    app (app (outerProd 3 3) a) b

funcTest8 =
    app (app (matMul 2 3 2) (transpose [2,1] $ vecView "mat" [2,3])) (vecView "mat" [3,2])

funcTest9 =
    mkZipWith
        (lam x (lam y (add x y )))
        (mkMap
            (lam x (mul (scl 3) x))
            a)
        b
                

    


