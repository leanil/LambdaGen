module FunctionalTest where

import Expr
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

v = var "v" (power double (size 3))

funcTest6 =
    mkMap
        (lam v
            (mkReduce 
                (lam x (lam y (add x y )))
                (mkZipWith
                    (lam x (lam y (mul x y )))
                    v
                    (vecView "vec" 3))))
        (vec [
            vec [scl 1, scl 2, scl 3],
            vec [scl 4, scl 5, scl 6]])

a = vecView "a" 3
b = vecView "b" 3

funcTest7 =
    mkZipWith
        (lam x (lam y (add x y )))
        (mkMap
            (lam x (mul (scl 3) x))
            a)
        b
                

    


