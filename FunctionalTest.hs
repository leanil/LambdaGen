module FunctionalTest where

import Expr
import LinAlg
import Type

x = var "x" double
y = var "y" double

test1 = mul (add (scl 2) (scl 3)) (scl 4)

test2 = 
    app
        (app
            (lam x (lam y (add x y )))
            (scl 5))
        (mul (scl 4) (scl 3))

test3 =
    app
        (lam y (mul y y))
        (app
            (lam x (add x x))
            (scl 2))

test4 = vec [test1, test2, test3]

test5 =
    mkMap
        (app
            (lam x (lam y (add x y)))
            (scl 5))
        test4

-- {50, 122}
test6 =
    mkMap
        (app (dotProd 3) (vecView "vec" [3]))
        (vecView "mat" [2,3])

-- {50, 122}
test7 =
    mkReduce
        (vecAdd 2)
        (mkZipWith
            (sclVecMul 2)
            (vecView "vec" [3])
            (transpose [2,1] $ vecView "mat" [2,3]))

a = vecView "a" [3]
b = vecView "b" [3]

test8 =
    app (app (outerProd 3 3) a) b

-- {{22,28},{49,64}}
test9 =
    app (app (matMul 2 3 2) (vecView "mat" [2,3])) (transpose [2,1] $ vecView "mat" [3,2])

-- {{{50,60},{114,140},{178,220}},{{242,300},{306,380},{370,460}}}
test10 =
    let m    = var "m" (power (power double (dim 3)) (dim 4))
        prod = lam m (mkReduce
                        (matAdd 3 2)
                        (mkZipWith (outerProd 3 2) m (vecView "mat8" [4,2])))
    in  mkMap prod (transpose [1,3,2] $ vecView "tens" [2,3,4])

-- {4,8,12}
test11 =
    mkZipWith
        (lam x (lam y (add x y )))
        (mkMap
            (lam x (mul (scl 3) x))
            a)
        b

funcTests = [test1, test2, test3, test6, test7, test8, test9, test10, test11]

    


