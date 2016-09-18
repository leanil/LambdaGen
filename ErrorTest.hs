module ErrorTest where 

import Expr
import Type

x = var "x" double
y = var "y" double

typeError1 = vec [scl 0, vecView "vec" 1]

typeError2 = add (scl 0) (vecView "vec" 1)

typeError3 = app (scl 0) (scl 0)

typeError4 =
    app
        (lam x x)
        (vecView "vec" 1)

typeError5 = mkMap (scl 0) (scl 0)

typeError6 =
    mkMap
        (lam (var "x" (power double (size 1))) (scl 0))
        (vecView "vec" 1)

typeError7 = mkReduce (lam x (scl 0)) (scl 0)

typeError8 =
    mkReduce
        (lam (var "x" (power double (size 1)))
            (lam y (scl 0)))
        (vec [x])

typeError9 =
    mkZipWith
        (lam (var "x" (power double (size 1)))
            (lam y (scl 0)))
        (vec [scl 0, scl 0])
        (vec [x])

typeErrors = vec [typeError1, typeError2, typeError3, typeError4, typeError5, typeError6, typeError7, typeError8, typeError9]
