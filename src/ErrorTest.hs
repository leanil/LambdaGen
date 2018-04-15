module ErrorTest where 

import Expr
import Type

x,y,v1,v2 :: Expr0
x = var "x" double
y = var "y" double
v1 = vecView "vec" [1]
v2 = vecView "vec2" [2]

typeError2,typeError3,typeError4,typeError5,typeError6,typeError7,typeError8,typeError9 :: Expr0

typeError2 = add (scl 0) (vecView "vec" [1])

typeError3 = app (scl 0) [scl 0]

typeError4 =
    app
        (lam [x] x)
        [vecView "vec" [1]]

typeError5 = mkMap (scl 0) (scl 0)

typeError6 =
    mkMap
        (lam [var "x" (power double (dim 1))] (scl 0))
        v1

typeError7 = mkReduce (lam [x] (scl 0)) (scl 0)

typeError8 =
    mkReduce
        (lam [var "x" (power double (dim 1))]
            (lam [y] (scl 0)))
        v1

typeError9 =
    mkZipWith
        (lam [var "x" (power double (dim 1))]
            (lam [y] (scl 0)))
        v2
        v1

typeErrors :: [Expr0]
typeErrors = [typeError2, typeError3, typeError4, typeError5, typeError6, typeError7, typeError8, typeError9]
