{-# LANGUAGE OverloadedStrings #-}

module FunctionalTest where

import Expr
import LinAlg
import Type
import Data.Text (Text)

x,y,a,b :: Expr0
x = var "x" double
y = var "y" double
a = vecView "a" [3]
b = vecView "b" [3]

type Test = (Expr0, Text)
test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11 :: Test

test1 = (mul (add (scl 2) (scl 3)) (scl 4), "20")

test2 = (
    app
        (lam [x,y] (add x y ))
        [(scl 5),
        (mul (scl 4) (scl 3))],
    "17")

test3 = (
    app
        (lam [y] (mul y y))
        [app
            (lam [x] (add x x))
            [scl 2]],
    "16")

test4 = let v1 = var "v1" (power double (dim 3))
            v2 = var "v2" (power double (dim 3))
        in
            (
            mkMap
                (lam [v2]
                    (bind v1 (vecView "vec" [3])
                        (mkReduce sclAdd
                            (mkZipWith sclMul v1 v2))))
                (vecView "mat" [2,3]),
            "{50,122}")

test5 = (
    mkReduce
        (vecAdd 2)
        (mkZipWith
            (sclVecMul 2)
            (vecView "vec" [3])
            (transpose [2,1] $ vecView "mat" [2,3])),
    "{50,122}")



test6 = (
    app (outerProd 3 3) [a,b],
    "{{1,2,3},{2,4,6},{3,6,9}}")

test7 = (
    app (matMul 2 3 2) [vecView "mat" [2,3], transpose [2,1] $ vecView "mat" [3,2]],
    "{{22,28},{49,64}}")

test8 = (
    let m    = var "m" (power (power double (dim 3)) (dim 4))
        prod = lam [m] (mkReduce
                        (matAdd 3 2)
                        (mkZipWith (outerProd 3 2) m (vecView "mat8" [4,2])))
    in  mkMap prod (transpose [1,3,2] $ vecView "tens" [2,3,4]),
    "{{{50,60},{114,140},{178,220}},{{242,300},{306,380},{370,460}}}")

test9 = (
    mkZipWith
        (lam [x,y] (add x y ))
        (mkMap
            (lam [x] (mul (scl 3) x))
            a)
        b,
    "{4,8,12}")

test10 = (
    mkMap
        (app
            (lam [x,y] 
                (add (mul y (scl 0)) x))
            [scl 3])
        a,
    "{3,3,3}")

test11 = (
    mkMap
        (app sclMul [scl 2])
        (mkMap
            (lam [x] (add x (scl 1)))
            a),
    "{4,6,8}")

funcTests :: [Test]
funcTests = [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11]
