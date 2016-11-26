module LinAlg where

import Expr
import Type

sclAdd :: Expr
sclAdd = let x = var "x" double
             y = var "y" double
         in  lam x (lam y (add x y))

sclMul :: Expr
sclMul = let x = var "x" double
             y = var "y" double
         in  lam x (lam y (mul x y))

sclVecMul :: Int -> Expr
sclVecMul d = let x = var "x" double
                  v = var "v" (power double (dim d))
              in  lam x (lam v
                      (mkMap (app sclMul x) v))

vecAdd :: Int -> Expr
vecAdd d = let v1 = var "v1" (power double (dim d))
               v2 = var "v2" (power double (dim d))
           in  lam v1 (lam v2
                   (mkZipWith sclAdd v1 v2))

dotProd :: Int -> Expr
dotProd d = let v1 = var "v1" (power double (dim d))
                v2 = var "v2" (power double (dim d))
            in  lam v1 (lam v2
                    (mkReduce sclAdd
                        (mkZipWith sclMul v1 v2)))

outerProd :: Int -> Int -> Expr
outerProd a b = let x  = var "x" double
                    v1 = var "v1" (power double (dim a))
                    v2 = var "v2" (power double (dim b))
                in  lam v1 (lam v2
                        (mkMap 
                            (lam x $ app (app (sclVecMul b) x) v2)
                            v1))

matAdd :: Int -> Int -> Expr
matAdd a b = let m1 = var "m1" (power (power double (dim b)) (dim a))
                 m2 = var "m2" (power (power double (dim b)) (dim a))
             in  lam m1 (lam m2 (mkZipWith (vecAdd b) m1 m2))

matMul :: Int -> Int -> Int -> Expr
matMul a b c = let m1 = var "m1" (power (power double (dim a)) (dim b))
                   m2 = var "m2" (power (power double (dim c)) (dim b))
               in lam m1 (lam m2
                      (mkReduce (matAdd a c)
                          (mkZipWith (outerProd a c) m1 m2)))