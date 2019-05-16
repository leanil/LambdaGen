module LinAlg where

import Expr
import Type

sclAdd :: Expr0
sclAdd = let x = var "x" double
             y = var "y" double
         in  lam [x,y] (add x y)

sclMul :: Expr0
sclMul = let x = var "x" double
             y = var "y" double
         in  lam [x,y] (mul x y)

sclVecMul :: Int -> Expr0
sclVecMul d = let x = var "x" double
                  v = var "v" (power double [d])
              in  lam [x,v] (mkMap (app sclMul [x]) v)

vecAdd :: Int -> Expr0
vecAdd d = let v1 = var "v1" (power double [d])
               v2 = var "v2" (power double [d])
           in  lam [v1,v2] (mkZipWith sclAdd v1 v2)

dotProd :: Int -> Expr0
dotProd d = let v1 = var "v1" (power double [d])
                v2 = var "v2" (power double [d])
            in  lam [v1,v2]
                    (mkRnZ sclAdd sclMul [v1,v2])

outerProd :: Int -> Int -> Expr0
outerProd a b = let x  = var "x" double
                    v1 = var "v1" (power double [a])
                    v2 = var "v2" (power double [b])
                in  lam [v1,v2]
                        (mkMap 
                            (lam [x] $ app (sclVecMul b) [x,v2])
                            v1)

matAdd :: Int -> Int -> Expr0
matAdd a b = let m1 = var "m1" (power double [a,b])
                 m2 = var "m2" (power double [a,b])
             in  lam [m1,m2] (mkZipWith (vecAdd b) m1 m2)

matMul :: Int -> Int -> Int -> Expr0
matMul a b c = let v  = var "v" (power double [b])
                   m1 = var "m1" (power double [a,b])
                   m2 = var "m2" (power' double [(c,1),(b,c)])
               in lam [m1,m2]
                      (mkMap
                          (lam [v] (mkMap
                                     (app (dotProd b) [v])
                                     m2))
                          m1)

matMul2 :: Int -> Int -> Int -> Expr0
matMul2 a b c = let m1 = var "m1" (power double [a,b])
                    m2 = var "m2" (power double [c,b])
                in lam [m1,m2]
                      (mkRnZ (matAdd a c) (outerProd a c) [m1,m2])