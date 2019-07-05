module Test.ContractionTest where

import Generate.Contraction
import Data.List (permutations)

-- Baseline:
t01 = calcDims $ Sum 0 [Tensor 0 [0]] -- a = sum_i A_i
-- B_i = A_i + 1
t03 = calcDims $ Sum 1 [Tensor 0 [0]] -- B_i = c * A_i

-- 1D:
t11 = calcDims $ Sum 0 [Tensor 0 [0], Tensor 1 [0]] -- sum_i A_i * B_i					(dot)
t12 = calcDims $ Sum 0 [Tensor 0 [0], Tensor 1 [0], Tensor 2 [0]] -- sum_i A_i * B_i * C_i					(expressivity check)
-- sum_i ( A_i + B_i ) * C_i				(fold-zip fusion check)
-- sum_i ( A_i + B_i ) * (A_i  - C_i)
-- sum_i ( A_i + B_i ) * (C_i  - D_i)			(2way fold zip fusion)
-- sum_i ( a * A_i + b * B_i ) * (c * C_i + d * D_i)	(2way map-zip & fold-zip fusion)
-- sum_i (( A_i + B_i )) * ((C_i  - D_i))			(force temporary)

-- (sum_i A_i * B_i, sum_i C_i * D_i)			(horizontal fusion)

-- 2D:
t21 = calcDims $ Sum 1 [Tensor 0 [0,1], Tensor 1 [1]] -- t1_i = sum_j A_ij * B_j					(mat-vct mul)
t22 = calcDims $ Sum 1 [Tensor 0 [0,1], Tensor 1 [1], Tensor 2 [0]] -- t2_i = sum_j A_ij * B_j * C_i				(expressivity check)
-- t3_i = sum_j (A_ij + B_ij) * C_j			(fold-zip fusion)
-- t4_i = sum_j (A_ij + B_ij) * (C_j + D_j)		(2-way fold-zip fusion)
-- t5_i = sum_j (a*A_ij + b*B_ij) * (c*C_j + d*D_j)	(2-way map-fold-zip fusion)
t26 = calcDims $ Sum 1 [Tensor 0 [0], Tensor 1 [1], Tensor 2 [0], Tensor 3 [1]] -- t6_i = sum_j A_i * B_j * C_i * D_j			(outer product fusion / expressivity)
t27 = calcDims $ Sum 1 [Tensor 0 [0,1], Sum 2 [Tensor 1 [1,2], Tensor 2 [2]]] -- t7_i = sum_j A_ij * (sum_k B_jk * C_k)		(double matrix - vector mul)
-- t8_i = sum_j (A_ij + B_ij) (sum_k C_jk * D_k)	(double matrix - vector mul with zip fusion)
-- t9_i = sum_j ( sum_k A_ik * B_kj) (C_j + D_j)	(mat - mat product with vector product fusion)
t210 = calcDims $ Sum 1 [Sum 2 [Tensor 0 [0,2], Tensor 1 [2,1]], Sum 3 [Tensor 2 [1,3], Tensor 3 [3]]] -- t10_i = sum_j ( sum_k A_ik * B_kj) (sum_l C_jl * D_l) (all above)
t211 = calcDims $ Sum 2 [Tensor 0 [0,2], Tensor 1 [2,1]] -- (mat - mat mul)
t211a = calcDims $ Sum 2 [Tensor 0 [0,2], Tensor 1 [1,2]] -- (mat - mat mul with transposed B)
t211b = calcDims $ Sum 2 [Tensor 0 [2,0], Tensor 1 [2,1]]
t211c = calcDims $ Sum 2 [Tensor 0 [2,0], Tensor 1 [1,2]]
-- 3D:

t31 = calcDims $ Sum 3 [Tensor 0 [0,1,3], Tensor 1 [3,2]] -- t1_ijk = sum_l A_ijl * B_lk			(tensor- matrix mul)
t31a = calcDims $ Sum 3 [Tensor 0 [0,1,3], Tensor 1 [2,3]]
t31b = calcDims $ Sum 3 [Tensor 0 [0,3,1], Tensor 1 [3,2]]
t31c = calcDims $ Sum 3 [Tensor 0 [0,3,1], Tensor 1 [2,3]]
t31d = calcDims $ Sum 3 [Tensor 0 [3,0,1], Tensor 1 [3,2]]
t31e = calcDims $ Sum 3 [Tensor 0 [3,0,1], Tensor 1 [2,3]]
t31f = calcDims $ Sum 3 [Tensor 0 [3,1,0], Tensor 1 [2,3]]
t32 = calcDims $ Sum 3 [Tensor 0 [0,1,3], Tensor 1 [3], Tensor 2 [2]] -- t2_ijk = sum_l (A_ijl) * (B_l * C_k)   		(outer product fusion check)
t33 = calcDims $ Sum 3 [Tensor 0 [0,1,3], Tensor 1 [3,2], Tensor 2 [3], Tensor 3 [1]] -- t3_ijk = sum_l A_ijl * B_lk * C_l * D_j	(expressivity check)
-- t4_ijk = sum_l (A_ijl + B_ijl) * C_lk 		(zip fusion check)
-- t5_ijk = sum_l (A_ijl + B_ijl) * (C_lk + D_lk)		(zip fusion check)
-- t6_ijk = sum_l (a*A_ijl + b*B_ijl) * (c*C_lk + d*D_lk)	(2 way map-zip fusion check)
-- t7_ijk = sum_l (a*A_ijl + b*B_ijl) * (c*C_lk + d*D_lk) * (e*E_l + f*F_l) * (g * G_j + h * H_j) (fusion + expressivity check)
t38 = calcDims $ Sum 3 [Sum 4 [Tensor 0 [0,3,4], Tensor 1 [4,1], Tensor 2 [3,2]]] -- t8_ijk = sum_lm A_ilm * B_mj * C_lk    (tensor- matrix double mul, temporary should be better!)
t39 = calcDims $ Sum 2 [Tensor 0 [0,1,2], Sum 3 [Tensor 1 [2,3], Tensor 2 [3]]] -- t9_ij = sum_k A_ijk * (sum_l B_kl * C_l)    (double product fusion check, temporary should be better)
t310 = calcDims $ Sum 2 [Tensor 0 [0,1,2], Sum 3 [Tensor 1 [0,2,3], Tensor 2 [3]]] -- t10_ij = sum_k A_ijk * (sum_l B_ikl * C_l)    (double product fusion check, temporary should be better)
t311 = calcDims $ Sum 2 [Tensor 0 [0], Tensor 1 [1], Tensor 2 [2], Sum 3 [Tensor 3 [0,2,3], Tensor 4 [3]]] -- t11_ij = sum_k (A_i * B_j * C_k) * (sum_l D_ikl * E_l)    (double product fusion check with outer product, temporary should be better)

-- Generalized matrix multiplications
genMatMul = map (calcDims . \(ind0,ind1) -> Sum 3 [Tensor 0 ind0, Tensor 1 ind1]) [(ind0,ind1)|ind0 <- permutations [0,1,3], ind1 <- permutations [2,3]]
contTests = [t01, t03, t11, t12, t21, t22, t26, t27, t210, t211, t211a, t211b, t211c, t31, t31a, t31b, t31c, t31d, t31e, t31f, t32, t33, t38, t39, t310, t311]