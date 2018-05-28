module Utility where

import Data.List (sort, sortBy)

mapFst :: (a -> b) -> (a,c) -> (b,c)
mapFst f (p,q) = (f p, q)

mapPair :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
mapPair f g (a,b) = (f a, g b)

partitionByIndex :: [Int] -> [a] -> ([a],[a])
partitionByIndex = helper 0 where
    helper _ [] a = ([],a)
    helper _ _ [] = ([],[])
    helper i (j:js) (a:as)
         | i == j = mapFst (a:) $ helper (i+1) js as
         | otherwise = fmap (a:) $ helper (i+1) (j:js) as

takeByIndex :: [Int] -> [a] -> [a]
takeByIndex i a = fst $ partitionByIndex i a

dropByIndex :: [Int] -> [a] -> [a]
dropByIndex i a = snd $ partitionByIndex i a

setByIndex :: [(Int,a)] -> [a] -> [a]
setByIndex vals = helper 0 (sortBy (\(a,_) (b,_) -> compare a b) vals) where
    helper _ [] a = a
    helper i x@((j,a):as) (b:bs)
        | i == j = a : helper (i+1) as bs
        | otherwise = b : helper (i+1) x bs

intersectWithIndex :: Ord a => [a] -> [a] -> [(Int,Int)]
intersectWithIndex a b = helper (sort $ zip a [0..]) (sort $ zip b [0..]) where
                            helper x@((p,i):xs) y@((q,j):ys)
                                | p == q = (i,j) : helper xs ys
                                | p < q = helper xs y
                                | p > q = helper x ys
                            helper _ _ = []