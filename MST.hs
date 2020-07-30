module MST where

import DSU
import Graph
import MergeSort

edgeCmp :: Ord(a) => (Int, Int, a) -> (Int, Int, a) -> Bool
edgeCmp (_, _, x) (_, _, y) = x < y

edgeSort :: Ord(a) => [(Int, Int, a)] -> [(Int, Int, a)]
edgeSort xs = mergeSortCmp edgeCmp xs

buildMST :: Integral a => Graph a -> [(Int, Int, a)]
buildMST gr = help DSU.empty $ edgeSort $ Graph.edges gr
    where 
        help :: Ord a => DSU Int -> [(Int, Int, a)] -> [(Int, Int, a)]
        help _ [] = []
        help dsu (x@(l, r, w):xs) = if need then x : help dsu' xs
                                  else help dsu' xs
            where
                (dsu', need) = DSU.merge dsu l r

{-
buildMST :: graph -> list of edges in minimum spanning tree, O(n log(n) alpha(n))
-}