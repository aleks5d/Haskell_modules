module Dijkstra where

import Graph
import BinomialHeap as BH
import IntMap

update :: (Int, Int) -> [(Int, Int)] -> IntMap Int -> BinomialHeap (Int, Int) 
                        -> (BinomialHeap (Int, Int), IntMap Int)
update _ [] a b = (b, a)
update (v, w) ((to, toW) : xs) dst hp | IntMap.maybeGet dst to == Nothing = f'
                                      | IntMap.get dst to > w + toW = f'
                                      | otherwise = update (v, w) xs dst hp
                                      where
                                        nW = toW + w
                                        f' = update (v, w) xs (IntMap.put dst to nW) 
                                                (BH.insert hp (to, nW))

temp :: Graph -> BH.BinomialHeap (Int, Int) -> IntMap Int -> IntMap Int -> IntMap Int
temp gr hp was dst = if BH.isEmpty hp 
                     then dst else
                     if IntMap.maybeGet was v /= Nothing 
                     then temp gr (BH.deleteMin hp) was dst
                     else temp gr newHeap newWas newDst
                     where 
                        (v, w) = BH.getMin hp
                        edg = Graph.getEdgesFromW gr v
                        (newHeap, newDst) = update (v, w) edg dst $ BH.deleteMin hp
                        newWas = IntMap.put was v 1

calcDists :: Graph -> Int -> IntMap Int
calcDists gr start = temp gr (BH.insert BH.empty (start, 0)) 
                            IntMap.empty (IntMap.put IntMap.empty start 0)


dijkstra :: Graph -> Int -> [Int]
dijkstra gr start = IntMap.toList 1 (Graph.size gr) (-1) $ calcDists gr start

{-
dijkstra get Graph, start vertex and return list of dists, where -1 if you cant reach it
O((n + m) log (n + m))
-}