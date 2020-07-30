module Dijkstra where

import Graph
import BinomialHeap as BH
import IntMap

update :: Integral a => (Int, a) -> [(Int, a)] -> IntMap a -> BinomialHeap (a, Int) 
                        -> (BinomialHeap (a, Int), IntMap a)
update _ [] x y = (y, x)
update (v, w) ((to, toW) : xs) dst hp | IntMap.maybeGet dst to == Nothing = f'
                                      | IntMap.get dst to > w + toW = f'
                                      | otherwise = update (v, w) xs dst hp
                                      where
                                        nW = toW + w
                                        f' = update (v, w) xs (IntMap.put dst to nW) 
                                                (BH.insert hp (nW, to))

temp :: Integral a => Graph a -> BH.BinomialHeap (a, Int) -> IntMap Int -> IntMap a -> IntMap a
temp gr hp was dst = if BH.isEmpty hp 
                     then dst else
                     if IntMap.maybeGet was v /= Nothing 
                     then temp gr (BH.deleteMin hp) was dst
                     else temp gr newHeap newWas newDst
                     where 
                        (w, v) = BH.getMin hp
                        edg = Graph.getEdgesFromW gr v
                        (newHeap, newDst) = update (v, w) edg dst $ BH.deleteMin hp
                        newWas = IntMap.put was v 1

calcDists :: Integral a => Graph a -> Int -> IntMap a
calcDists gr start = temp gr (BH.insert BH.empty (0, start)) 
                            IntMap.empty (IntMap.put IntMap.empty start 0)


dijkstra :: Integral a => Graph a -> Int -> [a]
dijkstra gr start = IntMap.toList 1 (Graph.size gr) (-1) $ calcDists gr start

{-
dijkstra get Graph, start vertex and return list of dists, where -1 if you cant reach it
dijkstra :: Graph -> start vertex -> list of dists, O((n + m) log (n + m))
-}