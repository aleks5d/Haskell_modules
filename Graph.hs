module Graph where

import IntMap
import Data.Maybe

data Graph = Graph {edges :: [(Int, Int, Int)], gr :: IntMap.IntMap [(Int, Int)], sz :: Int}

instance Show Graph where
    show (Graph _ gr sz) = "size: " ++ show sz ++ "\n" ++ help gr 1 sz 
        where 
        help gr ind sz = if ind > sz then ""
                         else show ind ++ ": " ++ show (IntMap.maybeGet gr ind) ++ "\n" ++ 
                                                                help gr (ind + 1) sz

empty :: Graph
empty = Graph [] IntMap.empty 0

getEdgesFromW :: Graph -> Int -> [(Int, Int)]
getEdgesFromW (Graph _ gr _) ind = if IntMap.maybeGet gr ind == Nothing 
                                   then []
                                   else IntMap.get gr ind

getEdgesFrom :: Graph -> Int -> [Int]
getEdgesFrom gr ind = fst $ unzip $ getEdgesFromW gr ind

addEdgeW :: Graph -> (Int, Int, Int) -> Graph
addEdgeW me@(Graph edges gr sz) e@(a, b, w) = Graph (e:edges) newMap $ max a $ max b sz
        where
            fromA = (b, w) : getEdgesFromW me a
            newMap  = IntMap.put gr a fromA

addEdge :: Graph -> (Int, Int) -> Graph
addEdge gr (a, b) = addEdgeW gr (a, b, 1)

addEdgeBothW :: Graph -> (Int, Int, Int) -> Graph
addEdgeBothW gr (a, b, c) = addEdgeW (addEdgeW gr (a, b, c)) (b, a, c)

addEdgeBoth :: Graph -> (Int, Int) -> Graph
addEdgeBoth gr (a, b) = addEdgeBothW gr (a, b, 1)

size :: Graph -> Int
size = sz

{-
fW for weighted function
f  for unweighted function (weight = 1)

in O(log(n)) real time
-}