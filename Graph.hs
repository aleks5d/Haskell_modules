module Graph where

import IntMap
import Data.Maybe

data Graph a = Graph {edges :: [(Int, Int, a)], gr :: IntMap.IntMap [(Int, a)], sz :: Int}

instance Show a => Show (Graph a) where
    show (Graph _ gr sz) = "size: " ++ show sz ++ "\n" ++ help gr 1 sz 
        where 
        help gr ind sz = if ind > sz then ""
                         else show ind ++ ": " ++ show (IntMap.maybeGet gr ind) ++ "\n" ++ 
                                                                help gr (ind + 1) sz

empty :: Graph a
empty = Graph [] IntMap.empty 0

getEdgesFromW :: Integral a => Graph a -> Int -> [(Int, a)]
getEdgesFromW (Graph _ gr _) ind = if IntMap.maybeGet gr ind == Nothing 
                                   then []
                                   else IntMap.get gr ind

getEdgesFrom :: Integral a => Graph a -> Int -> [Int]
getEdgesFrom gr ind = fst $ unzip $ getEdgesFromW gr ind

addEdgeW :: Integral a => Graph a -> (Int, Int, a) -> Graph a
addEdgeW me@(Graph edges gr sz) e@(a, b, w) = Graph (e:edges) newMap $ max a $ max b sz
        where
            fromA = (b, w) : getEdgesFromW me a
            newMap  = IntMap.put gr a fromA

addEdge :: Integral a => Graph a -> (Int, Int) -> Graph a
addEdge gr (a, b) = addEdgeW gr (a, b, 1)

addEdgeBothW :: Integral a => Graph a -> (Int, Int, a) -> Graph a
addEdgeBothW gr (a, b, c) = addEdgeW (addEdgeW gr (a, b, c)) (b, a, c)

addEdgeBoth :: Integral a => Graph a -> (Int, Int) -> Graph a
addEdgeBoth gr (a, b) = addEdgeBothW gr (a, b, 1)

size :: Graph a -> Int
size = sz

{-
data structure for weighted graphs
weights can be Int or Integer
empty         :: create empty graph
getEdgesFromW :: curr graph -> vertex -> list of weighted edges from vertex 
getEdgesFrom  :: curr graph -> vertex -> list of edges from vertex 
addEdgeW      :: curr graph -> (from, to, w) -> new graph, append edge from -> to (w)
addEdge       :: curr graph -> (from, to)    -> new graph, append edge from -> to (1)
addEdgeBothW  :: curr graph -> (v1, v2, w)   -> new graph, append edge v2 -> v1 (w)
                                                           append edge v1 -> v2 (w)
addEdgeBoth   :: curr graph -> (v1, v2)      -> new graph, append edge v2 -> v1 (1)
                                                           append edge v1 -> v2 (1)
size          :: curr graph -> max nubmer of vertex
-}