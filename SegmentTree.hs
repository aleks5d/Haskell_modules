module SegmentTree where

import Data.Maybe

data Node a = Node {val :: Maybe a, l :: Node a, r :: Node a} | Leaf {val :: Maybe a}

data SegmentTree a = SegmentTree {tree :: Node a, f :: a -> a -> a, sz :: Int}

needSize :: Int -> Int -> Int
needSize x y | x > y = x
             | otherwise = needSize (x * 2) y

createNode :: Int -> Node a
createNode 1 = Leaf Nothing
createNode n = Node{val = Nothing, l = createNode half, r = createNode half} 
                where half = div n 2

empty :: Int -> (a -> a -> a) -> SegmentTree a
empty size f = SegmentTree (createNode s) f s
    where 
        s = needSize 1 size

getVal :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
getVal f Nothing x = x
getVal f x Nothing = x
getVal f (Just x) (Just y) = Just $ f x y

updateVal :: (a -> a -> a) -> Node a -> Node a
updateVal _ xs@(Leaf _) = xs
updateVal f c@(Node vl ln rn) = c{val = getVal f (val ln) (val rn)}

updateNode :: (a -> a -> a) -> Node a -> Int -> Int -> Int -> a -> Node a
updateNode f nd li ri ind x | li == ri - 1 = Leaf $ Just x
                            | ind < m = updateVal f $ nd{l = updateNode f (l nd) li m ind x}
                            | ind >= m = updateVal f $ nd{r = updateNode f (r nd) m ri ind x}
                            where m = div (li + ri) 2

getNode :: (a -> a -> a) -> Node a -> Int -> Int -> Int -> Int -> Maybe a
getNode _ (Leaf x) _ _ _ _ = x
getNode f (Node val li ri) fl fr cl cr | fl <= cl && cr <= fr = val
                                       | fr <= m = getNode f li fl fr cl m
                                       | fl >= m = getNode f ri fl fr m cr
                                       | otherwise = getVal f (getNode f li fl m cl m) 
                                                              (getNode f ri m fr m cr)
                                       where m = div (cl + cr) 2


put :: SegmentTree a -> Int -> a -> SegmentTree a
put st ind x = st {tree = updateNode (f st) (tree st) 0 (sz st) ind x}

putP :: SegmentTree a -> (Int, a) -> SegmentTree a
putP st (ind, x) = put st ind x

get :: SegmentTree a -> Int -> Int -> Maybe a
get st li ri | ri >= li = getNode (f st) (tree st) li (ri + 1) 0 (sz st)
             | otherwise = Nothing 

getP :: SegmentTree a -> (Int, Int) -> Maybe a
getP st (li, ri) = get st li ri

fromList :: [a] -> (a -> a -> a) -> SegmentTree a
fromList xs f = foldl putP (empty (length xs) f) $ zip [1..] xs 

{-
1-numeration
operation in O(log(n)) real time
get on [l, r] = arr[l] `f` arr[l + 1] `f` ... `f` arr[r]
put - update in index
-}