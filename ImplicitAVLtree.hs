module ImplicitAVLtree where

import Data.Maybe

data Node a = Node{l :: Node a, r :: Node a, val :: Maybe a, h :: Int, sz :: Int}

instance Show(a) => Show (Node a) where
    show (Node _ _ Nothing _ _) = ""
    show (Node ltree rtree (Just me) he sz) = "[" ++ show ltree ++ 
                                            "](" ++ show me ++
                                            "," ++ show he ++
                                            "," ++ show sz ++
                                            ")[" ++ show rtree ++ "]"

empty :: Node a
empty = Node empty empty Nothing 0 0 

size :: Node a -> Int
size t = sz t

updNode :: Node a -> Node a
updNode tree@(Node _ _ Nothing _ _) = empty
updNode tree@(Node ltree rtree _ _ _) = tree{h = 1 + h ltree `max` h rtree, 
                                             sz = 1 + sz ltree + sz rtree}

smallLeft :: Node a -> Node a
smallLeft tree@(Node ltree rtree _ _ _) = rtree{l = tree'}
    where
        tree' = updNode $ tree{r = l rtree}

smallRight :: Node a -> Node a
smallRight tree@(Node ltree rtree _ _ _) = ltree{r = tree'}
    where
        tree' = updNode $ tree{l = r ltree}

bigLeft :: Node a -> Node a
bigLeft tree@(Node ltree rtree _ _ _) = smallLeft tree'
    where
        tree' = updNode $ tree{r = smallRight rtree}

bigRight :: Node a -> Node a
bigRight tree@(Node ltree rtree _ _ _) = smallRight tree'
    where
        tree' = updNode $ tree{l = smallLeft ltree}

lBalance :: Node a -> Node a
lBalance tree@(Node ltree rtree _ _ _) | h (l rtree) - h (r rtree) <= 0 = smallLeft tree
                                       | otherwise = bigLeft tree

rBalance :: Node a -> Node a
rBalance tree@(Node ltree rtree _ _ _) | h (r ltree) - h (l ltree) <= 0 = smallRight tree
                                       | otherwise = bigRight tree

balance :: Node a -> Node a
balance tree@(Node _ _ Nothing _ _) = empty
balance tree@(Node ltree rtree me _ _) | h ltree - h rtree == 2 = updNode $ rBalance tree
                                       | h rtree - h ltree == 2 = updNode $ lBalance tree
                                       | otherwise = tree

find :: Node a -> Int -> Maybe a
find (Node ltree rtree me h _) x | h == 0 = Nothing
                                 | y == x = me
                                 | y < x = find rtree (x - y - 1)
                                 | y > x = find ltree x
                                 where 
                                     y = size ltree

insert :: Node a -> Int -> a -> Node a
insert (Node _ _ Nothing _ _) ind x = Node empty empty (Just x) 1 1
insert tree@(Node ltree rtree me _ _) ind x = updNode $ tree'
    where
        y = size ltree
        tree'' = if y < ind then tree{r = insert rtree (ind - y - 1) x}
                            else tree{l = insert ltree ind x}
        tree' = balance tree''

erase :: Node a -> Int -> Node a
erase t@(Node ltree rtree me _ _) x | h t == 0 = empty
                                    | y < x  = updNode $ balance
                                              t{r = erase rtree (x - y - 1)}
                                    | y > x  = updNode $ balance
                                              t{l = erase ltree x}
                                    | h ltree + h rtree == 0 = empty 
                                    | h ltree >= h rtree = updNode $ balance
                                              treeL{val = prv}
                                    | h ltree <  h rtree = updNode $ balance 
                                              treeR{val = nxt}
                                    where
                                        y = size ltree
                                        nxt = find t (x + 1)
                                        prv = find t (x - 1)
                                        treeL = erase t $ x - 1
                                        treeR = erase t $ x + 1

pushBack :: Node a -> a -> Node a
pushBack t x = insert t (size t) x

popBack :: Node a -> Node a
popBack t | h t == 0 = empty
          | otherwise = erase t (size t - 1)

toList :: Node a -> [a]
toList (Node lt rt me _ _) = case me of
                             Nothing -> []
                             Just x -> toList lt ++ [x] ++ toList rt

longMerge :: Node a -> Node a -> Node a
longMerge lt rt = foldl pushBack rt $ toList lt

rMergeVal :: Node a -> Node a -> Maybe a -> Node a
rMergeVal lt rt x | h lt >= h rt = updNode $ Node lt rt x 0 0
             | otherwise = updNode . balance $ rt{l = rMergeVal lt (l rt) x} 

rMerge :: Node a -> Node a -> Node a
rMerge lt rt = rMergeVal lt' rt el
    where
        el  = find lt (size lt - 1)
        lt' = erase lt (size lt - 1)

lMergeVal :: Node a -> Node a -> Maybe a -> Node a
lMergeVal lt rt x | h lt <= h rt = updNode $ Node lt rt x 0 0
                  | otherwise = updNode . balance $ lt{r = lMergeVal (r lt) rt x}

lMerge :: Node a -> Node a -> Node a
lMerge lt rt = lMergeVal lt rt' el
    where
        el = find rt 0
        rt' = erase rt 0

fastMerge :: Node a -> Node a -> Node a
fastMerge lt rt | h lt <= h rt = rMerge lt rt
                | otherwise    = lMerge lt rt

merge :: Node a -> Node a -> Node a
merge ltree rtree | (h rtree) == 0 = ltree
                  | (h ltree) == 0 = rtree
                  | otherwise = fastMerge ltree rtree

split :: Node a -> Int -> (Node a, Node a)
split t x | h t == 0 = (empty, empty)
          | y > x = (ll, rMergeVal lr (r t) (val t))
          | otherwise = (lMergeVal (l t) rl (val t), rr)
          where
            (ll, lr) = split (l t) x
            (rl, rr) = split (r t) (x - y - 1)
            y = size . l $ t

{-
implicit AVL tree
you can insert, erase, find, split by index
merge = concatenation
empty    :: new implicit AVL tree,                                    O(1) 
isnert   :: curr tree -> index -> value -> new tree,                  O(log(n))
erase    :: curr tree -> index -> new tree,                           O(log(n))
find     :: curr tree -> index -> value -> new tree,                  O(log(n))
pushBack :: curr tree -> value -> new tree, append at tre end,        O(log(n))
popBack  :: curr tree -> new tree, rempve last value,                 O(log(n))
merge    :: frst tree -> scnd tree -> frst tree ++ scnd tree,         O(log(n))
split    :: curr tree -> index -> (ltree,tree),                       O(log(n))
ltree = values with indexes <= index
rtree = other values
toList   :: curr tree -> [a], list of values                          O(n)

-}