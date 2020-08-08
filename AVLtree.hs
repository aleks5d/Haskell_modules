module AVLtree where

import Data.Maybe

data Node a = Node{l :: Node a, r :: Node a, val :: Maybe a, h :: Int, sz :: Int}

instance Show(a) => Show (Node a) where
    show (Node _ _ Nothing _ _) = ""
    show (Node ltree rtree (Just me) he sz) = "[" ++ show ltree ++ 
                                            "](" ++ show me ++
                                            "," ++ show he ++
                                            "," ++ show sz ++
                                            ")[" ++ show rtree ++ "]"

empty :: Ord(a) => Node a
empty = Node empty empty Nothing 0 0 

size :: Ord(a) => Node a -> Int
size t = sz t

updNode :: Ord(a) => Node a -> Node a
updNode tree@(Node _ _ Nothing _ _) = empty
updNode tree@(Node ltree rtree _ _ _) = tree{h = 1 + h ltree `max` h rtree, 
                                             sz = 1 + sz ltree + sz rtree}

smallLeft :: Ord(a) => Node a -> Node a
smallLeft tree@(Node ltree rtree _ _ _) = rtree{l = tree'}
    where
        tree' = updNode $ tree{r = l rtree}

smallRight :: Ord(a) => Node a -> Node a
smallRight tree@(Node ltree rtree _ _ _) = ltree{r = tree'}
    where
        tree' = updNode $ tree{l = r ltree}

bigLeft :: Ord(a) => Node a -> Node a
bigLeft tree@(Node ltree rtree _ _ _) = smallLeft tree'
    where
        tree' = updNode $ tree{r = smallRight rtree}

bigRight :: Ord(a) => Node a -> Node a
bigRight tree@(Node ltree rtree _ _ _) = smallRight tree'
    where
        tree' = updNode $ tree{l = smallLeft ltree}

lBalance :: Ord(a) => Node a -> Node a
lBalance tree@(Node ltree rtree _ _ _) | h (l rtree) - h (r rtree) <= 0 = smallLeft tree
                                       | otherwise = bigLeft tree

rBalance :: Ord(a) => Node a -> Node a
rBalance tree@(Node ltree rtree _ _ _) | h (r ltree) - h (l ltree) <= 0 = smallRight tree
                                       | otherwise = bigRight tree

balance :: Ord(a) => Node a -> Node a
balance tree@(Node _ _ Nothing _ _) = empty
balance tree@(Node ltree rtree me _ _) | h ltree - h rtree == 2 = updNode $ rBalance tree
                                       | h rtree - h ltree == 2 = updNode $ lBalance tree
                                       | otherwise = tree

find :: Ord(a) => Node a -> a -> Bool
find (Node ltree rtree me h _) x | h == 0 = False
                                 | y == x = True
                                 | y < x = find rtree x
                                 | y > x = find ltree x
                                 where 
                                     y = fromJust me

next :: Ord(a) => Node a -> a -> Maybe a
next (Node ltree rtree me h _) x | h == 0 = Nothing
                                 | y <= x = next rtree x
                                 | z == Nothing = Just y
                                 | otherwise = z
                                 where
                                   y = fromJust me
                                   z = next ltree x

prev :: Ord(a) => Node a -> a -> Maybe a
prev (Node ltree rtree me h _) x | h == 0 = Nothing
                                 | y >= x = prev ltree x
                                 | z == Nothing = Just y
                                 | otherwise = z
                                 where
                                    y = fromJust me
                                    z = prev rtree x

insert :: Ord(a) => Node a -> a -> Node a
insert (Node _ _ Nothing _ _) x = Node empty empty (Just x) 1 1
insert tree@(Node ltree rtree me _ _) x = updNode $ tree'
    where
        y = fromJust me
        tree'' = if y < x      then tree{r = insert rtree x}
                 else if y > x then tree{l = insert ltree x}
                      else tree
        tree' = balance tree''

erase :: Ord(a) => Node a -> a -> Node a
erase t@(Node ltree rtree me _ _) x | h t == 0 = empty
                                    | y < x  = updNode $ balance
                                              t{r = erase rtree x}
                                    | y > x  = updNode $ balance
                                              t{l = erase ltree x}
                                    | h ltree + h rtree == 0 = empty 
                                    | h ltree >= h rtree = updNode $ balance
                                              treeL{val = prv}
                                    | h ltree <  h rtree = updNode $ balance 
                                              treeR{val = nxt}
                                    where
                                        y = fromJust me
                                        nxt = next t x
                                        prv = prev t x
                                        treeL = erase t $ fromJust prv
                                        treeR = erase t $ fromJust nxt

getMax :: Ord(a) => Node a -> Maybe a
getMax (Node _ rtree me h _) | h == 0 = Nothing
                             | x == Nothing = me
                             | otherwise = x
                             where
                                 x = getMax rtree

getMin :: Ord(a) => Node a -> Maybe a
getMin (Node ltree _ me h _) | h == 0 = Nothing
                             | x == Nothing = me
                             | otherwise = x
                             where
                                 x = getMin ltree

toList :: Ord(a) => Node a -> [a]
toList (Node lt rt me _ _) = case me of
                             Nothing -> []
                             Just x -> toList lt ++ [x] ++ toList rt

longMerge :: Ord(a) => Node a -> Node a -> Node a
longMerge lt rt = foldl insert rt $ toList lt

rMergeVal :: Ord(a) => Node a -> Node a -> Maybe a -> Node a
rMergeVal lt rt x | h lt >= h rt = updNode $ Node lt rt x 0 0
             | otherwise = updNode . balance $ rt{l = rMergeVal lt (l rt) x} 

rMerge :: Ord(a) => Node a -> Node a -> Node a
rMerge lt rt = rMergeVal lt' rt el
    where
        el  = getMax lt
        lt' = erase lt $ fromJust el

lMergeVal :: Ord(a) => Node a -> Node a -> Maybe a -> Node a
lMergeVal lt rt x | h lt <= h rt = updNode $ Node lt rt x 0 0
                  | otherwise = updNode . balance $ lt{r = lMergeVal (r lt) rt x}

lMerge :: Ord(a) => Node a -> Node a -> Node a
lMerge lt rt = lMergeVal lt rt' el
    where
        el = getMin rt
        rt' = erase rt $ fromJust el

fastMerge :: Ord(a) => Node a -> Node a -> Node a
fastMerge lt rt | h lt <= h rt = rMerge lt rt
                | otherwise    = lMerge lt rt

merge :: Ord(a) => Node a -> Node a -> Node a
merge ltree rtree | (h rtree) == 0 = ltree
                  | (h ltree) == 0 = rtree
                  | getMin rtree > getMax ltree = fastMerge ltree rtree
                  | getMin ltree > getMax rtree = fastMerge rtree ltree
                  | size ltree <= size rtree     = longMerge     ltree rtree
                  | otherwise                    = longMerge     rtree ltree

split :: Ord(a) => Node a -> a -> (Node a, Node a)
split t x | h t == 0 = (empty, empty)
          | y > x = (ll, rMergeVal lr (r t) (val t))
          | otherwise = (lMergeVal (l t) rl (val t), rr)
          where
            (ll, lr) = split (l t) x
            (rl, rr) = split (r t) x
            y = fromJust . val $ t

{-
AVL tree for any ordered type
empty  :: new AVL tree,                                    O(1) 
isnert :: curr tree -> value -> new tree,                  O(log(n))
erase  :: curr tree -> value -> new tree,                  O(log(n))
find   :: curr tree -> value -> is value in tree,          O(log(n))
next   :: curr tree -> x     -> minimal value in tree > x, O(log(n))
prev   :: curr tree -> x     -> maximum value in tree < x, O(log(n))
getMin :: curr tree -> minimum value in tree,              O(log(n))
getMax :: curr tree -> maximum value in tree,              O(log(n))
merge  :: frst tree -> scnd tree -> frst tree + scnd tree, O(log(n))
work if all keys of one tree less than keys of second one
otherwise work in O((min size) * log(max size))
split  :: curr tree -> key -> (ltree,tree),                O(log(n))
ltree = all keys <= key
rtree = all keys >  key
toList :: curr tree -> [a], list of keys                   O(n)

-}