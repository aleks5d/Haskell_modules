module AVLtree where

import Data.Maybe

data Node a = Node{l :: Node a, r :: Node a, val :: Maybe a, h :: Int}
data AVLtree a = AVLtree{tree :: Node a, sz :: Int}

instance Show(a) => Show (Node a) where
    show (Node _ _ Nothing _) = ""
    show (Node ltree rtree (Just me) he) = "[" ++ show ltree ++ 
                                            "](" ++ show me ++
                                            "," ++ show he ++
                                            ")[" ++ show rtree ++ "]"

instance Show(a) => Show (AVLtree a) where
    show (AVLtree tree _) = "AVLTREE: " ++ show tree 

emptyNode :: Node a
emptyNode = Node emptyNode emptyNode Nothing 0

empty :: Ord(a) => AVLtree a
empty = AVLtree emptyNode 0

size :: AVLtree a -> Int
size = sz

updHeight :: Node a -> Node a
updHeight tree@(Node _ _ Nothing _) = emptyNode
updHeight tree@(Node ltree rtree _ _) = tree{h = 1 + h ltree `max` h rtree}

smallLeft :: Node a -> Node a
smallLeft tree@(Node ltree rtree _ _) = rtree{l = tree'}
    where
        tree' = updHeight $ tree{r = l rtree}

smallRight :: Node a -> Node a
smallRight tree@(Node ltree rtree _ _) = ltree{r = tree'}
    where
        tree' = updHeight $ tree{l = r ltree}

bigLeft :: Node a -> Node a
bigLeft tree@(Node ltree rtree _ _) = smallLeft tree'
    where
        tree' = updHeight $ tree{r = smallRight rtree}

bigRight :: Node a -> Node a
bigRight tree@(Node ltree rtree _ _) = smallRight tree'
    where
        tree' = updHeight $ tree{l = smallLeft ltree}

lBalance :: Node a -> Node a
lBalance tree@(Node ltree rtree _ _) | h (l rtree) - h (r rtree) <= 0 = smallLeft tree
                                        | otherwise = bigLeft tree

rBalance :: Node a -> Node a
rBalance tree@(Node ltree rtree _ _) | h (r ltree) - h (l ltree) <= 0 = smallRight tree
                                        | otherwise = bigRight tree

balance :: Node a -> Node a
balance tree@(Node _ _ Nothing _) = emptyNode
balance tree@(Node ltree rtree me _) | h ltree - h rtree == 2 = updHeight $ rBalance tree
                                     | h rtree - h ltree == 2 = updHeight $ lBalance tree
                                     | otherwise = tree

findNode :: Ord(a) => Node a -> a -> Bool
findNode (Node ltree rtree me h) x | h == 0 = False
                                   | y == x = True
                                   | y < x = findNode rtree x
                                   | y > x = findNode ltree x
                                   where 
                                       y = fromJust me

find :: Ord(a) => AVLtree a -> a -> Bool
find (AVLtree _ 0) _ = False
find (AVLtree tree _) x = findNode tree x

nextNode :: Ord(a) => Node a -> a -> Maybe a
nextNode (Node ltree rtree me h) x | h == 0 = Nothing
                                   | y <= x = nextNode rtree x
                                   | z == Nothing = Just y
                                   | otherwise = z
                                   where
                                      y = fromJust me
                                      z = nextNode ltree x

prevNode :: Ord(a) => Node a -> a -> Maybe a
prevNode (Node ltree rtree me h) x | h == 0 = Nothing
                                   | y >= x = prevNode ltree x
                                   | z == Nothing = Just y
                                   | otherwise = z
                                   where
                                      y = fromJust me
                                      z = prevNode rtree x

next :: Ord(a) => AVLtree a -> a -> Maybe a
next (AVLtree tree _) x = nextNode tree x

prev :: Ord(a) => AVLtree a -> a -> Maybe a
prev (AVLtree tree _) x = prevNode tree x

insertNode :: Ord(a) => Node a -> a -> Node a
insertNode (Node _ _ Nothing _) x = Node emptyNode emptyNode (Just x) 1
insertNode tree@(Node ltree rtree me _) x = updHeight $ tree'
    where
        y = fromJust me
        tree'' = if y < x then tree{r = insertNode rtree x}
                          else tree{l = insertNode ltree x}
        tree' = balance tree''

eraseNode :: Ord(a) => Node a -> a -> Node a
eraseNode t@(Node ltree rtree me _) x | y < x  = updHeight $ balance
                                                t{r = eraseNode rtree x}
                                      | y > x  = updHeight $ balance
                                                t{l = eraseNode ltree x}
                                      | h ltree + h rtree == 0 = emptyNode 
                                      | h ltree > h rtree = treeL{val = prv}
                                      | h ltree < h rtree = treeR{val = nxt}
                                      where
                                          y = fromJust me
                                          nxt = nextNode t x
                                          prv = prevNode t x
                                          treeL = eraseNode t $ fromJust prv
                                          treeR = eraseNode t $ fromJust nxt

insert :: Ord(a) => AVLtree a -> a -> AVLtree a
insert t@(AVLtree tree sz) x | find t x = t
                             | otherwise = AVLtree (insertNode tree x) $ sz + 1

erase :: Ord(a) => AVLtree a -> a -> AVLtree a
erase t@(AVLtree tree sz) x | find t x = AVLtree (eraseNode tree x) $ sz - 1
                            | otherwise = t


{-
AVL tree for any ordered type
empty  :: new AVL tree,                                    O(1) 
isnert :: curr tree -> value -> new tree,                  O(log(n))
erase  :: curr tree -> value -> new tree,                  O(log(n))
find   :: curr tree -> value -> is value in tree,          O(log(n))
next   :: curr tree -> x     -> minimal value in tree > x, O(log(n))
prev   :: curr tree -> x     -> maximum value in tree < x, O(log(n))
-}