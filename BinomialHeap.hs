module BinomialHeap where

data Tree a = Tree {rk :: Int, val :: a, to :: [Tree a]}

data BinomialHeap a = BinomialHeap {minimal :: Maybe a, arr :: [Tree a]}

instance Show a => Show (Tree a) where
    show (Tree _ x []) = show x
    show (Tree _ x xs) = show x ++ "->" ++ show xs

instance Show a => Show (BinomialHeap a) where
    show x = show (minimal x) ++ "->" ++ show (arr x)

link :: Ord(a) => Tree a -> Tree a -> Tree a
link t1@(Tree r1 x1 c1) t2@(Tree r2 x2 c2) | x1 < x2 = Tree (r1 + 1) x1 (t2 : c1)
                                           | otherwise = Tree (r2 + 1) x2 (t1 : c2)

rank :: Tree a -> Int
rank (Tree x _ _) = x

insertTree :: Ord(a) => Tree a -> [Tree a] -> [Tree a]
insertTree t [] = [t]
insertTree t ts@(t':ts') | rank t < rank t' = t : ts
                         | otherwise = insertTree (link t t') ts'

mergeTree :: Ord(a) => [Tree a] -> [Tree a] -> [Tree a]
mergeTree [] x = x
mergeTree x [] = x
mergeTree ts1@(t1' : ts1') ts2@(t2' : ts2')
        | rank t1' < rank t2' = t1' : mergeTree ts1' ts2
        | rank t2' < rank t1' = t2' : mergeTree ts1 ts2'
        | otherwise = insertTree (link t1' t2') (mergeTree ts1' ts2')

removeMinTree :: Ord(a) => [Tree a] -> (Tree a, [Tree a])
removeMinTree [] = error "Empty tree list"
removeMinTree [t] = (t, [])
removeMinTree (t:ts) | val t < val t' = (t, ts)
                     | otherwise = (t', t:ts')
                     where (t', ts') = removeMinTree ts

empty :: BinomialHeap a
empty = BinomialHeap Nothing []

isEmpty :: BinomialHeap a -> Bool
isEmpty (BinomialHeap Nothing []) = True
isEmpty _ = False

insert :: (Ord a) => BinomialHeap a -> a -> BinomialHeap a
insert (BinomialHeap Nothing ts) y = BinomialHeap (Just y) ts
insert (BinomialHeap (Just x) ts) y = 
                    if x < y 
                    then BinomialHeap (Just x) $ insertTree (Tree 0 y []) ts
                    else BinomialHeap (Just y) $ insertTree (Tree 0 x []) ts

getMin :: (Ord a) => BinomialHeap a -> a
getMin (BinomialHeap Nothing _) = error "empty heap"
getMin (BinomialHeap (Just x) _) = x

deleteMin :: (Ord a) => BinomialHeap a -> BinomialHeap a
deleteMin (BinomialHeap Nothing []) = BinomialHeap Nothing []
deleteMin (BinomialHeap (Just x) []) = BinomialHeap Nothing []
deleteMin (BinomialHeap (Just x) ts) = BinomialHeap (Just y) $ mergeTree (reverse ts1) ts2
            where (Tree _ y ts1, ts2) = removeMinTree ts

{-
Binomial Heap for any ordered type
insert   : O(log(n)) real time
getMin   : O(1)      real time
deleteMin: O(log(n)) real time
-}