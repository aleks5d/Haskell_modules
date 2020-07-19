module BinomialHeap where

data Tree a = Tree {rk :: Int, val :: a, to :: [Tree a]}

data BinomialHeap a = BinomialHeap {arr :: [Tree a]}

instance Show a => Show (Tree a) where
    show (Tree _ x []) = show x
    show (Tree _ x xs) = show x ++ "->" ++ show xs

instance Show a => Show (BinomialHeap a) where
    show x = show $ arr x 

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
empty = BinomialHeap []

isEmpty :: BinomialHeap a -> Bool
isEmpty (BinomialHeap []) = True
isEmpty _ = False

insert :: (Ord a) => BinomialHeap a -> a -> BinomialHeap a
insert (BinomialHeap ts) x = BinomialHeap $ insertTree (Tree 0 x []) ts

getMin :: (Ord a) => BinomialHeap a -> a
getMin (BinomialHeap []) = error "empty heap"
getMin (BinomialHeap ts) = val $ fst $ removeMinTree ts

deleteMin :: (Ord a) => BinomialHeap a -> BinomialHeap a
deleteMin (BinomialHeap []) = BinomialHeap []
deleteMin (BinomialHeap ts) = BinomialHeap $ mergeTree (reverse ts1) ts2
                where (Tree _ x ts1, ts2) = removeMinTree ts