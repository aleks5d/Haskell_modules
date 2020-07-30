module MergeSort where

merge :: Ord(a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge xs1@(x1:xs1') xs2@(x2:xs2') = if x1 < x2 then x1 : merge xs1' xs2
                                               else x2 : merge xs1 xs2'

mergeCmp :: [a] -> [a] -> (a -> a -> Bool) -> [a]
mergeCmp [] xs _ = xs
mergeCmp xs [] _ = xs
mergeCmp xs1@(x1:xs1') xs2@(x2:xs2') f = if f x1 x2 then x1 : mergeCmp xs1' xs2 f
                                                    else x2 : mergeCmp xs2' xs1 f

split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([], [x])
split (x : xs) = (xs2, x : xs1) where (xs1, xs2) = split xs

mergeSort :: Ord(a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort lpart) (mergeSort rpart)
         where (lpart, rpart) = split xs

mergeSortCmp :: (a -> a -> Bool) -> [a] -> [a]
mergeSortCmp _ [] = []
mergeSortCmp _ [x] = [x]
mergeSortCmp f xs = mergeCmp (mergeSortCmp f lpart) (mergeSortCmp f rpart) f
        where (lpart, rpart) = split xs

{-
Merge sorting algorithm for any Ordered type
mergeSort    :: list -> sorted list by (<) operator,             O(n log(n)) time
mergeSortCmp :: comparator -> list -> sorted list by comporator, O(n log(n)) time
comporator must be (val1 -> val2 -> Bool). val1 < val2 -> True
                                           otherwise   -> False
-}