module MergeSort where

merge :: Ord(a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge xs1@(x1:xs1') xs2@(x2:xs2') = if x1 < x2 then x1 : merge xs1' xs2
                                               else x2 : merge xs1 xs2'

split :: Ord(a) => [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([], [x])
split (x : xs) = (xs2, x : xs1) where (xs1, xs2) = split xs

mergeSort :: Ord(a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort lpart) (mergeSort rpart)
         where (lpart, rpart) = split xs

{-
Merge sorting algorithm for any Ordered type
mergeSort: O(n log(n)) real time
-}