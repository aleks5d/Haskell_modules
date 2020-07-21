module SuffixArray where

import MergeSort
import Data.Char

cut :: Int -> [a] -> ([a], [a])
cut 0 xs = ([], xs)
cut n [] = ([], [])
cut n (x : xs) = (x : a, b) where (a, b) = cut (n - 1) xs

shift :: Int -> [a] -> [a]
shift n xs = b ++ a where (a, b) = cut n xs

compress :: [(Int, Int)] -> [Int] 
compress xs = snd $ unzip $ mergeSort $ help 0 $ mergeSort $ zip xs [1..len]
    where 
        len = length xs
        help :: Int -> [((Int, Int), Int)] -> [(Int, Int)]
        help n [] = []
        help n [x] = [(snd x, n)]
        help n (x : (y : xs)) = if fst y == fst x then (snd x, n) : help n (y : xs) 
                                else (snd x, n) : help (n + 1) (y : xs)

buildLog'2 :: [Int] -> [Int]
buildLog'2 xs = tail $ snd $ unzip $ mergeSort $ zip (help 1 xs) [0..length xs]
    where
    help :: Int -> [Int] -> [Int]
    help n xs = if n >= length xs
                 then xs
                 else help (n * 2) $ compress $ zip xs $ shift n xs

suffixArray :: [Char] -> [Int]
suffixArray str = buildLog'2 $ map ord str ++ [0]

{-
SuffixArray for string
buildLog'2 : O(n log^2(n))
-}