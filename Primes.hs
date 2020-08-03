module Primes where

import PriorityQueue

updHeap :: Integral(a) => a -> PriorityQueue a a -> PriorityQueue a a
updHeap x heap | c == x = updHeap x $ changeMin heap (c + d) d
               | otherwise = heap
               where
                (c, d) = getMin heap

pl :: Integral(a) => a -> PriorityQueue a a -> [a]
pl n heap | isEmpty heap = (n :) $ pl (n + 1) $ insert heap (n * n) n
          | c /= n       = (n :) $ pl (n + 1) $ insert heap (n * n) n
          | otherwise    =         pl (n + 1) $ updHeap n heap
          where
            (c, _) = getMin heap

primeList :: Integral(a) => [a]
primeList = pl 2 empty

isPrime :: Integral(a) => a -> Bool
isPrime x = minDiviser x == x

allDividers :: Integral(a) => a -> [a]
allDividers n | n < 1 = error "Must be positive"
              | n == 1 = [1]
              | otherwise = help 1 n [] []
              where 
                help :: Integral(a) => a -> a -> [a] -> [a] -> [a]
                help x n l r | x * x > n = reverse l ++ r
                             | x * x == n = help (x + 1) n l (x : r)
                             | n `mod` x /= 0 = help (x + 1) n l r
                             | otherwise = help (x + 1) n (x : l) (n `div` x : r)

primeDividers :: Integral(a) => a -> [a]
primeDividers n | n < 1 = error "Must be positive"
                | n == 1 = [1]
                | otherwise = help 2 n 
                where
                    help :: Integral(a) => a -> a -> [a] 
                    help x n | x * x > n = if n == 1 then []
                                                     else [n]
                             | n `mod` x == 0 = x : help x (n `div` x)
                             | otherwise = help (x + 1) n

minDiviser :: Integral(a) => a -> a
minDiviser n | n < 1 = error "Must be positive"
             | n == 1 = 1
             | otherwise = help 2 n
             where
                help :: Integral(a) => a -> a -> a
                help x n | x * x > n = n 
                         | n `mod` x == 0 = x
                         | otherwise = help (x + 1) n     

{-
primeList     :: lazy sieve in O(n log(n) loglog(n))
                    (https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf)
isPrime       :: return is n is prime, O(sqrt(n))
minDiviser    :: return minimal diviser of n
allDividers   :: return all dividers of n, O(sqrt(n))
primeDividers :: return list of primes [p1, p2 ... pk], there p1 * ... pk = n
-}