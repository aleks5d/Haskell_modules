module FastPower where

fastPower :: Integral b => (a -> a -> a) -> a -> b -> a
fastPower f x n | n <= 0 =  error "bad power"
                | n == 1 = x
                | n `mod` 2 == 0 = f hlf hlf
                | otherwise = f x $ f hlf hlf
                where
                    hlf = fastPower f x (n `div` 2)

{-
fastPower x n f = x `f` x `f` x ... `f` x. n times
in O(log(n)) time
`f` must be associatime 
-}