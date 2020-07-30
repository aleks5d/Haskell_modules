module FastPower where

fastPower :: a -> Int -> (a -> a -> a) -> a
fastPower x n f | n <= 0 =  error "bad power"
                | n == 1 = x
                | n `mod` 2 == 0 = f hlf hlf
                | otherwise = f x $ f hlf hlf
                where
                    hlf = fastPower x (n `div` 2) f

{-
fastPower x n f = x `f` x `f` x ... `f` x. n times
-}