module Deque where

import Stack

data Deque a = Deque {l :: Stack a, r :: Stack a, 
                     lf :: Maybe a, rf :: Maybe a}
                
instance Show a => Show (Deque a) where
  show d = "[[" ++ (show $ l d) ++ "],[" ++ (show $ r d) ++ "]]"

empty :: Deque a
empty = Deque Stack.empty Stack.empty Nothing Nothing

isEmpty :: Deque a -> Bool
isEmpty (Deque l r lf rf) = Stack.isEmpty l && Stack.isEmpty r

size :: Deque a -> Int
size (Deque l r lf rf) = Stack.size l + Stack.size r

pushBack :: Deque a -> a -> Deque a
pushBack (Deque l r Nothing rf) x = Deque l (push r x) (Just x) (Just x)
pushBack (Deque l r lf rf) x = Deque l (push r x) lf (Just x)

pushFront :: Deque a -> a -> Deque a
pushFront (Deque l r lf Nothing) x = Deque (push l x) r (Just x) (Just x)
pushFront (Deque l r lf rf) x = Deque (push l x) r (Just x) rf

front :: Deque a -> a
front (Deque l r Nothing rf) = error "No front element"
front (Deque l r (Just x) rf) = x

back :: Deque a -> a
back (Deque l r lf Nothing) = error "No back element"
back (Deque l r lf (Just x)) = x

redistributeHalf :: (Stack a, Stack a) -> (Stack a, Stack a)
redistributeHalf (x, y) | Stack.size x <= Stack.size y = (x, y) 
                    | otherwise = redistributeHalf
                        (pop x, push y $ top x)

redistributeAll :: (Stack a, Stack a) -> (Stack a, Stack a) 
redistributeAll (x, y) | Stack.isEmpty x = (x, y)
                       | otherwise = redistributeAll 
                         (pop x, push y $ top x)

redistribute :: (Stack a, Stack a) -> (Stack a, Stack a)
redistribute (x, y) = (ansX, ansY) where
                      (x', y') = redistributeHalf (x, y)
                      (_, ansY) = redistributeAll (x', Stack.empty)
                      (_, ansX) = redistributeAll (y', Stack.empty)

needRealoc :: Deque a -> Deque a
needRealoc (Deque l r fl fr) | Stack.size l + Stack.size r == 1 = 
                                     let (l', r') = redistributeAll (l, r) in
                                     Deque l' r' (maybeTop r') (maybeTop r')
                                   | Stack.isEmpty l = 
                                     let (r', l') = redistribute (r, l) in
                                     Deque l' r' (maybeTop l') (maybeTop r')
                                   | Stack.isEmpty r =
                                     let (l', r') = redistribute (l, r) in
                                     Deque l' r' (maybeTop l') (maybeTop r')
                                   | otherwise = Deque l r (maybeTop l) (maybeTop r)


popBack :: Deque a -> Deque a
popBack (Deque l r fl fr) | not $ Stack.isEmpty r = 
                            needRealoc $ Deque l (pop r) fl fr
                          | not $ Stack.isEmpty l = 
                            popBack $ Deque l' r' fl fr
                          | otherwise = Deque.empty
                          where (l', r') = redistribute (l, r)

popFront :: Deque a -> Deque a
popFront (Deque l r fl fr) | not $ Stack.isEmpty l =
                             needRealoc $ Deque (pop l) r fl fr
                           | not $ Stack.isEmpty r = 
                             popFront $ Deque l' r' fl fr
                           | otherwise = Deque.empty
                           where (r', l') = redistribute (r, l)

{-
Deque for any type by 2 stack
empty     :: new deque, create empty deque,     O(1)
isEmpty   :: curr deque -> is this deque empty, O(1)
pushBack  :: curr deque -> value -> new deque,  O(1) real time
pushFront :: curr deque -> value -> new deque,  O(1) real time
back      :: curr deque -> back value,          O(1) real time
front     :: curr deque -> fron value,          O(1) real time
popFront  :: curr deque -> new deque,           O(1) amortized time, O(n) worst case
popBack   :: curr deque -> new deque,           O(1) amortized time, O(n) worst case
-}