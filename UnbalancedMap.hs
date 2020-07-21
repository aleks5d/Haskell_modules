module UnbalancedMap where

import Data.Maybe

data UnbalancedMap a b = UnbalancedMap {l :: Maybe (UnbalancedMap a b), 
                        r :: Maybe (UnbalancedMap a b), me :: a, val :: b}

instance (Show a, Show b) => Show (UnbalancedMap a b) where
    show (UnbalancedMap Nothing Nothing x y) = show (x, y)
    show (UnbalancedMap Nothing r x y) = show (x, y) ++ 
                                         "->[" ++ show r ++ "]"
    show (UnbalancedMap l Nothing x y) = "[" ++ show l ++ "]<-" ++ 
                                         show (x, y)
    show (UnbalancedMap l r x y) =       "[" ++ show l ++ "]<-" ++ 
                                         show (x, y) ++ 
                                         "->[" ++ show r ++ "]"
                                         
empty :: Maybe (UnbalancedMap a b)
empty = Nothing

find :: (Ord a) => Maybe (UnbalancedMap a b) -> a -> Maybe b
find Nothing _ = Nothing
find (Just x) y | y == me x = Just $ val x
                | y < me x = find (l x) y
                | otherwise = find (r x) y

merge :: (Ord a) => Maybe (UnbalancedMap a b) -> Maybe (UnbalancedMap a b) 
                                              -> Maybe (UnbalancedMap a b)
merge Nothing Nothing = Nothing 
merge x Nothing = x
merge Nothing x = x
merge (Just x) y = Just $ UnbalancedMap (l x) (merge (r x) y) (me x) (val x)

insert :: (Ord a) => Maybe (UnbalancedMap a b) -> a -> b 
                                                    -> Maybe (UnbalancedMap a b)
insert Nothing y v = Just $ UnbalancedMap Nothing Nothing y v
insert (Just x) y v | me x == y = Just $ x{val = v} 
                    | me x > y = Just $ UnbalancedMap (insert (l x) y v) (r x) 
                                                                    (me x) (val x)
                    | otherwise = Just $ UnbalancedMap (l x) (insert (r x) y v) 
                                                                    (me x) (val x)

erase :: (Ord a) => Maybe (UnbalancedMap a b) -> a -> Maybe (UnbalancedMap a b)
erase Nothing y = Nothing
erase (Just x) y | me x == y = UnbalancedMap.merge (l x) (r x)
                 | me x < y = Just $ UnbalancedMap (l x) (erase (r x) y) 
                                                                    (me x) (val x)
                 | otherwise = Just $ UnbalancedMap (erase (l x) y) (r x) 
                                                                    (me x) (val x)

{-
Unbalanced Map a->b for any ordered a and any b
insert: O(n) worst case
erase : O(n) worst case
find  : O(n) worst case
-}                                                            