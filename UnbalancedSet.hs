module UnbalancedSet where

import Data.Maybe

data UnbalancedSet a = UnbalancedSet {l :: Maybe (UnbalancedSet a), 
                                    r :: Maybe (UnbalancedSet a), me :: a}
                            
instance Show a => Show (UnbalancedSet a) where
    show (UnbalancedSet Nothing Nothing x) = show x
    show (UnbalancedSet Nothing (Just r) x) = show x ++ "->[" ++ show r ++ "]"
    show (UnbalancedSet (Just l) Nothing x) = "[" ++ show l ++ "]<-" ++ show x
    show (UnbalancedSet (Just l) (Just r) x) = "[" ++ show l ++ "]<-" ++ show x ++
                                             "->[" ++ show r ++ "]"

empty :: Maybe (UnbalancedSet a)
empty = Nothing

find :: (Ord a) => Maybe (UnbalancedSet a) -> a -> Bool
find Nothing _ = False
find (Just x) y | y == me x = True
                | y < me x = find (l x) y
                | otherwise = find (r x) y

merge :: (Ord a) => Maybe (UnbalancedSet a) -> Maybe (UnbalancedSet a) 
                                            -> Maybe (UnbalancedSet a)
merge Nothing Nothing = Nothing 
merge x Nothing = x
merge Nothing x = x
merge (Just x) y = Just $ UnbalancedSet (l x) (merge (r x) y) (me x)


insert :: (Ord a) => Maybe (UnbalancedSet a) -> a -> Maybe (UnbalancedSet a)
insert Nothing y = Just $ UnbalancedSet Nothing Nothing y
insert (Just x) y | me x == y = Just x
                  | me x > y = Just $ UnbalancedSet (insert (l x) y) (r x) (me x)
                  | otherwise = Just $ UnbalancedSet (l x) (insert (r x) y) (me x)

erase :: (Ord a) => Maybe (UnbalancedSet a) -> a -> Maybe (UnbalancedSet a)
erase Nothing y = Nothing
erase (Just x) y | me x == y = merge (l x) (r x)
                 | me x < y = Just $ UnbalancedSet (l x) (erase (r x) y) (me x)
                 | otherwise = Just $ UnbalancedSet (erase (l x) y) (r x) (me x)