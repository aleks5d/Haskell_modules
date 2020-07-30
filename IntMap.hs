module IntMap where

import Data.Maybe

data IntMap a = IntMap {val :: Maybe a, l :: IntMap a, r :: IntMap a}

empty :: IntMap a
empty = IntMap Nothing empty empty

put :: Integral b => IntMap a -> b -> a -> IntMap a
put map 0 y = map {val = Just y}
put map n y = if n `mod` 2 == 1 then map {r = put (r map) (n `div` 2) y}
                                else map {l = put (l map) (n `div` 2) y}

maybeGet :: Integral b => IntMap a -> b -> Maybe a
maybeGet map 0 = val map
maybeGet map n = if n `mod` 2 == 1 then maybeGet (r map) (n `div` 2)
                                   else maybeGet (l map) (n `div` 2)

get :: Integral b => IntMap a -> b -> a
get (IntMap Nothing _ _) 0 = error "empty index"
get map 0 = fromJust $ val map
get map n = if n `mod` 2 == 1 then get (r map) (n `div` 2)
                              else get (l map) (n `div` 2)

toList :: Integral b => b -> b -> a -> IntMap a -> [a]
toList from to def map | from > to = []
                       | otherwise = (f' def $ maybeGet map from) :
                                             toList (from + 1) to def map
                       where f' def Nothing = def
                             f' _ (Just x) = x

{-
IntMap Int -> a for any type
put: O(log(key)) real time
get: O(log(key)) real time
toList: left bound -> right bound -> default value -> intMap
-}