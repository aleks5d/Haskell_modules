module IntMap where

import Data.Maybe

data IntMap a = IntMap {val :: Maybe a, l :: IntMap a, r :: IntMap a}

empty :: IntMap a
empty = IntMap Nothing empty empty

put :: IntMap a -> Int -> a -> IntMap a
put map 0 y = map {val = Just y}
put map n y = if n `mod` 2 == 1 then map {r = put (r map) (n `div` 2) y}
                                else map {l = put (l map) (n `div` 2) y}

maybeGet :: IntMap a -> Int -> Maybe a
maybeGet map 0 = val map
maybeGet map n = if n `mod` 2 == 1 then maybeGet (r map) (n `div` 2)
                                   else maybeGet (l map) (n `div` 2)

get :: IntMap a -> Int -> a
get (IntMap Nothing _ _) 0 = error "empty index"
get map 0 = fromJust $ val map
get map n = if n `mod` 2 == 1 then get (r map) (n `div` 2)
                              else get (l map) (n `div` 2)

{-
IntMap Int -> a for any type
put: O(log(key)) real time
get: O(log(key)) real time
-}