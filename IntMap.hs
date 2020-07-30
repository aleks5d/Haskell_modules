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
IntMap - data structure to storage (a type) values by not negative Int / Integer keys
empty    :: IntMap of (a type)
put      :: curr IntMap -> index -> new value -> new IntMap
maybeGet :: curr IntMap -> index -> Nothing - value wasn't inserted
									Just x  - value was inserted and it's x
get      :: curr IntMap -> index -> value   - if value wasn't inserted will throw error
toList   :: left bound -> right bound -> default value -> curr IntMap -> [values]
	form list of values with keys in [left, right]. if it's no such value then use default

toList work in O((right - left) * log(right)) time
other operations in O(log(key))

-}