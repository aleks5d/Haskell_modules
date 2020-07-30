module DSU where 

import IntMap

data DSU a = DSU {pre :: IntMap a, sz :: IntMap a}

empty :: DSU a
empty = DSU IntMap.empty IntMap.empty

putPrev :: Integral a => DSU a -> a -> a -> DSU a
putPrev dsu ind prev = dsu{pre = IntMap.put (pre dsu) ind prev}

getPrev :: Integral a => DSU a -> a -> (DSU a, a)
getPrev dsu ind = case IntMap.maybeGet (pre dsu) ind of
                        Nothing -> (dsu, ind)
                        Just x -> (putPrev dsu' ind y, y) where (dsu', y) = getPrev dsu x

getSize :: Integral a => DSU a -> a -> a
getSize dsu ind = case IntMap.maybeGet (sz dsu) ind of
                    Nothing -> 1
                    Just x -> x

updSize :: Integral a => DSU a -> a -> a -> DSU a
updSize dsu ind newSz = dsu{sz = IntMap.put (sz dsu) ind newSz}

link :: Integral a => DSU a -> a -> a -> DSU a
link dsu l r = updSize (putPrev dsu l r) r (getSize dsu l + getSize dsu r)

merge :: Integral a => DSU a -> a -> a -> (DSU a, Bool)
merge dsu l r | l' == r' = (dsu'', False)
              | otherwise = if getSize dsu l' < getSize dsu r' then (link dsu'' l' r', True)
                                                               else (link dsu'' r' l', True)
              where 
                (dsu',  l') = getPrev dsu  l
                (dsu'', r') = getPrev dsu' r

{-
empty   :: empty dsu
getPrev :: curr dsu -> index -> (new dsu, previous of index), O(log(n) * alpha(n)) amortized
putPrev :: curr dsu -> index -> newPrevious -> new dsu      , O(log(n))
getSize :: curr dsu -> index -> size of index subtree       , O(log(n))
updSize :: curr dsu -> index -> newSize -> new dsu          , O(log(n))
link    :: curr dsu -> child -> parent  -> new dsu          , O(log(n))
merge   :: curr dsu -> firs  -> second  -> (new dsu, merged), O(log(n) * alpha(n)) amortized
-}