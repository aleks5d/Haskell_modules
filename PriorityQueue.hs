module PriorityQueue where

data PriorityQueue k v = PQE | PriorityQueue{key :: k, val :: v, l :: PriorityQueue k v,
                                            r :: PriorityQueue k v, sz :: Int}

instance (Show(k), Show(v), Ord(k)) => Show (PriorityQueue k v) where
    show PQE = ""
    show pq =  show (getMin pq) ++ "[" ++ show (l pq) ++ "][" ++ show (r pq) ++ "]"

empty :: Ord(k) => PriorityQueue k v
empty = PQE

isEmpty :: Ord(k) => PriorityQueue k v -> Bool
isEmpty PQE = True
isEmpty _   = False

size :: Ord(k) => PriorityQueue k v -> Int
size PQE = 0
size pq  = sz pq

insert :: Ord(k) => PriorityQueue k v -> k -> v -> PriorityQueue k v
insert PQE k' v' = PriorityQueue k' v' empty empty 1
insert pq  k' v' | k'' < k' = if size lt > size rt 
                              then pq{r = insert rt k' v', sz = size pq + 1}
                              else pq{l = insert lt k' v', sz = size pq + 1}
                 | otherwise = if size lt > size rt
                               then PriorityQueue k' v' lt (insert rt k'' v'') (size pq + 1)
                               else PriorityQueue k' v' (insert lt k'' v'') rt (size pq + 1)
                 where
                    k'' = key pq
                    v'' = val pq 
                    lt = l pq
                    rt = r pq

insertP :: Ord(k) => PriorityQueue k v -> (k, v) -> PriorityQueue k v
insertP pq (x, y) = insert pq x y

getMinVal :: Ord(k) => PriorityQueue k v -> v
getMinVal PQE = error "empty pq"
getMinVal pq = val pq

getMinKey :: Ord(k) => PriorityQueue k v -> k
getMinKey PQE = error "empty pq"
getMinKey pq = key pq

getMin :: Ord(k) => PriorityQueue k v -> (k, v)
getMin PQE = error "empty pq"
getMin pq = (key pq, val pq)

changeMin :: Ord(k) => PriorityQueue k v -> k -> v -> PriorityQueue k v
changeMin PQE _ _ = PQE
changeMin pq x y | isEmpty $ l pq = insert empty x y
                 | isEmpty $ r pq = insertP (insert empty x y) $ getMin $ l pq
                 | x < min lx rx = pq{key = x, val = y}
                 | lx < rx   = pq{key = lx, val = ly, l = changeMin lt x y}
                 | otherwise = pq{key = rx, val = ry, r = changeMin rt x y}
                 where
                    lt = l pq
                    rt = r pq
                    (lx, ly) = getMin lt
                    (rx, ry) = getMin rt