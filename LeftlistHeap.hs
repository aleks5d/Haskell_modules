module LeftlistHeap where 

import Data.Maybe

data LeftlistHeap a = LeftlistHeap{rk :: Int, me :: a, l :: Maybe (LeftlistHeap a),
                                                       r :: Maybe (LeftlistHeap a)}

instance (Show a) => Show (LeftlistHeap a) where
    show (LeftlistHeap _ x Nothing Nothing) = show x
    show (LeftlistHeap _ x l Nothing) = "[" ++ show l ++ "]<-" ++ show x  
    show (LeftlistHeap _ x Nothing r) = show x ++ "->[" ++ show r ++ "]"
    show (LeftlistHeap _ x l r) = "[" ++ show l ++ "]<-" ++ show x ++ 
                                "->[" ++ show r ++ "]"

isEmpty :: Ord(a) => Maybe (LeftlistHeap a) -> Bool
isEmpty Nothing = True
isEmpty (Just x) = False

rank :: Ord(a) => Maybe (LeftlistHeap a) -> Int
rank Nothing = 0
rank (Just x) = rk x

makeHeap :: Ord(a) => a -> Maybe (LeftlistHeap a) -> Maybe (LeftlistHeap a) 
                                                  -> Maybe (LeftlistHeap a)
makeHeap x a b = if rank a >= rank b 
                 then Just $ LeftlistHeap (rank b + 1) x a b
                 else Just $ LeftlistHeap (rank a + 1) x b a

empty :: Maybe (LeftlistHeap a)
empty = Nothing

insert :: Ord(a) => Maybe (LeftlistHeap a) -> a -> Maybe (LeftlistHeap a)
insert h x = merge (Just $ LeftlistHeap 1 x Nothing Nothing) h

merge :: Ord(a) => Maybe (LeftlistHeap a) -> Maybe (LeftlistHeap a) 
                                          -> Maybe (LeftlistHeap a)
merge h1 Nothing = h1
merge Nothing h2 = h2
merge h1@(Just (LeftlistHeap _ x a1 b1)) h2@(Just (LeftlistHeap _ y a2 b2)) =
                if x <= y
                then makeHeap x a1 $ merge b1 h2
                else makeHeap y a2 $ merge b2 h1

findMin :: Ord(a) => Maybe (LeftlistHeap a) -> a
findMin Nothing = error "empty heap"
findMin (Just x) = me x

deleteMin :: Ord(a) => Maybe (LeftlistHeap a) -> Maybe (LeftlistHeap a)
deleteMin Nothing = Nothing
deleteMin (Just x) = merge (l x) (r x)

fromList :: Ord(a) => [a] -> Maybe (LeftlistHeap a)
fromList [] = Nothing
fromList xs = help $ map (\x -> Just $ LeftlistHeap 1 x Nothing Nothing) xs
    where 
        help :: Ord(a) => [Maybe (LeftlistHeap a)] -> Maybe (LeftlistHeap a)
        help [x] = x
        help xs = help $ cut2 xs
        cut2 :: Ord(a) => [Maybe (LeftlistHeap a)] -> [Maybe (LeftlistHeap a)]
        cut2 [] = []
        cut2 [x] = [x]
        cut2 (x : (y : xs)) = merge x y : cut2 xs