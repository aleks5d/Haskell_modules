module LeftistHeap where 

import Data.Maybe

data LeftistHeap a = LeftistHeap{rk :: Int, me :: a, l :: Maybe (LeftistHeap a),
                                                       r :: Maybe (LeftistHeap a)}

instance (Show a) => Show (LeftistHeap a) where
    show (LeftistHeap _ x Nothing Nothing) = show x
    show (LeftistHeap _ x l Nothing) = "[" ++ show l ++ "]<-" ++ show x  
    show (LeftistHeap _ x Nothing r) = show x ++ "->[" ++ show r ++ "]"
    show (LeftistHeap _ x l r) = "[" ++ show l ++ "]<-" ++ show x ++ 
                                "->[" ++ show r ++ "]"

isEmpty :: Ord(a) => Maybe (LeftistHeap a) -> Bool
isEmpty Nothing = True
isEmpty (Just x) = False

rank :: Ord(a) => Maybe (LeftistHeap a) -> Int
rank Nothing = 0
rank (Just x) = rk x

makeHeap :: Ord(a) => a -> Maybe (LeftistHeap a) -> Maybe (LeftistHeap a) 
                                                  -> Maybe (LeftistHeap a)
makeHeap x a b = if rank a >= rank b 
                 then Just $ LeftistHeap (rank b + 1) x a b
                 else Just $ LeftistHeap (rank a + 1) x b a

empty :: Maybe (LeftistHeap a)
empty = Nothing

insert :: Ord(a) => Maybe (LeftistHeap a) -> a -> Maybe (LeftistHeap a)
insert h x = merge (Just $ LeftistHeap 1 x Nothing Nothing) h

merge :: Ord(a) => Maybe (LeftistHeap a) -> Maybe (LeftistHeap a) 
                                          -> Maybe (LeftistHeap a)
merge h1 Nothing = h1
merge Nothing h2 = h2
merge h1@(Just (LeftistHeap _ x a1 b1)) h2@(Just (LeftistHeap _ y a2 b2)) =
                if x <= y
                then makeHeap x a1 $ merge b1 h2
                else makeHeap y a2 $ merge b2 h1

findMin :: Ord(a) => Maybe (LeftistHeap a) -> a
findMin Nothing = error "empty heap"
findMin (Just x) = me x

deleteMin :: Ord(a) => Maybe (LeftistHeap a) -> Maybe (LeftistHeap a)
deleteMin Nothing = Nothing
deleteMin (Just x) = merge (l x) (r x)

fromList :: Ord(a) => [a] -> Maybe (LeftistHeap a)
fromList [] = Nothing
fromList xs = help $ map (\x -> Just $ LeftistHeap 1 x Nothing Nothing) xs
    where 
        help :: Ord(a) => [Maybe (LeftistHeap a)] -> Maybe (LeftistHeap a)
        help [x] = x
        help xs = help $ cut2 xs
        cut2 :: Ord(a) => [Maybe (LeftistHeap a)] -> [Maybe (LeftistHeap a)]
        cut2 [] = []
        cut2 [x] = [x]
        cut2 (x : (y : xs)) = merge x y : cut2 xs

{-
Leftist heap for any ordered type
empty    :: heap
insert   :: curr heap -> value -> new heap, O(log(n)) time
findMin  :: curr heap -> minimum in heap  , O(1)      time
deleteMin:: curr heap -> new heap,          O(log(n)) time
fromList :: list of values -> new heap,     O(n)      time
merge    :: heap1 -> heap2 -> new heap,     O(log(n)) time
-}