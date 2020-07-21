module Array where

import Data.Maybe

data Node a = Leaf {val :: Maybe a} | Node {l :: Node a, r :: Node a}

data Array a = Array {arr :: Node a, sz :: Int, cp :: Int}

instance Show a => Show (Node a) where
    show (Leaf Nothing) = ""
    show (Leaf (Just x)) = show x
    show (Node x y) = show x ++ help (show y)
        where help "" = ""
              help s = ", " ++ s

instance Show a => Show (Array a) where
    show x = "["++ show (Array.arr x) ++ "]";

empty :: Array a
empty = Array (Leaf Nothing) 0 1

isEmpty :: Array a -> Bool
isEmpty x = sz x == 0

size :: Array a -> Int
size x = sz x

capacity :: Array a -> Int
capacity x = cp x

get ::Array a -> Int -> a
get x ind | (ind < 0 || ind >= sz x) = error "bad index"
           | otherwise = get' (arr x) 0 (cp x) ind

get' :: Node a -> Int -> Int -> Int -> a
get' (Leaf Nothing) _ _ _ = error "No element"
get' (Leaf (Just x)) _ _ _ = x
get' x l r ind | m > ind = get' (Array.l x) l m ind
               | otherwise = get' (Array.r x) m r ind
               where m = (r + l) `div` 2

update :: Array a -> Int -> a -> Array a
update x ind el | ind < 0 || ind > cp x = error "bad index"
                | otherwise = x {arr = update' (arr x) 0 (cp x) ind el}

update' :: Node a -> Int -> Int -> Int -> a -> Node a
update' (Leaf _) _ _ _ el = Leaf $ Just el
update' (Node lN rN) l r ind el | ind < m = Node (update' lN l m ind el) rN
                                | otherwise = Node lN (update' rN m r ind el)
                                where m = (r + l) `div` 2

realloc :: Array a -> Array a
realloc (Array x sz cp) = Array (Node x $ clear x) sz (cp * 2)
    where clear (Leaf _) = Leaf Nothing
          clear (Node x y) = Node (clear x) (clear y)

pushBack :: Array a -> a -> Array a
pushBack x y | sz x == cp x = pushBack (realloc x) y
             | otherwise = (update x (sz x) y) {sz = (sz x + 1)}

popBack :: Array a -> Array a
popBack x | sz x == 0 = x
          | otherwise = x {sz = (sz x - 1)}


toList' :: Node a -> [a]
toList' (Leaf Nothing) = []
toList' (Leaf (Just x)) = [x]
toList' (Node l r) = toList' l ++ toList' r

toList :: Array a -> [a]
toList x = toList' $ arr x

fromList :: [a] -> Array a
fromList arr = foldl pushBack empty arr

ofSize' :: Int -> (Node a, Int)
ofSize' sz | sz <= 1 = (Leaf Nothing, 1)
           | otherwise = (Node x x, y * 2)
           where (x, y) = ofSize' $ (sz + 1) `div` 2

ofSize :: Int -> Array a
ofSize sz | sz < 0 =  error "bad size"
          | otherwise = Array arr sz cap 
          where (arr, cap) = ofSize' sz

{-

-}