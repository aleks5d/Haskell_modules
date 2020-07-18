module Queue where

import Stack

data Queue a = Queue {f :: Stack a, s :: Stack a, fr :: Maybe a} 

instance Show a => Show (Queue a) where
    show (Queue l r _) = "[[" ++ show l ++ "],[" ++ show r ++ "]]"

empty :: Queue a
empty = Queue Stack.empty Stack.empty Nothing

isEmpty :: Queue a -> Bool
isEmpty (Queue f s _ ) = Stack.isEmpty f && Stack.isEmpty s

push :: Queue a -> a -> Queue a
push (Queue f s Nothing) x = Queue f (Stack.push s x) (Just x)
push (Queue f s fr) x = Queue f (Stack.push s x) fr

realloc :: Queue a -> Queue a
realloc (Queue f s fr)  | Stack.isEmpty s = Queue f s (Stack.maybeTop f)
                        | otherwise = realloc $ Queue 
                         (Stack.push f $ Stack.top s) (Stack.pop s) fr

needRealloc :: Queue a -> Queue a
needRealloc (Queue f s fr) | Stack.isEmpty f = realloc $ Queue f s fr
                           | otherwise = Queue f s $ Stack.maybeTop f

size :: Queue a -> Int
size (Queue f s fr) = Stack.size f + Stack.size s

front :: Queue a -> a
front (Queue _ _ Nothing) = error "no front element"
front (Queue _ _ (Just x)) = x

maybeFront :: Queue a -> Maybe a
maybeFront (Queue _ _ x) = x

pop :: Queue a -> Queue a
pop (Queue f s fr) | not $ Stack.isEmpty f = needRealloc $ Queue (Stack.pop f) s fr
                   | not $ Stack.isEmpty s = Queue.pop $ realloc (Queue f s fr)
                   | otherwise = (Queue Stack.empty Stack.empty Nothing) 
