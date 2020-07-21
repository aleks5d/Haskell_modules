module Stack where

import Data.Maybe

data Stack a = Stack {me :: Maybe a, prev :: Maybe (Stack a), sz :: Int} 
                
instance Show a => Show (Stack a) where
    show (Stack Nothing Nothing _) = ""
    show (Stack (Just x) Nothing _) = show x
    show (Stack Nothing (Just x) _) = show x
    show (Stack (Just x) (Just y) _) = show x  ++ "," ++  show y

empty :: Stack a
empty = Stack Nothing Nothing 0

isEmpty :: Stack a -> Bool
isEmpty (Stack Nothing Nothing _) = True
isEmpty (Stack _ _ _) = False

size :: Stack a -> Int
size (Stack _ _ x) = x

maybeTop :: Stack a -> Maybe a
maybeTop (Stack x _ _) = x 

top :: Stack a -> a
top (Stack Nothing _ _) = error "no top element"
top (Stack (Just x) _ _) = x

pop :: Stack a -> Stack a
pop (Stack _ Nothing _) = Stack Nothing Nothing 0
pop (Stack _ (Just x) _) = x

push :: Stack a -> a -> Stack a
push x y = Stack (Just y) (Just x) (size x + 1)

{-
any type Stack
push: O(1) real time
top : O(1) real time
pop : O(1) real time
size: O(1) real time
-}
