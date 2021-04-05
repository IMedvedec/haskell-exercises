module Stack (
    Stack(..),
    pop,
    push,
    pop',
    push'
) where

import Control.Monad.State

type Stack a = [a]

pop :: Stack a -> (a, Stack a)
pop (x:xs) = (x,xs)

push :: a -> Stack a -> ((), Stack a)
push a xs = ((),a:xs)

pop' :: State (Stack a) a
pop' = state $ \(x:xs) -> (x,xs)

push' :: a -> State (Stack a) ()
push' a = state $ \xs -> ((),a:xs)
