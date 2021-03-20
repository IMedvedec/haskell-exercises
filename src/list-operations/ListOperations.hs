module ListOperations(len) where

len :: Integral b => [a] -> b
len [] = 0
len (_:xs) = 1 + len xs