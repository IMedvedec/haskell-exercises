module ListOperations(len,sumEven,toReverseDigits,toDigits) where

len :: Integral b => [a] -> b
len [] = 0
len (_:xs) = 1 + len xs

sumEven :: Num a => [a] -> a
sumEven [] = 0
sumEven (x:_:xs) = x + sum xs

toReverseDigits :: Integral a => a -> [a]
toReverseDigits x
    | x <= 0 = []
    | otherwise = (x `mod` 10) : toReverseDigits (x `div` 10)

toDigits :: Integral a => a -> [a]
toDigits x = reverse $ toReverseDigits x