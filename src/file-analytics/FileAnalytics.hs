module FileAnalytics (
    Movie(..)
    ,highestGross
    ,highestIncome)
where

data Movie = Movie {
    title :: String
    , year :: Int
    , gross :: Float
    , budget :: Float
} deriving (Show,Eq)

highestGross :: [Movie] -> Maybe Movie
highestGross [] = Nothing
highestGross [m] = Just m
highestGross (m1:m2:ms) = highestGross $ (if gross m1 >= gross m2 then m1 else m2):ms

highestIncome :: [Movie] -> Maybe Movie
highestIncome [] = Nothing
highestIncome [m] = Just m
highestIncome (m1:m2:ms)
    | income m1 > income m2 = highestIncome $ m1 : ms
    | otherwise = highestIncome $ m2 : ms
    where income m = gross m - budget m