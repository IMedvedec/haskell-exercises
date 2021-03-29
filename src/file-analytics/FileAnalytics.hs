module FileAnalytics (
    Movie(..)
    ,highestGross
    ,highestIncome
    ,csvRowToMovie
    ,moviesFromFile)
where

import Data.List.Split ( splitOn )
import Data.Char ( isDigit )

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

moviesFromFile :: String -> IO [Maybe Movie]
moviesFromFile path = do
    content <- readFile path
    let strMovies = lines content
    return $ filter (/= Nothing) $ map csvRowToMovie strMovies
    

csvRowToMovie :: String -> Maybe Movie
csvRowToMovie row =
    let 
        values = splitOn "," row
        isValid = 
            length values == 4 && 
            values!!0 /= "" &&
            all isDigit (values!!1) &&
            all isDigit (values!!2) &&
            all isDigit (values!!3)
        movie = Movie {
            title=values!!0
            , year=read $ values!!1
            , gross=read $ values!!2
            , budget=read $ values!!3}
    in if isValid then Just movie else Nothing
