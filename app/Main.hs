module Main where

import FileAnalytics (moviesFromFile)
import ListOperations (printElements)

main :: IO ()
main = do
    movies <- moviesFromFile "./static/movies.csv"
    printElements movies
