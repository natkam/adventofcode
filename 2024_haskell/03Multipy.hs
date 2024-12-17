module Main where

import Text.Regex.Posix

main = do
    input <- readFile "03input"
    let matches = input =~ "mul\\(([0-9]+),([0-9]+)\\)" :: [[String]]
    let pairs = map (map readInt . tail) matches
    let result = sum $ map (\p -> head p * (p !! 1)) pairs

    print result


readInt :: String -> Int
readInt = read
