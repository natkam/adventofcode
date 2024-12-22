module Main where

import Data.List.Split (splitOn)
import Text.Regex.Posix ((=~))

main = do
  input <- readFile "03input"

  print $ getResult (extractPairs input)

  print $ (getResult . extractPairs . cut) input

readInt :: String -> Int
readInt = read

matches :: String -> String -> [[String]]
matches text pattern = text =~ pattern :: [[String]]

extractPairs :: String -> [[Int]]
extractPairs input =
  map
    (map readInt . tail)
    (matches input "mul\\(([0-9]+),([0-9]+)\\)")

getResult :: [[Int]] -> Int
getResult pairs = sum $ map (\[x, y] -> x * y) pairs

-- Only keep parts of the string after a "do()" (of which the string may
-- contain multiple occurrences)
cut :: String -> String
cut = concatMap (head . splitOn "don't()") . splitOn "do()"
