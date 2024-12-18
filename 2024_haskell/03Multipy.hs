module Main where

import Data.List.Split (splitOn)
import Text.Regex.Posix ((=~))

main = do
  input <- readFile "03input"

  print $ getResult (extractPairs input)

  -- The input is split on "don't()"s, producing a list. The first elem should
  -- be kept, so I prefix it with "do()". All the subsequent elems started
  -- with "don't()", and I only want to keep their part after a "do()".
  print $ (getResult . extractPairs . cutOutDon'ts) ("do()" ++ input)

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
getDos :: String -> String
getDos s = case splitOn "do()" s of
  x : xs -> concat xs
  _ -> ""

cutOutDon'ts :: String -> String
cutOutDon'ts input = concatMap getDos (splitOn "don't()" input)
