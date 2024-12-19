module Main where

import Data.List.Split (condense, dropDelims, oneOf, split)

main :: IO ()
main = do
  input <- readFile "07input"
  let eqs = map parseLine (lines input)
  let result = sum $ map check eqs
  print result

readInt :: String -> Int
readInt = read

check :: [Int] -> Int
check (res : vals) = if calc vals res == res then res else 0
check _ = 0

calc :: [Int] -> Int -> Int
calc (x : y : xs) res
  | calc (x + y : xs) res == res = res
  | calc ((x * y) : xs) res == res = res
  | otherwise = calc (concatInts x y : xs) res
calc [x] res = x

concatInts :: Int -> Int -> Int
concatInts x y = readInt (show x ++ show y)

parseLine :: String -> [Int]
parseLine = map readInt . split (condense . dropDelims $ oneOf ": ")
