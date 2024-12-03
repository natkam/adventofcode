{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.List (sort, zip)

-- Adapted from this answer: https://stackoverflow.com/a/7867786
main = do
  input <- readFile "01input"
  let pairs = map words $ lines input

  let left = sortNthCol 0 pairs
  let right = sortNthCol 1 pairs
  let diff = sum $ zipWith (curry (abs . uncurry subtract)) left right

  print diff

readInt :: String -> Int
readInt = read

sortNthCol :: Int -> [[String]] -> [Int]
sortNthCol n = sort . map (readInt . (!! n))
