{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.List (group, nub, sort, zip)
import Data.List.Utils (countElem)

main = do
  input <- readFile "01input"
  let pairs = map words $ lines input

  let left = sortNthCol 0 pairs
  let right = sortNthCol 1 pairs
  let diff = sum $ zipWith (curry (abs . uncurry subtract)) left right

  print diff

  let diff' = sum [x * countElem x right | x <- left]

  print diff'

readInt :: String -> Int
readInt = read

sortNthCol :: Int -> [[String]] -> [Int]
sortNthCol n = sort . map (readInt . (!! n))
