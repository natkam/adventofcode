{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.List (group, nub, sort, zip)
import Data.List.Utils (countElem)

main = do
  input <- readFile "01input"
  let pairs = [map readInt level | level <- map words (lines input)]
  let [left, right] = [map (!! n) pairs | n <- [0, 1]]
  let diff = sum $ zipWith (curry (abs . uncurry subtract)) (sort left) (sort right)

  print diff

  let diff' = sum [x * countElem x right | x <- left]

  print diff'

readInt :: String -> Int
readInt = read
