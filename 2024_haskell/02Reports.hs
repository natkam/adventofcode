module Main where

import Data.Ix (inRange)
import Data.List (elemIndices)

main = do
  input <- readFile "02testinput"
  let reports = map (map readInt . words) (lines input)
  let safe = countSafe (inRange (-3, -1)) reports + countSafe (inRange (1, 3)) reports

  print safe

  let safe' = countSafe' (inRange (-3, -1)) reports + countSafe' (inRange (1, 3)) reports

  print safe'

readInt :: String -> Int
readInt = read

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) xs (drop 1 xs)

checkReport :: (Int -> Bool) -> [Int] -> Bool
checkReport cond = all cond . diffs

checkReport' :: (Int -> Bool) -> [Int] -> Bool
checkReport' cond = almostAll cond . diffs

almostAll :: (Int -> Bool) -> [Int] -> Bool
almostAll cond xs
  | all cond xs = True
  | falseCount == 1 = True -- nope, it has to work after removing a *level*!
  | falseCount <= 2 && (idx !! 1 - head idx) == 1 = True
  | otherwise = False
  where
    falseCount = countTrue (map (not . cond) xs)
    idx = elemIndices False (map cond xs)

-- condAsc :: Int -> Bool
-- condAsc x = x <= 3 && x >= 1

-- condDesc :: Int -> Bool
-- condDesc x = x >= -3 && x <= -1

-- getSafe :: (Int -> Bool) -> [[Int]] -> [Bool]
countTrue :: [Bool] -> Int
countTrue = length . filter (== True)

countSafe :: (Int -> Bool) -> [[Int]] -> Int
countSafe cond reports = countTrue (map (checkReport cond) reports)

countSafe' :: (Int -> Bool) -> [[Int]] -> Int
countSafe' cond reports = countTrue (map (checkReport' cond) reports)
