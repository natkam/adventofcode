module Main where

import Data.List (transpose)

main :: IO ()
main = do
  input <- readFile "04input"
  let rows = lines input

  -- rows left-right & right-left, cols up-down & down-up;
  let lr = fromRows rows
  let rl = fromRows (map reverse rows)
  let ud = fromRows (transpose rows)
  let du = fromRows ((transpose . reverse) rows)
  -- diagonal: right-down, left-up, right-up, left-down
  let rd = diag rows
  let lu = diag (reverse rows)
  let ru = diag (map reverse rows)
  let ld = diag (reverse (map reverse rows))

  let result1 = lr + rl + ud + du + rd + lu + ru + ld
  print result1

  -- positions of the 'M'-s: left, right, bottom, top
  let left = crosses rows
  let right = crosses (map reverse rows)
  let top = crosses (transpose rows)
  let bottom = crosses ((transpose . reverse) rows)

  let result2 = left + right + top + bottom
  print result2

fromRows :: [[Char]] -> Int
fromRows rows =
  length
    [ (i, j)
      | i <- [0 .. length rows - 4],
        j <- [0 .. length (rows !! 0) - 1],
        "XMAS" == map ((rows !! j) !!) [i .. i + 3]
    ]

diag :: [[Char]] -> Int
diag rows =
  length
    [ (i, j)
      | i <- [0 .. length rows - 4],
        j <- [0 .. length (rows !! 0) - 4],
        'X' == (rows !! j) !! i
          && 'M' == (rows !! (j + 1)) !! (i + 1)
          && 'A' == (rows !! (j + 2)) !! (i + 2)
          && 'S' == (rows !! (j + 3)) !! (i + 3)
    ]

crosses :: [[Char]] -> Int
crosses rows =
  length
    [ (i, j)
      | i <- [0 .. length rows - 3],
        j <- [0 .. length (rows !! 0) - 3],
        'M' == (rows !! j) !! i
          && 'M' == (rows !! (j + 2)) !! i
          && 'A' == (rows !! (j + 1)) !! (i + 1)
          && 'S' == (rows !! j) !! (i + 2)
          && 'S' == (rows !! (j + 2)) !! (i + 2)
    ]
