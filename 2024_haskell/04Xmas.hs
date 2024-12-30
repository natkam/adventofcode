module Main where

import Data.List (transpose)

main :: IO ()
main = do
  input <- readFile "04input"
  let rows = lines input

  -- rows left-right & right-left, cols up-down & down-up;
  let lr = length (concatMap fromRow rows)
  let rl = length (concatMap (fromRow . reverse) rows)
  let ud = length (concatMap fromRow (transpose rows))
  let du = length (concatMap (fromRow . reverse) (transpose rows))
  -- diagonal: right-down, left-up, right-up, left-down
  let rd = length (diag rows)
  let lu = length (diag (reverse rows))
  let ru = length (diag (map reverse rows))
  let ld = length (diag (reverse (map reverse rows)))

  let result1 = lr + rl + ud + du + rd + lu + ru + ld
  print result1

fromRow :: [Char] -> [Int]
fromRow xs =
  [ i
    | i <- [0 .. length xs - 4],
      "XMAS" == map (xs !!) [i .. i + 3]
  ]

diag :: [[Char]] -> [(Int, Int)]
diag rows =
  [ (i, j)
    | j <- [0 .. length rows - 4],
      i <- [0 .. length (rows !! 0) - 4],
      'X' == (rows !! j) !! i
        && 'M' == (rows !! (j + 1)) !! (i + 1)
        && 'A' == (rows !! (j + 2)) !! (i + 2)
        && 'S' == (rows !! (j + 3)) !! (i + 3)
  ]
