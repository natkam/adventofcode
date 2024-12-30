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

  -- positions of the 'X'-s: left, right, bottom, top
  let left = length (cross rows)
  let right = length (cross (map reverse rows))
  let top = length ((cross . transpose) rows)
  let bottom = length ((cross . transpose) (reverse rows))

  let result2 = left + right + top + bottom
  print result2

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

cross :: [[Char]] -> [(Int, Int)]
cross rows =
  [ (i, j)
    | i <- [0 .. length rows - 3],
      j <- [0 .. length (rows !! 0) - 3],
      'M' == (rows !! j) !! i
        && 'M' == (rows !! (j + 2)) !! i
        && 'A' == (rows !! (j + 1)) !! (i + 1)
        && 'S' == (rows !! j) !! (i + 2)
        && 'S' == (rows !! (j + 2)) !! (i + 2)
  ]
