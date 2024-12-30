module Main where

import Data.List (transpose)

main :: IO ()
main = do
  input <- readFile "04input"
  let rows = lines input

  -- rows, reverse rows, cols, reverse cols;
  let horis = length (concatMap fromRow rows)
  let vert = length (concatMap fromRow (transpose rows))
  -- diagonal: right-down, left-up, right-up, left-down
  let rd = length (diag rows)
  let lu = length (diag (reverse rows))
  let ru = length (diag (map reverse rows))
  let ld = length (diag (reverse (map reverse rows)))

  print $ horis + vert + rd + lu + ru + ld

fromRow :: [Char] -> [Int]
fromRow xs =
  [ i
    | i <- [0 .. length xs - 4],
      "XMAS" == map (xs !!) [i .. i + 3]
        || "XMAS" == map (reverse xs !!) [i .. i + 3]
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

{-
....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX
-}
{- DIAGONAL (10): rd - right-down, ru - right-up, etc.
0 rd
1 -
2 -
3 ld
4 -
5 ru lu
6 -
7 -
8 -
9 ru ru,lu ru,lu lu
-}
