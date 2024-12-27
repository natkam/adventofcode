module Main where

main :: IO ()
main = do
  input <- readFile "02input"
  let reports = map (map readInt . words) (lines input)

  let safe = length $ filter (== True) (map isSafe reports)
  print safe

  let safe' = length $ filter (== True) (map isSafe' reports)
  print safe'

readInt :: String -> Int
readInt = read

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) xs (drop 1 xs)

cond1 :: Int -> Bool
cond1 x = x >= 1 && x <= 3

cond2 :: Int -> Bool
cond2 x = x <= -1 && x >= -3

popNth :: [Int] -> Int -> [Int]
popNth xs n = take n xs ++ drop (n + 1) xs

isSafe :: [Int] -> Bool
isSafe r = all cond1 (diffs r) || all cond2 (diffs r)

-- Note: popping the (-1)st item in the list produces the whole list
isSafe' :: [Int] -> Bool
isSafe' r = or [isSafe r' | r' <- map (popNth r) [(-1) .. length r - 1]]
