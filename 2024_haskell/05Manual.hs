module Main where

import Data.List (isPrefixOf, isSuffixOf)
import Data.List.Split (splitOn)

-- import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  input <- readFile "05input"
  let [rules, seqsInit] = map lines (splitOn "\n\n" input)
  let seqs = map (splitOn ",") seqsInit
  let validSeqs = filter (validate rules) seqs
  let result = sum $ map getMiddle validSeqs
  print result

readInt :: String -> Int
readInt = read

validate :: [String] -> [String] -> Bool
validate rules [] = True
validate rules (x : xs)
  | all (anyRuleMatches (rulesForPage x rules)) xs = validate rules xs
  | otherwise = False

rulesForPage :: String -> [String] -> [String]
rulesForPage rules x = filter (x `isPrefixOf`) rules

anyRuleMatches :: [String] -> String -> Bool
anyRuleMatches rules p = any (p `isSuffixOf`) rules

getMiddle :: [String] -> Int
getMiddle seq = readInt (seq !! div (length seq) 2)
