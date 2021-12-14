module Advent2021.Day1
  ( day1,
    findIncreases1,
    findIncreases3,
    mkDepths,
  )
where

import Data.List (tails, transpose)
import Lib

windows :: Int -> [a] -> [[a]]
windows n = transpose . take n . tails

findIncreases1 :: [Int] -> Int
findIncreases1 depths =
  length [(a, b) | (a, b) <- zip depths (tail depths), b > a]

findIncreases3 :: [Int] -> Int
findIncreases3 depths =
  length [(sum a, sum b) | (a, b) <- zip wins (tail wins), sum b > sum a]
  where
    wins = windows 3 depths

mkDepths :: String -> [Int]
mkDepths str = map read $ lines str

day1 :: IO ()
day1 = do
  contents <- readFile "data/day1_input.txt"
  let depths = mkDepths contents

  putStr "win1: "
  print $ findIncreases1 depths
  putStr "win3: "
  print $ findIncreases3 depths
