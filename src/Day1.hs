module Day1
  ( day1,
  )
where

import Data.List (tails, transpose)

loadValues :: Read b => FilePath -> IO [b]
loadValues fname = do
  contents <- readFile fname
  let asLines = lines contents
  let values = map read asLines
  return values

windows :: Int -> [a] -> [[a]]
windows n = transpose . take n . tails

findIncreases1 depths =
  length [(a, b) | (a, b) <- zip depths (tail depths), b > a]

findIncreases3 depths =
  length [(sum a, sum b) | (a, b) <- zip wins (tail wins), sum b > sum a]
  where
    wins = windows 3 depths

day1 :: IO ()
day1 = do
  depths <- loadValues "data/day1_input.txt" :: IO [Int]
  print $ findIncreases1 depths
  print $ findIncreases3 depths
