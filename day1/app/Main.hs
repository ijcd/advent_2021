module Main where

import Data.List

readInts :: Read b => FilePath -> IO [b]
readInts fname = do
  contents <- readFile fname
  let asLines = lines contents
  let numbers = map read asLines
  return numbers

windows :: Int -> [a] -> [[a]]
windows n = transpose . take n . tails

findIncreases1 depths =
  length [(a, b) | (a, b) <- zip depths (tail depths), b > a]

findIncreases3 depths =
  length [(sum a, sum b) | (a, b) <- zip wins (tail wins), sum b > sum a]
  where
    wins = windows 3 depths

main :: IO ()
main = do
  depths <- readInts "input.txt" :: IO [Int]
  print $ findIncreases1 depths
  print $ findIncreases3 depths
