module Main where

import Advent2021.Day1
import Advent2021.Day2
import Lib

banner :: [Char] -> IO ()
banner label = putStrLn $ "\n= " ++ label ++ " ==============="

main :: IO ()
main = do
  banner "Day 1"
  day1

  banner "Day 2"
  day2

  putStrLn ""
