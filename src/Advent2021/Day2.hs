{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Advent2021.Day2
  ( day2,
    runCommands,
    mkCommands,
    Position (..),
  )
where

-- import Debug.Trace

import Data.Coerce (coerce)

newtype Horizontal = Horizontal Int
  deriving (Show, Eq)
  deriving newtype (Read, Num)

newtype Depth = Depth Int
  deriving (Show, Eq)
  deriving newtype (Read, Num)

data Position = Position
  { horizontal :: Horizontal,
    depth :: Depth
  }
  deriving (Show, Eq)

data Command
  = Forward Horizontal
  | Up Depth
  | Down Depth
  deriving (Show)

mkCommand :: String -> Command
mkCommand line =
  case words line of
    "forward" : d : _ -> Forward (read d :: Horizontal)
    "down" : d : _ -> Down (read d :: Depth)
    "up" : d : _ -> Up (read d :: Depth)
    _ -> undefined

mkCommands :: String -> [Command]
mkCommands str = map mkCommand $ lines str

updatePosition :: Position -> Command -> Position
updatePosition pos@Position {..} cmd =
  case cmd of
    Forward n -> pos {horizontal = horizontal + n}
    Up n -> pos {depth = depth - n}
    Down n -> pos {depth = depth + n}

runCommands :: [Command] -> Position -> Position
runCommands cmds pos = foldl updatePosition pos cmds

day2 :: IO ()
day2 = do
  contents <- readFile "data/day2_input.txt"
  let cmds = mkCommands contents

  let Position {..} = runCommands cmds (Position 0 0)
  putStr "horiz * depth: "
  print (coerce horizontal * coerce depth :: Int)