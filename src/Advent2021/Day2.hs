{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Advent2021.Day2
  ( day2,
    runCommandsWithoutAim,
    runCommandsWithAim,
    mkCommands,
    Position (..),
    PositionWithAim (..),
    Horizontal (..),
    Depth (..),
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

newtype Aim = Aim Int
  deriving (Show, Eq)
  deriving newtype (Read, Num)

data Position = Position
  { horizontal :: Horizontal,
    depth :: Depth
  }
  deriving (Show, Eq)

data PositionWithAim = PositionWithAim
  { horizontal :: Horizontal,
    depth :: Depth,
    aim :: Aim
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

updatePositionWithAim :: PositionWithAim -> Command -> PositionWithAim
updatePositionWithAim pos@PositionWithAim {..} cmd =
  case cmd of
    Forward n -> pos {horizontal = horizontal + n, depth = coerce depth + (coerce aim * coerce n)}
    Up n -> pos {aim = coerce aim - coerce n}
    Down n -> pos {aim = coerce aim + coerce n}

runCommandsWithoutAim :: [Command] -> Position -> Position
runCommandsWithoutAim cmds pos = foldl updatePosition pos cmds

runCommandsWithAim :: [Command] -> PositionWithAim -> PositionWithAim
runCommandsWithAim cmds pos = foldl updatePositionWithAim pos cmds

day2 :: IO ()
day2 = do
  contents <- readFile "data/day2_input.txt"
  let cmds = mkCommands contents

  let Position {..} = runCommandsWithoutAim cmds (Position 0 0)
  putStr "Part1: "
  print (coerce horizontal * coerce depth :: Int)

  let PositionWithAim {..} = runCommandsWithAim cmds (PositionWithAim 0 0 0)
  putStr "Part2: "
  print (coerce horizontal * coerce depth :: Int)