module Lib
  ( getFileLines,
  )
where

getFileLines :: FilePath -> IO [String]
getFileLines fpath = do
  contents <- readFile fpath
  return $ lines contents