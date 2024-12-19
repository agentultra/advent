{-# LANGUAGE OverloadedStrings #-}

module Advent.Y2024.Day12.Part1 where

import Advent.Grid (Grid)
import Advent.Y2024.Day12.Garden
import Advent.Y2024.Day12.Input
import qualified Data.Text.IO as T

answer :: Grid Char -> Int
answer g = evalState search $ RegionSearch (mkVisited g) g length id 0

solution :: IO ()
solution = do
  raw <- T.readFile "data/2024/Day12.txt"
  let grid = fromRight (error "Invalid Input") $ getInput raw
  print $ answer grid
