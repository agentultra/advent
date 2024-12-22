{-# LANGUAGE OverloadedStrings #-}

module Advent.Y2024.Day12.Part1 where

import Advent.Grid (Grid, (.+.))
import qualified Advent.Grid as Grid
import Advent.Y2024.Day12.Garden
import Advent.Y2024.Day12.Input
import qualified Data.Text.IO as T

answer :: Grid Char -> Int
answer g
  = evalState search
  $ RegionSearch (mkVisited g) g ortho sumPerimeter getSum 0
  where
    sumPerimeter :: (Int, Int) -> [((Int, Int), Maybe Char)] -> Sum Int
    sumPerimeter _ xs = Sum $ length xs

ortho :: (Int, Int) -> Grid Char -> [((Int, Int), Maybe Char)]
ortho c g = do
  d <- Grid.orthogonal
  let offset = c .+. Grid.offset d
  pure $ (offset, Grid.getAt g offset)

solution :: IO ()
solution = do
  raw <- T.readFile "data/2024/Day12.txt"
  let grid = fromRight (error "Invalid Input") $ getInput raw
  print $ answer grid
