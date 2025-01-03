{-# LANGUAGE OverloadedStrings #-}

module Advent.Y2024.Day12.Part2 where

import Advent.Grid (Direction (..), Grid, (.+.), (.-.))
import qualified Advent.Grid as Grid
import Advent.Y2024.Day12.Garden
import Advent.Y2024.Day12.Input
import qualified Data.Text.IO as T

answer :: Grid Char -> Int
answer g = evalState search $ RegionSearch (mkVisited g) g cardinal vertices sides 0

cardinal :: (Int, Int) -> Grid Char -> [((Int, Int), Maybe Char)]
cardinal c g = do
  d <- Grid.cardinal
  let offset = Grid.offset d
  pure (offset, Grid.getAt g offset)

vertices :: (Int, Int) -> [((Int, Int), Maybe Char)] -> [(Int, Int)]
vertices _ _ = []

getAngle :: [Maybe Direction] -> Int
getAngle ds = case sort ds of
  [Just North, Just NorthEast, Just East] -> 90
  [Nothing, Nothing, Just NorthEast] -> 270
  [Just East, Just SouthEast, Just South] -> 90
  [Nothing, Nothing, Just SouthEast] -> 270
  [Just South, Just SouthWest, Just West] -> 90
  [Nothing, Nothing, Just SouthWest] -> 270
  [Just West, Just NorthWest, Just North] -> 90
  [Nothing, Nothing, Just NorthWest] -> 270
  _ -> 0

corners :: [[Maybe Direction] -> [Maybe Direction]]
corners = [topRight, bottomRight, bottomLeft, topLeft]

topRight :: [Maybe Direction] -> [Maybe Direction]
topRight = filter (`elem` [Just North, Just NorthEast, Just East])

bottomRight :: [Maybe Direction] -> [Maybe Direction]
bottomRight = filter (`elem` [Just East, Just SouthEast, Just South])

bottomLeft :: [Maybe Direction] -> [Maybe Direction]
bottomLeft = filter (`elem` [Just South, Just SouthWest, Just West])

topLeft :: [Maybe Direction] -> [Maybe Direction]
topLeft = filter (`elem` [Just West, Just NorthWest, Just North])

sides :: [(Int, Int)] -> Int
sides = const 0

solution :: IO ()
solution = do
  raw <- T.readFile "data/2024/Day12.txt"
  let grid = fromRight (error "Invalid Input") $ getInput raw
  print $ answer grid
