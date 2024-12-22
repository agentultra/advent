{-# LANGUAGE OverloadedStrings #-}

module Advent.Y2024.Day12.Part2 where

import Advent.Grid (Grid)
import Advent.Y2024.Day12.Garden
import Advent.Y2024.Day12.Input
import qualified Data.Text.IO as T

answer :: Grid Char -> Int
answer g = evalState search $ RegionSearch (mkVisited g) g cardinal interiorAngle sides 0

cardinal :: (Int, Int) -> Grid Char -> [Maybe ((Int, Int), Char)]
cardinal c g = [ (c .+. Grid.offset d,) <$> Grid.getAt g (c .+. Grid.offset d) | d <- Grid.cardinal ]

interiorAngle :: [Maybe ((Int, Int), Char)] -> Int
interiorAngle xs
  | length xs == 2 = 90
  | length xs == 3 = 180
  | length xs == 4 = 360
  | otherwise      = 0

sides :: Int -> Int
sides n = (n `div` 180) + 2

solution :: IO ()
solution = do
  raw <- T.readFile "data/2024/Day12.txt"
  let grid = fromRight (error "Invalid Input") $ getInput raw
  print $ answer grid
