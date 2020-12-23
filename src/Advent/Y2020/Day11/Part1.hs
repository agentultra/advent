module Advent.Y2020.Day11.Part1 where

import Advent.Input
import Advent.Y2020.Day11.Grid

solve :: Grid -> Int
solve grid =
  let grid' = step grid
  in if grid == grid'
     then countOfOccupied grid'
     else solve grid'

solution :: IO ()
solution = do
  input <- readInput "data/2020/Day11.txt"
  print $ solve $ parseGrid input
