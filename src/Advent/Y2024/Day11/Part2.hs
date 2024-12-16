module Advent.Y2024.Day11.Part2 where

import Advent.Y2024.Day11.Input
import Advent.Y2024.Day11.Stone

solution :: IO ()
solution = do
  ns <- getInput "data/2024/Day11.txt"
  print $ blinkStones 75 ns
