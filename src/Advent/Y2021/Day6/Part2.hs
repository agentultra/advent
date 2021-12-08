module Advent.Y2021.Day6.Part2 where

import Data.Text.IO as T

import Advent.Y2021.Day6.Fish
import Advent.Y2021.Day6.Parse

solution :: IO ()
solution = do
  raw <- T.readFile "data/2021/Day6.txt"
  case parseInput raw of
    Left err -> print err
    Right fishes -> print $ part2Solution fishes
