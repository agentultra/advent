module Advent.Y2021.Day7.Part2 where

import qualified Data.Text.IO as T

import Advent.Y2021.Day7.Crab
import Advent.Y2021.Day7.Parse

solution :: IO ()
solution = do
  raw <- T.readFile "data/2021/Day7.txt"
  case parseInput raw of
    Left err    -> print err
    Right crabs -> print $ part2Solution $ fromList crabs
