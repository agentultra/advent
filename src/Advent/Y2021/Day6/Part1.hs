module Advent.Y2021.Day6.Part1 where

import qualified Data.Text.IO as T

import Advent.Y2021.Day6.Fish
import Advent.Y2021.Day6.Parse

solution :: IO ()
solution = do
  raw <- T.readFile "data/2021/Day6.txt"
  case parseInput raw of
    Left err -> print err
    Right fs -> print $ part1Solution fs
