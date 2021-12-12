module Advent.Y2021.Day8.Part1 where

import qualified Data.Text.IO as T

import Advent.Y2021.Day8.Digits
import Advent.Y2021.Day8.Parse

solution :: IO ()
solution = do
  raw <- T.readFile "data/2021/Day8.txt"
  case parseInput raw of
    Left err -> print err
    Right ds -> print $ part1Solution $ fromList ds
