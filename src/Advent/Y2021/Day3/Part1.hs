module Advent.Y2021.Day3.Part1 where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Advent.Y2021.Day3.Diagnostic

solution :: IO ()
solution = do
  ns <- T.lines <$> T.readFile "data/2021/Day3.txt"
  print $ part1Solution $ fromList ns
