module Advent.Y2020.Day3.Part2 where

import qualified Data.Text as T

import Advent.Input
import Advent.Y2020.Day3.Forest

solution :: IO ()
solution = do
  lines <- readInput "data/2020/Day3.txt"
  let forest = map T.unpack lines
  print . product $ flip countTrees forest <$> [(1,1),(3,1),(5,1),(7,1),(1,2)]
