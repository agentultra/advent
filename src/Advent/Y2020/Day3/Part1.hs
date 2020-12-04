module Advent.Y2020.Day3.Part1 where

import qualified Data.Text as T

import Advent.Input
import Advent.Y2020.Day3.Forest

solution :: IO ()
solution = do
  lines <- readInput "data/2020/Day3.txt"
  let forest = countTrees (3, 1) (map T.unpack lines)
  print forest
