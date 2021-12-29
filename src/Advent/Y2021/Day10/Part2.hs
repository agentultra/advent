module Advent.Y2021.Day10.Part2 where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Advent.Y2021.Day10.Chunk

solution :: IO ()
solution = do
  input <- T.lines <$> T.readFile "data/2021/Day10.txt"
  print $ part2Solution $ fromList input
