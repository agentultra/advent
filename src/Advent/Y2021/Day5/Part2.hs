module Advent.Y2021.Day5.Part2 where

import qualified Data.Text.IO as T

import Advent.Y2021.Day5.Hydro
import Advent.Y2021.Day5.Parse

solution :: IO ()
solution = do
  raw <- T.readFile "data/2021/Day5.txt"
  case parseInput raw of
    Left err -> print err
    Right ls ->
      print $ part2Solution . fromList $ ls
