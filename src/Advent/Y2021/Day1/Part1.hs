module Advent.Y2021.Day1.Part1 where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.IO as T

import Advent.Y2021.Day1.Parse
import Advent.Y2021.Day1.Sonar

solution :: IO ()
solution = do
  raw <- T.readFile "data/2021/Day1.txt"
  case parseInput raw of
    Left err -> print err
    Right readings -> do
      let s = part1Solution $ NE.fromList readings
      print s
