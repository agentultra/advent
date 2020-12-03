module Advent.Y2020.Day2.Part2 where

import qualified Data.Text.IO as T

import Advent.Y2020.Day2.Parse
import Advent.Y2020.Day2.Policy
import Advent.Input
import Advent.Text.Utils

solution :: IO ()
solution = do
  raw <- T.readFile "data/2020/Day2.txt"
  case parseInput raw of
    Right checks -> print $ checkMany checkExclusiveMatch checks
    Left err -> putStrLn err
