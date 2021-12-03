module Advent.Y2021.Day2.Part2 where

import qualified Data.Text.IO as T

import Advent.Y2021.Day2.Parse
import Advent.Y2021.Day2.Sub

solution :: IO ()
solution = do
  raw <- T.readFile "data/2021/Day2.txt"
  case parseInput raw of
    Left err -> print err
    Right cmds -> print $ part2Solution . fromList $ cmds
