module Advent.Y2024.Day2.Part2 where

import qualified Data.Text.IO as T
import Advent.Y2024.Day2.Input
import Advent.Y2024.Day2.Report

solution :: IO ()
solution = do
  raw <- T.readFile "data/2024/Day2.txt"
  let rs = getInput raw
      answer = length . filter isSafe . map dampenResult <$> rs
  print answer
