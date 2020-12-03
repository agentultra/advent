module Advent.Y2020.Day2.Part1 where

import Advent.Y2020.Day2.Parse
import Advent.Y2020.Day2.Policy
import Advent.Input
import Advent.Text.Utils

solution :: IO ()
solution = do
  lines <- readInput "data/2020/Day2.txt"
  case traverse parseLine . map addNewLine $ lines of
    Right checks -> print $ checkMany check checks
    Left err -> putStrLn err
