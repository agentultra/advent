module Advent.Y2021.Day4.Part2 where

import qualified Data.Attoparsec.Text as A
import qualified Data.Text.IO as T

import Advent.Y2021.Day4.Bingo
import Advent.Y2021.Day4.Parse

solution :: IO ()
solution = do
  raw <- T.readFile "data/2021/Day4.txt"
  case A.parseOnly parseInput raw of
    Left err -> print err
    Right (picks, boards) ->
      print $ part2Solution picks boards
