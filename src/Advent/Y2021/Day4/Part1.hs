module Advent.Y2021.Day4.Part1 where

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Advent.Y2021.Day4.Parse
import Advent.Y2021.Day4.Bingo

solution :: IO ()
solution = do
  raw <- T.readFile "data/2021/Day4.txt"
  case A.parseOnly parseInput raw of
    Left err -> print err
    Right (picks, boards) -> do
      let (found, result) = part1Solution picks boards
      if found
        then print result
        else print "No solution found"
