module Advent.Y2024.Day3.Part1 where

import Advent.Y2024.Day3.Input
import Advent.Y2024.Day3.Instruction
import qualified Data.Text.IO as T

solution :: IO ()
solution = do
  raw <- T.readFile "data/2024/Day3.txt"
  let instrs = getInput raw
      answer = sum . map eval <$> instrs
  print answer
