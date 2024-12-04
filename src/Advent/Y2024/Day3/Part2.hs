module Advent.Y2024.Day3.Part2 where

import Advent.Y2024.Day3.Input
import Advent.Y2024.Day3.Instruction
import qualified Data.Text.IO as T

solution :: IO ()
solution = do
  raw <- T.readFile "data/2024/Day3.txt"
  let instrs = getDay2Input raw
      answer = eval <$> instrs
  print answer
