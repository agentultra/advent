module Advent.Y2024.Day9.Part2 where

import Advent.Y2024.Day9.File
import Advent.Y2024.Day9.Input
import qualified Data.Text.IO as T
import qualified Data.Vector as V

solution :: IO ()
solution = do
  raw <- T.readFile "data/2024/Day9.txt"
  let (blocks, freeMap, fileList) = fromRight (error "Invalid input") $ getInput raw
  print . checksum . compact (block freeMap fileList) $ blocks
