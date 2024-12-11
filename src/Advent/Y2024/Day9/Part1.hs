module Advent.Y2024.Day9.Part1 where

import Advent.Y2024.Day9.File
import Advent.Y2024.Day9.Input
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Prelude hiding (swap)

solution :: IO ()
solution = do
  raw <- T.readFile "data/2024/Day9.txt"
  let (blocks, _) = fromRight (error "Invalid input") $ getInput raw
  print . checksum . compact swap $ blocks
