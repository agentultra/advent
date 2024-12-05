module Advent.Y2024.Day4.Part2 where

import Advent.Grid (Grid)
import qualified Advent.Grid as Grid
import Advent.Input
import Advent.Y2024.Day4.WordSearch
import Data.Maybe
import qualified Data.Text as T

targets :: [String]
targets = ["MMSS", "SSMM", "MSMS", "SMSM"]

findXmas :: (Int, Int) -> Grid Char -> Int
findXmas (x, y) grid =
  let xs = catMaybes
           [ Grid.get grid (x+i) (y+j)
           | i <- [(-1), 1]
           , j <- [(-1), 1]
           ]
  in if xs `elem` targets then 1 else 0

solution :: IO ()
solution = do
  raw <- readInput "data/2024/Day4.txt"
  let grid = fromJust . Grid.mkGrid . map T.unpack $ raw
  print $ hits (== 'A') findXmas grid
