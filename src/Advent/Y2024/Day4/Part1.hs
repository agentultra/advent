module Advent.Y2024.Day4.Part1 where

import Advent.Grid (Grid)
import qualified Advent.Grid as Grid
import Advent.Input
import Advent.Y2024.Day4.WordSearch
import Data.Maybe
import qualified Data.Text as T

xmas :: String
xmas = "XMAS"

findXmas :: (Int, Int) -> Grid Char -> Int
findXmas ix grid
  = north ix grid
  + northE ix grid
  + east ix grid
  + southE ix grid
  + south ix grid
  + southW ix grid
  + west ix grid
  + northW ix grid

north :: (Int, Int) -> Grid Char -> Int
north (x, y) grid
  = if catMaybes [ Grid.get grid x (y-i) | i <- [0..3] ] == xmas
    then 1
    else 0

northE :: (Int, Int) -> Grid Char -> Int
northE (x, y) grid
  = if catMaybes [ Grid.get grid (x+i) (y-i) | i <- [0..3] ] == xmas
    then 1
    else 0

east :: (Int, Int) -> Grid Char -> Int
east (x, y) grid
  = if catMaybes [ Grid.get grid (x+i) y | i <- [0..3] ] == xmas
    then 1
    else 0

southE :: (Int, Int) -> Grid Char -> Int
southE (x, y) grid
  = if catMaybes [ Grid.get grid (x+i) (y+i) | i <- [0..3] ] == xmas
    then 1
    else 0

south :: (Int, Int) -> Grid Char -> Int
south (x, y) grid
  = if catMaybes [ Grid.get grid x (y+i) | i <- [0..3] ] == xmas
    then 1
    else 0

southW :: (Int, Int) -> Grid Char -> Int
southW (x, y) grid
  = if catMaybes [ Grid.get grid (x-i) (y+i) | i <- [0..3] ] == xmas
    then 1
    else 0

west :: (Int, Int) -> Grid Char -> Int
west (x, y) grid
  = if catMaybes [ Grid.get grid (x-i) y | i <- [0..3] ] == xmas
    then 1
    else 0

northW :: (Int, Int) -> Grid Char -> Int
northW (x, y) grid
  = if catMaybes [ Grid.get grid (x-i) (y-i) | i <- [0..3] ] == xmas
    then 1
    else 0

solution :: IO ()
solution = do
  raw <- readInput "data/2024/Day4.txt"
  let grid = fromJust . Grid.mkGrid . map T.unpack $ raw
  print $ hits (== 'X') findXmas grid
