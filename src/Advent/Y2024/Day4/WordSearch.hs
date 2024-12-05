module Advent.Y2024.Day4.WordSearch where

import Advent.Grid (Grid)
import qualified Advent.Grid as Grid
import Data.Maybe

hits
  :: (Char -> Bool)
  -> ((Int, Int) -> Grid Char -> Int)
  -> Grid Char
  -> Int
hits target search grid
  = let counts = [ search (x, y) grid
                 | (x, y) <- Grid.indices grid
                 , target . fromJust $ Grid.get grid x y
                 ]
    in sum counts
