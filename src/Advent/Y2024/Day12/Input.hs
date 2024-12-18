module Advent.Y2024.Day12.Input where

import Advent.Grid (Grid)
import qualified Advent.Grid as Grid
import Advent.Y2024.Day10.Trail
import Control.Error
import Data.Char
import qualified Data.Text as T

getInput :: Text -> Either String (Grid Char)
getInput raw = do
  let rows = map T.unpack $ T.lines raw
  note "getInput: invalid grid" $ Grid.mkGrid rows
