module Advent.Y2024.Day6.Input where

import Advent.Grid (Grid)
import qualified Advent.Grid as Grid
import Advent.Input
import Advent.Y2024.Day6.Room
import Control.Error
import Data.Maybe
import qualified Data.Text as T

parseInput :: [Text] -> Either String (Grid Tile, (Int, Int))
parseInput input = do
  let raw = map T.unpack input
  charGrid <- note "getInput: invalid grid" $ Grid.mkGrid raw
  let guardPos
        = fromMaybe (error "Invalid Room: missing guard")
        $ Grid.findIndex (== '^') charGrid
  pure $ ((fromJust . fromChar) <$> charGrid, guardPos)
