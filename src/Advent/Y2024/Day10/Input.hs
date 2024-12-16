module Advent.Y2024.Day10.Input where

import qualified Advent.Grid as Grid
import Advent.Y2024.Day10.Trail
import Control.Error
import Data.Char
import qualified Data.Text as T

getInput :: Text -> Either String TrailMap
getInput raw = do
  let rows = map T.unpack $ T.lines raw
  charGrid <- note "getInput: invalid grid" $ Grid.mkGrid rows
  pure . TrailMap $ (digitToInt <$> charGrid)
