module Advent.Y2021.Day5.Parse where

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A

import Advent.Parse.Utils

import Advent.Y2021.Day5.Hydro

parseInput :: Text -> Either String [Line]
parseInput = parseInputFile lineP

lineP :: Parser Line
lineP = do
  x1 <- A.decimal
  A.skip (== ',')
  y1 <- A.decimal <* A.string " -> "
  x2 <- A.decimal
  A.skip (== ',')
  y2 <- A.decimal
  pure $ Line x1 y1 x2 y2
