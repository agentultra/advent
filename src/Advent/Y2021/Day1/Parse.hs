module Advent.Y2021.Day1.Parse where

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A

import Advent.Parse.Utils
import Advent.Y2021.Day1.Sonar

parseInput :: Text -> Either String [Reading]
parseInput = parseInputFile parseReading

parseReading :: Parser Reading
parseReading = do
  n <- A.decimal
  pure . Reading $ n
