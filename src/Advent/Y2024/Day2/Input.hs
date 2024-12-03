module Advent.Y2024.Day2.Input where

import Advent.Y2024.Day2.Report
import Data.Attoparsec.Text
import qualified Data.List.NonEmpty as NE

getInput :: Text -> Either String [Report]
getInput = parseOnly parseInput

parseReport :: Parser Report
parseReport = do
  nums <- decimal `sepBy1'` (char ' ')
  endOfLine
  case NE.nonEmpty nums of
    Nothing -> fail "parseReport: expected at least one decimal number"
    Just ns -> pure $ Report ns

parseInput :: Parser [Report]
parseInput = many1' parseReport
