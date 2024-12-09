module Advent.Y2024.Day7.Input where

import Advent.Y2024.Day7.Calibration
import Data.Attoparsec.Text
import qualified Data.List.NonEmpty as NE

getInput :: Text -> Either String [Calibration]
getInput raw = NE.toList <$> parseOnly parseInput raw

parseCalibration :: Parser Calibration
parseCalibration = do
  r <- decimal <* char ':'
  skipSpace
  ns <- sepBy1' decimal (char ' ')
  case NE.nonEmpty ns of
    Nothing   -> fail "parseCalibration: must have at least one number"
    Just nums -> pure $ Calibration r nums

parseInput :: Parser (NonEmpty Calibration)
parseInput = do
  cs <- sepBy1' parseCalibration endOfLine
  case NE.nonEmpty cs of
    Nothing -> fail "parseInput: expected at least one Calibration"
    Just calibrations -> pure calibrations
