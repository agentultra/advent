module Advent.Y2024.Day11.Input where

import Advent.Input
import Data.Attoparsec.Text
import qualified Data.Text as T

getInput :: Text -> IO [String]
getInput filename = do
  raw <- readInput filename
  case listToMaybe raw of
    Nothing -> error "Missing input"
    Just x  -> pure . fromRight (error "Invalid input") $ parseOnly parseNums x

-- | Each string is comprised of digits
parseNums :: Parser [String]
parseNums = sepBy1' (many1' digit) space
