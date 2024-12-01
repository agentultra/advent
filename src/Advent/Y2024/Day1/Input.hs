module Advent.Y2024.Day1.Input where

import Data.Attoparsec.Text

getInput :: Text -> Either String [(Int, Int)]
getInput = parseOnly parseInput

parseNums :: Parser (Int, Int)
parseNums = do
  x <- decimal
  skipSpace
  y <- decimal
  pure (x, y)

parseInput :: Parser [(Int, Int)]
parseInput = many1' (parseNums <* endOfLine)
