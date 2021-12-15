module Advent.Y2021.Day8.Parse where

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.Set as S

import Advent.Parse.Utils
import Advent.Y2021.Day8.Digits

parseInput :: Text -> Either String [Display]
parseInput = parseInputFile displayP

segmentP :: Parser Segment
segmentP = do
  c <- A.satisfy (A.inClass "abcdefg")
  case c of
    'a' -> pure A
    'b' -> pure B
    'c' -> pure C
    'd' -> pure D
    'e' -> pure E
    'f' -> pure F
    'g' -> pure G
    _   -> fail "Invalid segment"

digitP :: Parser Digit
digitP = do
  ss <- S.fromList <$> A.many1' segmentP
  pure $ Digit ss

displayP :: Parser Display
displayP = do
  patterns <- digitP `A.sepBy'` A.space <* A.string " | "
  d1 <- digitP <* A.space
  d2 <- digitP <* A.space
  d3 <- digitP <* A.space
  d4 <- digitP
  pure $ Display patterns [d1, d2, d3, d4]
