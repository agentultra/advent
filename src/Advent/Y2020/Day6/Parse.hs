module Advent.Y2020.Day6.Parse where

import Data.Attoparsec.Text
import Data.Char
import qualified Data.Set as S
import qualified Data.Text as T

import Advent.Y2020.Day6.DeclarationForm

answerP :: Parser Answer
answerP = do
  ans <- takeWhile1 (not . isSpace) <* endOfLine
  pure $ S.fromList $ T.unpack ans

groupAnswersP :: Parser Group
groupAnswersP = do
  answers <- many1' answerP
  case nonEmpty answers of
    Nothing       -> fail "Failed parsing group: missing answers"
    Just answers' -> pure $ Group answers'

parseInput :: Text -> Either String [Group]
parseInput = parseOnly (groupAnswersP `sepBy` endOfLine)
