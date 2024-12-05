module Advent.Y2024.Day5.Input where

import Advent.Y2024.Day5.Rule
import Advent.Y2024.Day5.Update
import Data.Attoparsec.Text
import qualified Data.List.NonEmpty as NE

getInput :: Text -> Either String (NonEmpty Rule, [Update])
getInput = parseOnly parseInput

parseRule :: Parser Rule
parseRule = do
  x <- decimal
  char '|'
  y <- decimal
  pure $ Rule (x, y)

parseUpdate :: Parser Update
parseUpdate = do
  ns <- sepBy1' decimal $ char ','
  pure . Update $ NE.fromList ns

parseRuleSection :: Parser (NonEmpty Rule)
parseRuleSection = NE.fromList <$> sepBy1' parseRule endOfLine

parseUpdateSection :: Parser [Update]
parseUpdateSection = sepBy1' parseUpdate endOfLine

parseInput :: Parser (NonEmpty Rule, [Update])
parseInput = do
  rules <- parseRuleSection
  skipSpace
  updates <- parseUpdateSection
  pure (rules, updates)
