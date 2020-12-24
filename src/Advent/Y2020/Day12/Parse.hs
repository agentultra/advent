module Advent.Y2020.Day12.Parse where

import Data.Attoparsec.Text
import GHC.Float

import Advent.Y2020.Day12.Ship

cardinalP :: Parser Cardinal
cardinalP = do
  c <- char 'N' <|> char 'E' <|> char 'S' <|> char 'W'
  case c of
    'N' -> pure North
    'E' -> pure East
    'S' -> pure South
    'W' -> pure West

relativeP :: Parser Relative
relativeP = do
  c <- char 'L' <|> char 'R'
  case c of
    'L' -> pure Left'
    'R' -> pure Right'

translationP :: Parser Translation
translationP = do
  c <- char 'F'
  case c of
    'F' -> pure Forward

goCommandP :: Parser (Instruction a b)
goCommandP = do
  cmd <- translationP
  Go cmd <$> decimal

turnCommandP :: Parser (Instruction a b)
turnCommandP = do
  cmd <- relativeP
  Turn cmd . degree . double2Float <$> double

moveCommandP :: Parser (Instruction a b)
moveCommandP = do
  cmd <- cardinalP
  Move cmd <$> decimal

instructionP :: Parser (Instruction a b)
instructionP = moveCommandP <|> turnCommandP <|> goCommandP

parseInput :: Text -> Either String [Instruction a b]
parseInput = parseOnly $ instructionP `sepBy` endOfLine
