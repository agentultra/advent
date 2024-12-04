{-# LANGUAGE OverloadedStrings #-}

module Advent.Y2024.Day3.Input where

import Advent.Y2024.Day3.Instruction
import Data.Attoparsec.Text
import Replace.Attoparsec.Text

getDay1Input :: Text -> Either String [Instruction]
getDay1Input = parseOnly parseMulsOnly

getDay2Input :: Text -> Either String [Instruction]
getDay2Input = parseOnly parseRest

parseMul :: Parser Instruction
parseMul = do
  string "mul("
  x <- decimal
  char ','
  y <- decimal
  char ')'
  pure $ Mul x y

parseDo :: Parser Instruction
parseDo = string "do()" >> pure Do

parseDont :: Parser Instruction
parseDont = string "don't()" >> pure Dont

parseMulsOnly :: Parser [Instruction]
parseMulsOnly = rights <$> sepCap parseMul

parseRest :: Parser [Instruction]
parseRest = rights <$> sepCap (parseMul <|> parseDo <|> parseDont)
