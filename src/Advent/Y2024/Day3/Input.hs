{-# LANGUAGE OverloadedStrings #-}

module Advent.Y2024.Day3.Input where

import Advent.Y2024.Day3.Instruction
import Data.Attoparsec.Text
import Replace.Attoparsec.Text

getInput :: Text -> Either String [Instruction]
getInput = parseOnly parseInput

parseMul :: Parser Instruction
parseMul = do
  string "mul("
  x <- decimal
  char ','
  y <- decimal
  char ')'
  pure $ Mul x y

parseInput :: Parser [Instruction]
parseInput = rights <$> sepCap parseMul
