{-# LANGUAGE RankNTypes #-}
module Advent.Y2020.Day8.Parse where

import Data.Attoparsec.Text

import Advent.Y2020.Day8.Asm

noopP :: Parser Instr
noopP = do
  string "nop"
  skip (== ' ')
  n <- signed decimal <* endOfLine
  pure $ (Nop, n)

jmpP :: Parser Instr
jmpP = do
  string "jmp"
  skip (== ' ')
  n <- signed decimal <* endOfLine
  pure $ (Jmp, n)

accP :: Parser Instr
accP = do
  string "acc"
  skip (== ' ')
  n <- signed decimal <* endOfLine
  pure $ (Acc, n)

opcodeP :: Parser Instr
opcodeP = choice [noopP, jmpP, accP]

parseInput :: Text -> Either String [Instr]
parseInput = parseOnly $ many' opcodeP
