module Advent.Y2020.Day2.Parse where

import Data.Char
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

import Advent.Y2020.Day2.Policy

parseLine :: Text -> Either String (Policy Char, String)
parseLine = A.eitherResult . A.parse lineP

lineP :: Parser (Policy Char, String)
lineP = do
  p <- policyP
  A.skip (== ':')
  A.skip (== ' ')
  input <- A.takeWhile1 isAlpha
  A.endOfLine
  pure (p, T.unpack input)

policyP :: Parser (Policy Char)
policyP = do
  lo <- A.decimal
  A.skip (== '-')
  hi <- A.decimal
  A.skip (== ' ')
  Policy (lo, hi) <$> A.letter
