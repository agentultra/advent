module Advent.Y2021.Day4.Parse where

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Advent.Y2021.Day4.Bingo

picksP :: Parser [Int]
picksP = A.sepBy1' A.decimal (A.char ',') <* A.endOfLine

rowP :: Parser [Cell]
rowP = do
  A.skipMany (A.char ' ')
  c1 <- cell <$> A.decimal
  A.skipMany1 (A.char ' ')
  c2 <- cell <$> A.decimal
  A.skipMany1 (A.char ' ')
  c3 <- cell <$> A.decimal
  A.skipMany1 (A.char ' ')
  c4 <- cell <$> A.decimal
  A.skipMany1 (A.char ' ')
  c5 <- cell <$> A.decimal
  pure [c1, c2, c3, c4, c5]

boardP :: Parser Board
boardP = do
  r1 <- rowP <* A.endOfLine
  r2 <- rowP <* A.endOfLine
  r3 <- rowP <* A.endOfLine
  r4 <- rowP <* A.endOfLine
  r5 <- rowP <* A.endOfLine
  case mkBoard [r1, r2, r3, r4, r5] of
    Nothing -> fail "Unable to parse board!"
    Just board -> pure board

parseInput :: Parser ([Int], [Board])
parseInput = do
  picks <- picksP
  A.skip A.isEndOfLine
  boards <- boardP `A.sepBy'` A.endOfLine
  pure (picks, boards)
