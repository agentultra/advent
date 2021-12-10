module Advent.Y2021.Day7.Parse where

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A

parseInput :: Text -> Either String [Int]
parseInput = A.parseOnly crabP

crabP :: Parser [Int]
crabP = A.decimal `A.sepBy` A.char ','
