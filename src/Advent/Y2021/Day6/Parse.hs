module Advent.Y2021.Day6.Parse where

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Vector (Vector)
import qualified Data.Vector as V

parseInput :: Text -> Either String (Vector Int)
parseInput = A.parseOnly fishP

fishP :: Parser (Vector Int)
fishP = do
  fs <- A.decimal `A.sepBy` A.char ','
  pure $ V.fromList fs
