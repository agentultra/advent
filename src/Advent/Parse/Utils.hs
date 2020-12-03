module Advent.Parse.Utils where

import Data.Attoparsec.Text

parseInputLines :: Parser a -> Parser [a]
parseInputLines p = many' $ p <* endOfLine

parseInputFile :: Parser a -> Text -> Either String [a]
parseInputFile p = parseOnly (parseInputLines p)
