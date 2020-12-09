module Advent.Y2020.Day7.Parse where

import Data.Attoparsec.Text
import qualified Data.Text as T

import Advent.Y2020.Day7.Bag

bagP :: Parser Bag
bagP = T.strip . T.pack <$> manyTill' anyChar (string "bags" <|> string "bag")

countBagP :: Parser (Int, Bag)
countBagP = do
  c <- decimal
  skip (== ' ')
  b <- bagP
  pure (c, b)

containsP :: Parser Contains
containsP = string "no other bags." *> pure []
  <|> countBagP `sepBy'` (string ", ") <* char '.'

bagNodeP :: Parser BagNode
bagNodeP = do
  b <- bagP
  string " contain "
  bs <- containsP
  pure (b, bs)

parseInput :: Text -> Either String [BagNode]
parseInput = parseOnly $ bagNodeP `sepBy` char '\n'
