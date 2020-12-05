module Advent.Y2020.Day4.Parse where

import Control.Monad
import Data.Attoparsec.Text
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Advent.Y2020.Day4.Passport

passportKeyP :: Parser PassportKey
passportKeyP = do
  key <- string "byr"
    <|> string "iyr"
    <|> string "eyr"
    <|> string "hgt"
    <|> string "hcl"
    <|> string "ecl"
    <|> string "pid"
    <|> string "cid"
  case keyFromText key of
    Just key' -> pure key'
    Nothing -> fail $ "Could not parse passport key: " ++ T.unpack key

eolOrSpace :: Parser Char
eolOrSpace = char '\n' <|> char ' '

keyValueP :: Parser (PassportKey, Text)
keyValueP = do
  key <- passportKeyP
  skip (== ':')
  val <- takeWhile1 (not . isSpace) <* eolOrSpace
  pure (key, val)

passportP :: Parser (Map PassportKey Text)
passportP = do
  --kv <- keyValueP `sepBy'` eolOrSpace
  kv <- many1' keyValueP <* endOfLine
  pure $ M.fromList kv

parsePassports :: Text -> Either String [Map PassportKey Text]
parsePassports = parseOnly $ many' passportP
