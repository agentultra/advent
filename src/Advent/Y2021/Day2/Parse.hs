module Advent.Y2021.Day2.Parse where

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A

import Advent.Parse.Utils
import Advent.Y2021.Day2.Sub (Direction, Command (..))
import qualified Advent.Y2021.Day2.Sub as S

parseInput :: Text -> Either String [Command]
parseInput = parseInputFile commandP

directionP :: Parser Direction
directionP = do
  s <- A.string "forward" <|> A.string "up" <|> A.string "down"
  case s of
    "forward" -> pure S.Forward
    "up" -> pure S.Up
    "down" -> pure S.Down
    _ -> fail "Unknown"

amountP :: Parser Int
amountP = A.decimal

commandP :: Parser Command
commandP = do
  dir <- directionP
  A.skipSpace
  amt <- amountP
  pure $ Command dir amt
