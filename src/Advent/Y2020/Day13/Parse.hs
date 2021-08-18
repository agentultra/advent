module Advent.Y2020.Day13.Parse where

import Data.Bifunctor (bimap)
import Data.Either
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T

import Advent.Y2020.Day13.Bus

parseInput :: Text -> Either String (Int, NonEmpty Bus)
parseInput = parseRest . headToTuple . T.lines
  where
    headToTuple :: [a] -> (a, a)
    headToTuple [] = error "Our assumptions are broken"
    headToTuple [_] = error "Our assumptions are broken"
    headToTuple (x:y:_) = (x, y)

    parseRest :: (Text, Text) -> Either String (Int, NonEmpty Bus)
    parseRest (startText, busText) = do
      start <- T.decimal @Int $ startText
      let buses = rights . map (T.decimal @Int) . map xBusses . T.splitOn "," $ busText
      pure (fst start, N.fromList $ map (Bus . fst) buses)

    -- Interpret `x` as `-1` so that we parse those busses and
    -- preserve the indices of the puzzle input which is used in Part2
    -- to figure out the offsets from t.
    xBusses :: Text -> Text
    xBusses "x" = "-1"
    xBusses x   = x
