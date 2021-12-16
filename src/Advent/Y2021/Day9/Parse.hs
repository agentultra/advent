module Advent.Y2021.Day9.Parse where

import qualified Data.Text as T
import qualified Data.Text.Read as T

-- Quick and dirty
parseInput :: Text -> [[Int]]
parseInput = map parseRow . T.lines
  where
    parseRow :: Text -> [Int]
    parseRow tRow = case mapM T.decimal . T.chunksOf 1 $ tRow of
      Left err -> error $ T.pack err
      Right ns -> map fst ns
