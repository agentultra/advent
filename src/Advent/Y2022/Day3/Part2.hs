module Advent.Y2022.Day3.Part2 where

import Data.Char
import Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T

solution :: IO ()
solution = do
  raw <- T.readFile "data/2022/Day3.txt"
  let s = L.sum . map (convert . L.head . nub . \[x, y, z] -> x `intersect` (y `intersect` z)) . chunks 3 . map T.unpack . T.lines $ raw
  print s
  where
    convert :: Char -> Int
    convert c
      | isLower c = ord c - 96
      | isUpper c = ord c - 38
      | otherwise = error "Invalid input"
