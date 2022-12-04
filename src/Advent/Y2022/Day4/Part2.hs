module Advent.Y2022.Day4.Part2 where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Advent.Y2022.Day4.Interval

solution :: IO ()
solution = do
  raw <- T.readFile "data/2022/Day4.txt"
  let s = foldl' countOverlaps 0 . map parseIntervals . T.lines $ raw
  print s
  where
    countOverlaps :: Int -> (Interval, Interval) -> Int
    countOverlaps c (intervalA, intervalB)
      | intervalA `overlaps` intervalB = c + 1
      | otherwise = c
