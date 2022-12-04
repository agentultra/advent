module Advent.Y2022.Day4.Part1 where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Advent.Y2022.Day4.Interval

solution :: IO ()
solution = do
  raw <- T.readFile "data/2022/Day4.txt"
  let s = foldl' countContained 0 . map parseIntervals . T.lines $ raw
  print s
  where
    countContained :: Int -> (Interval, Interval) -> Int
    countContained c (intervalA, intervalB)
      | intervalA `contains` intervalB || intervalB `contains` intervalA = c + 1
      | otherwise = c
