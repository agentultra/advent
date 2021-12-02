module Advent.Y2021.Day1.Sonar where

import qualified Data.List.NonEmpty as NE

newtype Reading = Reading { getReading :: Int }
  deriving (Eq, Ord, Show)

data Delta
  = Increased Reading Reading
  | Decreased Reading Reading
  | NoChange Reading Reading
  | NoPrevious Reading
  deriving (Eq, Show)

adjacentDeltas :: NonEmpty Reading -> [Delta]
adjacentDeltas (x :| xs) =
  let xs' = x : xs
  in NoPrevious x : [ delta y y' | (y, y') <- zip xs' $ tail (NE.fromList xs')]

delta :: Reading -> Reading -> Delta
delta r1 r2 = case compare r1 r2 of
  LT -> Increased r1 r2
  EQ -> NoChange r1 r2
  GT -> Decreased r1 r2

part1Solution :: NonEmpty Reading -> Int
part1Solution = foldl' countIncreased 0 . adjacentDeltas
  where
    countIncreased :: Int -> Delta -> Int
    countIncreased count (Increased _ _ ) = count + 1
    countIncreased count _ = count
