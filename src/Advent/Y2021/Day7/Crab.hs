module Advent.Y2021.Day7.Crab where

import Data.List (maximum, minimum)

-- | Map the costs of moving @n@ to different indices up to @m@.
--
-- For example where @n = 2@ and @m = 7@:
-- Index: 0  1  2  3  4  5  6  7
-- Cost:  2  1  0  1  2  3  4  5
projectCost :: Int -> Int -> [Int]
projectCost m n = map abs [negate n .. m-n]

-- Like 'projectCost' except each step costs one more than the last.
--
-- For example where @n = 2@ and @m = 7@:
-- Index: 0  1  2  3  4  5  6  7
-- Cost:  3  1  0  1  3  6 10 15
projectCost' :: Int -> Int -> [Int]
projectCost' m = map collectSums . projectCost m
  where
    collectSums :: Int -> Int
    collectSums n = sum [1..n]

part1Solution :: NonEmpty Int -> Int
part1Solution ns =
  let m = maximum ns
  in minimum . map sum . transpose . fmap (projectCost m) . sort $ toList ns

part2Solution :: NonEmpty Int -> Int
part2Solution ns =
  let m = maximum ns
  in minimum . map sum . transpose . fmap (projectCost' m) . sort $ toList ns
