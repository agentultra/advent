module Advent.Y2020.Day3.Forest where

import Data.List ((!!))

-- idx
-- -----------------
-- 0   | ...###.#.##
-- 1   | ....##.#..#
-- 2   | ..#  ##.#.#

countTrees :: (Int, Int) -> [String] -> Int
countTrees (dx, dy) =
   length . filter (uncurry treesInSlope) . zip [0..]
   where
    treesInSlope idx row = idx `mod` dy == 0 &&
      (cycle row !! ((idx `div` dy) * dx)) == '#'
