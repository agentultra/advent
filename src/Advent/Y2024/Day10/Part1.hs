module Advent.Y2024.Day10.Part1 where

import Advent.Y2024.Day10.Input
import Advent.Y2024.Day10.Trail
import qualified Data.List as List
import qualified Data.Text.IO as T

answer :: TrailMap -> Int
answer trailMap
  = sum
  . map (findTrails List.nub trailMap)
  $ getTrailheads trailMap

solution :: IO ()
solution = do
  raw <- T.readFile "data/2024/Day10.txt"
  let trailMap = fromRight (error "Invalid input") $ getInput raw
  print $ answer trailMap
