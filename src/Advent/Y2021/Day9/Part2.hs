module Advent.Y2021.Day9.Part2 where

import qualified Data.Text.IO as T

import Advent.Y2021.Day9.Parse
import Advent.Y2021.Day9.Vents

solution :: IO ()
solution = do
  raw <- parseInput <$> T.readFile "data/2021/Day9.txt"
  print $ part2Solution . fmap fromList . fromList $ raw
