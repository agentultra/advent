module Advent.Y2022.Day1.Part2 where

import qualified Data.Text.IO as T

import Advent.Y2022.Day1.Elves

solution :: IO ()
solution = do
  raw <- T.readFile "data/2022/Day1.txt"
  let s = sum . take 3 . maxCalories . readCalories $ raw
  print s
