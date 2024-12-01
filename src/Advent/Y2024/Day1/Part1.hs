module Advent.Y2024.Day1.Part1 where

import Advent.Y2024.Day1.Input
import qualified Data.Text.IO as T

solution :: IO ()
solution = do
  raw <- T.readFile "data/2024/Day1.txt"
  let ns = getInput raw
      answer
        = foldl' (\a ps -> a + (uncurry max ps - uncurry min ps)) 0
        . uncurry zip
        . bimap sort sort
        . unzip <$> ns
  print answer
