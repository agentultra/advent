module Advent.Y2020.Day7.Part2 where

import qualified Data.HashMap.Strict as H
import qualified Data.Text.IO as T

import Advent.Y2020.Day7.Parse
import Advent.Y2020.Day7.Bag

costOfBags :: Text -> HashMap Bag Contains -> Int
costOfBags bag tree = sum $ map countBags $ H.lookupDefault [] bag tree
  where
    countBags (num, b) = num * (1 + costOfBags b tree)

solution :: IO ()
solution = do
  input <- T.readFile "data/2020/Day7.txt"
  case parseInput input of
    Left err       -> putStrLn err
    Right bagNodes -> print $ costOfBags "shiny gold" $ H.fromList bagNodes
