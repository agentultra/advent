module Advent.Y2020.Day13.Part1 where

import qualified Data.Text.IO as T

import Advent.Y2020.Day13.Bus
import Advent.Y2020.Day13.Parse

solution :: IO ()
solution = do
  input <- T.readFile "data/2020/Day13.txt"
  case parseInput input of
    Left err -> putStrLn err
    Right result -> print
      $ uncurry (part1Solution (fst result)) . uncurry soonestBusFromTime result
