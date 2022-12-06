module Advent.Y2022.Day6.Part1 where

import qualified Data.Text.IO as T

import Advent.Y2022.Day6.Signal

solution :: IO ()
solution = do
  raw <- T.readFile "data/2022/Day6.txt"
  case findPacketMarkerIndex 4 raw of
    Nothing -> putStrLn "No solution found!"
    Just ix -> print ix
