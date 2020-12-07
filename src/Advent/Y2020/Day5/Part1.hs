module Advent.Y2020.Day5.Part1 where

import qualified Data.List as L

import Advent.Input
import Advent.Y2020.Day5.SeatCode

solution :: IO ()
solution = do
  lines <- readInput "data/2020/Day5.txt"
  case traverse (findSeatId <=< seatCodeFromText) lines of
    Nothing    -> putStrLn "Nope"
    Just codes -> print . L.maximum $ codes
