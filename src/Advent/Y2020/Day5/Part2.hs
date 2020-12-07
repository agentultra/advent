module Advent.Y2020.Day5.Part2 where

import qualified Data.List as L
import Advent.Input
import Advent.Y2020.Day5.SeatCode

findSeat :: [Int] -> Maybe Int
findSeat seatIds
  = listToMaybe [ a + 1 | (a, b) <- consecutivePairs seatIds, b - a == 2]
  where
    consecutivePairs = (zip <*> L.tail) . sort

solution :: IO ()
solution = do
  lines <- readInput "data/2020/Day5.txt"
  case traverse (findSeatId <=< seatCodeFromText) lines of
    Nothing -> putStrLn "Nope"
    Just codes -> print . findSeat $ codes
