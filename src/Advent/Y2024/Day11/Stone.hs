module Advent.Y2024.Day11.Stone where

import Control.Monad.State.Strict
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Text.Read (read)

blink :: [String] -> [String]
blink = reverse . List.foldl' step []
  where
    step :: [String] -> String -> [String]
    step acc x
      | x == "0" = "1" : acc
      | even $ length x =
        let (l, r) = List.splitAt (length x `div` 2) x
        in show (read @Int r) : show (read @Int l) : acc
      | otherwise = show (read x * 2024) : acc

blinkStones :: Int -> [String] -> Int
blinkStones blinks stones = evalState doBlink (blinks, stones)

doBlink :: State (Int, [String]) Int
doBlink = do
  (count, stones) <- get
  if count == 0
    then pure $ length stones
    else put (count - 1, blink stones) >> doBlink
