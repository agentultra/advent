module Advent.Y2020.Day1.Part2 where

import Data.Text.Read as T

import Advent.Input

solution :: IO ()
solution = do
  input <- readInput "./data/2020/Day1.txt"
  let (Right nums) = map fst <$> traverse T.decimal input
  let result = take 1 $ [ (x, y, z) | x <- nums, y <-nums, z <- nums, x + y + z == 2020]
  case listToMaybe result of
    Just (x, y, z) -> print $ x * y * z
    Nothing -> putStrLn "Nope"
