module Advent.Y2020.Day1.Part1 where

import Data.Text.Read as T

import Advent.Input

combinations2 :: [a] -> [(a, a)]
combinations2 xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

solution :: IO ()
solution = do
  input <- readInput "./data/2020/Day1.txt"
  let (Right nums) = map fst <$> traverse T.decimal input
  let result = take 1 $ [ (x, y) | (x, y) <- combinations2 nums, x + y == 2020]
  case listToMaybe result of
    Just (x, y) -> print $ x * y
    Nothing     -> putStrLn "Nope"
