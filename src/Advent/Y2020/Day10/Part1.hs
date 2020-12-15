module Advent.Y2020.Day10.Part1 where

import qualified Data.List as L

import Advent.Input

chain :: [Int] -> Int
chain xs =
  let xs' = diffs $ 0 : sort xs ++ [L.maximum xs + 3]
  in sumOnes xs' * sumThrees xs'
  where
    sumOnes = foldl' (\c x -> if x == 1 then c + 1 else c) 0
    sumThrees = foldl' (\c x -> if x == 3 then c + 1 else c) 0

diffs :: [Int] -> [Int]
diffs [] = []
diffs [_] = []
diffs (x:y:xs) = y - x : diffs (y:xs)

solution :: IO ()
solution = do
  input <- readInput "data/2020/Day10.txt"
  case traverse readInt input of
    Left err   -> putStrLn err
    Right nums -> print $ chain nums
