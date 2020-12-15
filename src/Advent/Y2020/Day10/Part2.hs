module Advent.Y2020.Day10.Part2 where

import qualified Data.HashMap.Strict as H
import qualified Data.List as L

import Advent.Input

chainCombinations :: [Int] -> Int
chainCombinations cs = H.lookupDefault 0 (L.maximum cs) $ go init $ sort cs
  where
    init :: HashMap Int Int
    init = H.fromList [(0, 1)]

    go :: HashMap Int Int -> [Int] -> HashMap Int Int
    go chains = foldl' countJolt chains

    countJolt :: HashMap Int Int -> Int -> HashMap Int Int
    countJolt chains x =
      let chains' = H.insert x 0 chains
          x1 = H.lookup (x - 1) chains'
          x2 = H.lookup (x - 2) chains'
          x3 = H.lookup (x - 3) chains'
          chains'' = case x1 of
            Nothing -> chains'
            Just v  -> H.adjust (+ v) x chains'
          chains''' = case x2 of
            Nothing -> chains''
            Just v  -> H.adjust (+ v) x chains''
          chains'''' = case x3 of
            Nothing -> chains'''
            Just v  -> H.adjust (+ v) x chains'''
      in chains''''

solution :: IO ()
solution = do
  input <- readInput "data/2020/Day10.txt"
  case traverse readInt input of
    Left err   -> putStrLn err
    Right nums -> print . chainCombinations $ nums
