{-# LANGUAGE OverloadedStrings #-}

module Advent.Y2022.Day1.Elves where

import Data.List (groupBy)
import qualified Data.Text as T

readCalories :: Text -> [[Int]]
readCalories = map (map reallyReadInt) . groupBy (\a b -> a /= "" && b /= "") . T.lines
  where
    reallyReadInt :: Text -> Int
    reallyReadInt txt = case readInt txt of
      Left _  -> 0
      Right x -> x

totalCalories :: [[Int]] -> [Int]
totalCalories = map sum

maxCalories :: [[Int]] -> [Int]
maxCalories = sortBy (flip compare) . totalCalories
