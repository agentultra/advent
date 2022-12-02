{-# LANGUAGE OverloadedStrings #-}

module Advent.Y2022.Day1.Part1 where

import Data.List (maximum)
import qualified Data.Text.IO as T

import Advent.Y2022.Day1.Elves

maximumCalories :: [[Int]] -> Int
maximumCalories = maximum . map sum

solution :: IO ()
solution = do
  raw <- T.readFile "data/2022/Day1.txt"
  let s = maximumCalories . readCalories $ raw
  print s
