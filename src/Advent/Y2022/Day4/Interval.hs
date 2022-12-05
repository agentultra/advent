{-# LANGUAGE OverloadedStrings #-}

module Advent.Y2022.Day4.Interval where

import qualified Data.Text as T

data Interval = Interval Int Int
  deriving (Eq, Show)

parseIntervals :: Text -> (Interval, Interval)
parseIntervals raw =
  let [rawA, rawB] = T.splitOn "," raw
  in (parseIntervalInt rawA, parseIntervalInt rawB)

parseIntervalInt :: Text -> Interval
parseIntervalInt raw =
  let [digitsA, digitsB] = T.splitOn "-" raw
      (intA, intB) = (reallyReadInt digitsA, reallyReadInt digitsB)
  in if intA <= intB
     then Interval intA intB
     else error "Invalid Interval"
  where
    reallyReadInt :: Text -> Int
    reallyReadInt txt = case readInt txt of
      Left _  -> 0
      Right x -> x

contains :: Interval -> Interval -> Bool
contains (Interval x y) (Interval a b) = x <= a && y >= b

overlaps :: Interval -> Interval -> Bool
overlaps (Interval x y) (Interval a b) = x <= b && a <= y
