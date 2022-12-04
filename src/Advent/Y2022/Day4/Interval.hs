{-# LANGUAGE OverloadedStrings #-}

module Advent.Y2022.Day4.Interval where

import qualified Data.Text as T

data Interval a = Interval a a
  deriving (Eq, Show)

parseIntervals :: Text -> (Interval Int, Interval Int)
parseIntervals raw =
  let [rawA, rawB] = T.splitOn "," raw
  in (parseIntervalInt rawA, parseIntervalInt rawB)

parseIntervalInt :: Text -> Interval Int
parseIntervalInt raw =
  let [digitsA, digitsB] = T.splitOn "-" raw
  in Interval (reallyReadInt digitsA) (reallyReadInt digitsB)
  where
    reallyReadInt :: Text -> Int
    reallyReadInt txt = case readInt txt of
      Left _  -> 0
      Right x -> x

contains :: Ord a => Interval a -> Interval a -> Bool
contains (Interval x y) (Interval a b) = x <= a && y >= b

overlaps :: Ord a => Interval a -> Interval a -> Bool
overlaps (Interval x y) (Interval a b) = a <= y && a >= x || x <= b && x >= a
