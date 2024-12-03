{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Advent.Y2024.Day2.Report where

import qualified Data.List.NonEmpty as NE

newtype Report = Report (NonEmpty Int)
  deriving (Eq, Show)

data Delta
  = Increase Int
  | Decrease Int
  | NoChange
  deriving (Eq, Show)

delta :: Int -> Int -> Delta
delta x y = case compare x y of
  LT -> Increase $ y - x
  EQ -> NoChange
  GT -> Decrease $ x - y

reportDelta :: Report -> [Delta]
reportDelta (Report ns) = foldl' collectDelta [] . windows 2 . NE.toList $ ns
  where
    collectDelta :: [Delta] -> [Int] -> [Delta]
    collectDelta deltas (x:y:_) = delta x y : deltas
    collectDelta _ _ = error "collectDelta: expected pair"

subset :: Report -> [Report]
subset (Report ns)
  = map (Report . NE.fromList)
  . subsets
  . NE.toList $ ns
  where
    subsets :: [Int] -> [[Int]]
    subsets xs = [ take x xs ++ drop (x + 1) xs | x <- [0..length xs] ]

data Result = Safe | Unsafe
  deriving (Eq, Show)

isSafe :: Result -> Bool
isSafe = \case
  Safe   -> True
  Unsafe -> False

isUnsafe :: Result -> Bool
isUnsafe = \case
  Safe   -> False
  Unsafe -> True

result :: Report -> Result
result = checkSafe . reportDelta
  where
    checkSafe :: [Delta] -> Result
    checkSafe deltas = toResult $ allChanges deltas && allWithinTolerance deltas

    toResult :: Bool -> Result
    toResult = \case
      True  -> Safe
      False -> Unsafe

allChanges :: [Delta] -> Bool
allChanges deltas = all isIncrease deltas || all isDecrease deltas
  where
    isIncrease :: Delta -> Bool
    isIncrease = \case
      Increase _ -> True
      _          -> False

    isDecrease :: Delta -> Bool
    isDecrease = \case
      Decrease _ -> True
      _          -> False

allWithinTolerance :: [Delta] -> Bool
allWithinTolerance = all withinTolerance
  where
    withinTolerance :: Delta -> Bool
    withinTolerance = \case
      Increase x | x >= 1 && x <= 3 -> True
      Decrease x | x >= 1 && x <= 3 -> True
      _                             -> False

dampenResult :: Report -> Result
dampenResult = toResult . any (isSafe . result) . subset
  where
    toResult :: Bool -> Result
    toResult = \case
      True  -> Safe
      False -> Unsafe
