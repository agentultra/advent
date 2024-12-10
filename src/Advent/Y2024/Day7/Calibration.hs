module Advent.Y2024.Day7.Calibration where

import Data.List.NonEmpty as NE
import Prelude hiding (sum)

data Calibration
  = Calibration
  { result :: Int
  , input  :: NonEmpty Int
  }
  deriving (Eq, Show)

validCalibration :: NonEmpty (Int -> Int -> Int) -> Calibration -> Bool
validCalibration ops (Calibration expected (n :| ns)) =
  validCombination n ns
  where
    validCombination :: Int -> [Int] -> Bool
    validCombination acc [] = acc == expected
    validCombination acc (x:xs)
      = or [ validCombination (acc `op` x) xs | op <- NE.toList ops ]

(.||.) :: Int -> Int -> Int
x .||. y = x * (10 ^ n) + y
  where
    n = floor $ log10 (fromIntegral y) + 1
    log10 = logBase 10
