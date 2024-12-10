module Advent.Y2024.Day7.Calibration where

import Prelude hiding (sum)

data Calibration
  = Calibration
  { result :: Int
  , input  :: NonEmpty Int
  }
  deriving (Eq, Show)

validCalibration :: Calibration -> Bool
validCalibration (Calibration expected (n :| ns)) =
  validCombination n ns
  where
    validCombination :: Int -> [Int] -> Bool
    validCombination acc [] = acc == expected
    validCombination acc (x:xs)
      = acc == expected
      || validCombination (acc + x) xs
      || validCombination (acc * x) xs
