module Advent.Y2024.Day7.Calibration where

import Advent.List
import qualified Data.List.NonEmpty as NE
import Prelude hiding (sum)

import qualified Debug.Trace as Debug

data Calibration
  = Calibration
  { result :: Int
  , input  :: NonEmpty Int
  }
  deriving (Eq, Show)

validCalibration :: Calibration -> Bool
validCalibration (Calibration expected ns) =
  let xxs = powerCombinations . NE.toList $ ns
  in any (validCombination 0) xxs
  where
    validCombination :: Int -> [Int] -> Bool
    validCombination acc [] = acc == expected
    validCombination acc (x:xs)
      = acc + x == expected
      || acc * x == expected
      || validCombination (acc + x) xs
      || validCombination (acc * x) xs

sum :: Calibration -> Int
sum (Calibration _ ns) = foldl' (+) 0 ns
