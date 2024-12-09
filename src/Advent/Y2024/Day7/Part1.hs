{-# LANGUAGE OverloadedRecordDot #-}

module Advent.Y2024.Day7.Part1 where

import Advent.List
import qualified Advent.Y2024.Day7.Calibration as C
import Advent.Y2024.Day7.Input
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.IO as T

solution :: IO ()
solution = do
  raw <- T.readFile "data/2024/Day7.txt"
  let calibrations = fromRight (error "Invalid input") $ getInput raw
      validCalibrations = filter C.validCalibration calibrations
  print . sum . map C.sum $ validCalibrations
