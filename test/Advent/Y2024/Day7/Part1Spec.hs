module Advent.Y2024.Day7.Part1Spec where

import Test.Hspec

import Advent.Y2024.Day7.Calibration
import Advent.Y2024.Day7.Part1
import Data.List.NonEmpty (NonEmpty (..))

spec :: Spec
spec = do
  fdescribe "Advent - Y2024 -  - Part 1" $ do
    describe "validCalibration" $ do
      context "190: 10 19" $ do
        it "should be valid" $ do
          validCalibration (Calibration 190 (10 :| [19]))
            `shouldBe` True
      context "3267: 81 40 27" $ do
        it "should be valid" $ do
          validCalibration (Calibration 3267 (81 :| [40, 27]))
            `shouldBe` True
      context "292: 11 6 16 20" $ do
        it "should be valid" $ do
          validCalibration (Calibration 292 (11 :| [6, 16, 20]))
            `shouldBe` True
