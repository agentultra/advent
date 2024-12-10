module Advent.Y2024.Day7.Part1Spec where

import Test.Hspec

import Advent.Y2024.Day7.Calibration
import Advent.Y2024.Day7.Input
import Advent.Y2024.Day7.Part1
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.IO as T

spec :: Spec
spec = do
  fdescribe "Advent - Y2024 -  - Part 1" $ do
    describe "validCalibration" $ do
      context "10: 10" $ do
        it "should be valid" $ do
          validCalibration ((+) :| [(*)]) (Calibration 10 (10 :| []))
            `shouldBe` True
      context "10: 1 2 3" $ do
        it "should be invalid" $ do
          validCalibration ((+) :| [(*)]) (Calibration 10 (1 :| [2, 3]))
            `shouldBe` False
      context "83: 17 5" $ do
        it "should be valid" $ do
          validCalibration ((+) :| [(*)]) (Calibration 83 (17 :| [5]))
          `shouldBe` False
      context "190: 10 19" $ do
        it "should be valid" $ do
          validCalibration ((+) :| [(*)]) (Calibration 190 (10 :| [19]))
            `shouldBe` True
      context "3267: 81 40 27" $ do
        it "should be valid" $ do
          validCalibration ((+) :| [(*)]) (Calibration 3267 (81 :| [40, 27]))
            `shouldBe` True
      context "292: 11 6 16 20" $ do
        it "should be valid" $ do
          validCalibration ((+) :| [(*)]) (Calibration 292 (11 :| [6, 16, 20]))
            `shouldBe` True
      context "156: 15 6" $ do
        it "should be invalid" $ do
          validCalibration ((+) :| [(*)]) (Calibration 156 (15 :| [6]))
            `shouldBe` False
      context "Given the sample input" $ do

        raw <- runIO $ T.readFile "data/2024/Day7-sample.txt"
        let calibrations = fromRight (error "Invalid input") $ getInput raw

        it "should map each to the expected results" $ do
          map (validCalibration ((+) :| [(*)])) calibrations
            `shouldBe`
            [ True, True, False, False, False, False, False, False, True]
