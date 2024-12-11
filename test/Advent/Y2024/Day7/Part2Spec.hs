module Advent.Y2024.Day7.Part2Spec where

import Advent.Y2024.Day7.Calibration
import Advent.Y2024.Day7.Input
import qualified Data.Text.IO as T
import Test.Hspec

spec :: Spec
spec = do
  describe "Advent - Y2024 -  - Part 2" $ do
    describe "The .||. operator" $ do
      context "12 .||. 34" $ do
        it "should equal 1234" $ do
          (12 .||. 34) `shouldBe` 1234

    describe "validCalibration" $ do
      context "With the .||. operator" $ do
        context "156: 15 6" $ do
          it "should be valid" $ do
            validCalibration ((+) :| [(*), (.||.)]) (Calibration 156 (15 :| [6]))
              `shouldBe` True
        context "7290: 6 8 6 15" $ do
          it "should be valid" $ do
            validCalibration ((+) :| [(*), (.||.)]) (Calibration 7290 (6 :| [8, 6, 15]))
              `shouldBe` True
        context "192: 17 8 14" $ do
          it "should be valid" $ do
            validCalibration ((+) :| [(*), (.||.)]) (Calibration 192 (17 :| [8, 14]))
              `shouldBe` True

    context "Given the sample input" $ do

        raw <- runIO $ T.readFile "data/2024/Day7-sample.txt"
        let calibrations = fromRight (error "Invalid input") $ getInput raw

        it "should map each to the expected results" $ do
          map (validCalibration ((+) :| [(*), (.||.)])) calibrations
            `shouldBe`
            [ True, True, False, True, True, False, True, False, True]
