module Advent.Y2024.Day2.Part1Spec where

import Test.Hspec

import Advent.Y2024.Day2.Part1
import Advent.Y2024.Day2.Report

spec :: Spec
spec = do
  describe "Advent - Y2024 -  - Part 1" $ do
    describe "delta" $ do
      context "1 `delta` 3" $ do
        it "should return (Increase 2)" $ do
          (1 `delta` 3) `shouldBe` (Increase 2)

      context "4 `delta` 1" $ do
        it "should return (Decrease 3)" $ do
          (4 `delta` 1) `shouldBe` (Decrease 3)

      context "1 `delta` 1" $ do
        it "should return NoChange" $ do
          (1 `delta` 1) `shouldBe` NoChange

    describe "reportDelta" $ do
      context "Report 7 6 4 2 1" $ do
        it "should be all decreasing" $ do
          reportDelta (Report $ 7 :| [6, 4, 2, 1])
            `shouldBe` [Decrease 1, Decrease 2, Decrease 2, Decrease 1]

    describe "allChanges" $ do
      context "all Increases" $ do
        it "should be True" $ do
          allChanges [Increase 1, Increase 2] `shouldBe` True

    describe "allWithinTolerance" $ do
      context "all decreases within 1 or 2" $ do
        it "should be True" $ do
          allWithinTolerance [Decrease 1, Decrease 2] `shouldBe` True

      context "some decreases out of tolerance" $ do
        it "should be False" $ do
          allWithinTolerance [Decrease 1, Decrease 4] `shouldBe` False

    describe "result" $ do
      context "Report 7 6 4 2 1" $ do
        it "should be Safe" $ do
          result (Report $ 7 :| [6, 4, 2, 1])
            `shouldBe` Safe

      context "Report 1 2 7 8 9" $ do
        it "should be Unsafe" $ do
          result (Report $ 1 :| [2, 7, 8, 9])
            `shouldBe` Unsafe
