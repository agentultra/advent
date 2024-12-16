module Advent.Y2024.Day11.Part1Spec where

import Test.Hspec

import Advent.Y2024.Day11.Stone

spec :: Spec
spec = do
  fdescribe "Advent - Y2024 - Day 11 - Part 1" $ do
    describe "blink" $ do
      context "Given 0 1 10 99 999" $ do
        it "should give 1 2024 1 0 9 9 2021976" $ do
          blink ["0", "1", "10", "99", "999"]
            `shouldBe` ["1", "2024", "1", "0", "9", "9", "2021976"]

    describe "answer" $ do
      context "Given 125 17" $ do
        it "should return 22 stones after blinking 6 times" $
          blinkStones 6 ["125", "17"] `shouldBe` 22
        it "should return 55312 stones after blinking 25 times" $
          blinkStones 25 ["125", "17"] `shouldBe` 55312
