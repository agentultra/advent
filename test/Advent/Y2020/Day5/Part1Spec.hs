module Advent.Y2020.Day5.Part1Spec where

import Test.Hspec

import Advent.Y2020.Day5.SeatCode

spec :: Spec
spec = do
  describe "Advent - Y2020 - Day 5 - Part 1" $ do
    describe "findRow" $ do
      it "finds the correct row from the example" $ do
        let code = SeatCode F B F B B F F L L L
        findRow code `shouldBe` Just 44

    describe "findColumn" $ do
      it "finds the correct column from the example" $ do
        let code = SeatCode F F F F F F F R L R
        findColumn code `shouldBe` Just 5
