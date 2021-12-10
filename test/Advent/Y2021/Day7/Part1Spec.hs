module Advent.Y2021.Day7.Part1Spec where

import Test.Hspec

import Advent.Y2021.Day7.Crab

spec :: Spec
spec = do
  describe "Advent - Y2021 - Day 7 - Part 1" $ do
    it "solves the example input" $ do
      (part1Solution $ fromList [16,1,2,0,4,2,7,1,2,14])
        `shouldBe` 37
