module Advent.Y2020.Day10.Part2Spec where

import Test.Hspec

import Advent.Y2020.Day10.Part2

spec :: Spec
spec = do
  describe "Advent - Y2020 - Day 10 - Part 2" $ do
    describe "chainCombinations" $ do
      it "should return the same result as the small example" $ do
        let ns = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]
        chainCombinations ns `shouldBe` 8

      it "should return the same result as the large example" $ do
        let ns = [ 28, 33, 18, 42, 31
                 , 14, 46, 20, 48, 47
                 , 24, 23, 49, 45, 19
                 , 38, 39, 11, 1, 32
                 , 25, 35, 8, 17, 7
                 , 9, 4, 2, 34
                 , 10, 3 ]
        chainCombinations ns `shouldBe` 19208
