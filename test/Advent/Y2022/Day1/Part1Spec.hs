module Advent.Y2022.Day1.Part1Spec where

import Test.Hspec

import Advent.Y2022.Day1.Part1

spec :: Spec
spec = do
  fdescribe "Advent - 2022 - Part 1" $ do
    it "should return maximum sum of calories" $ do
      let example = [ [1000, 2000, 3000]
                    , [4000]
                    , [5000, 6000]
                    , [7000, 8000, 9000]
                    , [10000]
                    ]
          expected = 24000

      maximumCalories example `shouldBe` expected
