module Advent.Y2021.Day6.Part2Spec where

import qualified Data.Vector as V
import Test.Hspec

import Advent.Y2021.Day6.Fish

spec :: Spec
spec = do
  describe "Advent - Y2021 - Day 6 - Part 2" $ do
    it "should solve for the example" $ do
      (part2Solution $ V.fromList [3, 4, 3, 1, 2]) `shouldBe` 26984457539
