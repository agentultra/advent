module Advent.Y2021.Day10.Part2Spec where

import Test.Hspec

import Advent.Y2021.Day10.Chunk

spec :: Spec
spec = do
  describe "Advent - Y2021 - Day 10 - Part 2" $ do
    describe "validChunk" $ do
      context "given the example chunks" $ do
        it "should return incompletes for the first example" $ do
          validChunk (fromList "[({(<(())[]>[[{[]{<()<>>") `shouldBe` Incomplete "{{[[({(["

    describe "autocompleteScore" $ do
      context "given the example chunks" $ do
        it "should return the expected score for the first example" $ do
          autocompleteScore (Incomplete "{{[[({([") `shouldBe` 288957
