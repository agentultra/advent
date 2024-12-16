module Advent.Y2024.Day10.Part1Spec where

import Test.Hspec

import Advent.Y2024.Day10.Input
import Advent.Y2024.Day10.Part1
import Advent.Y2024.Day10.Trail
import qualified Data.List as List
import qualified Data.Text.IO as T

spec :: Spec
spec = do
  describe "Advent - Y2024 - Day10 - Part 1" $ do
    context "Given the small example" $ do

      raw <- runIO $ T.readFile "data/2024/Day10-small.txt"
      let trailMap = fromRight (error "Invalid input") $ getInput raw

      describe "getTrailheads" $ do
        it "should find 2" $ do
          length (getTrailheads trailMap) `shouldBe` 2

      describe "findTrails" $ do
        it "should find 1 at (0, 0)" $ do
          findTrails List.nub trailMap (Trailhead (0, 0)) `shouldBe` 1

    context "Given the sample" $ do

      raw <- runIO $ T.readFile "data/2024/Day10-sample.txt"
      let trailMap = fromRight (error "Invalid input") $ getInput raw

      describe "getTrailheads" $ do
        it "should find 9" $ do
          length (getTrailheads trailMap) `shouldBe` 9

      describe "validDirections" $ do
        it "should find [(2, 1), (3, 0)] at (2, 0)" $ do
          validDirections (2, 0) trailMap
            `shouldBe` [(3, 0), (2, 1)]

        it "should find [(3, 3), (2, 2)] at (3, 2)" $ do
          validDirections (3, 2) trailMap
            `shouldBe` [(3, 3), (2, 2)]

        it "should find [(1, 0)] at (0, 0)" $ do
          validDirections (0, 0) trailMap
            `shouldBe` [(1, 0)]

      describe "findTrails" $ do
        it "should find 5 at (2, 0)" $ do
          findTrails List.nub trailMap (Trailhead (2, 0)) `shouldBe` 5
