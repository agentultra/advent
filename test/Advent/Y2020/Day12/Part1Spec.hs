module Advent.Y2020.Day12.Part1Spec where

import qualified Data.List.NonEmpty as N
import Test.Hspec

import Advent.Y2020.Day12.Parse
import Advent.Y2020.Day12.Ship

example1 = "F10\nN3\nF7\nR90\nF11"

spec :: Spec
spec = do
  describe "Advent - Y2020 - Day 12 - Part 1" $ do
    describe "example" $ do
      it "should find the same answer as the example" $ do
        (distance . processCommands defaultShip . N.fromList  <$> parseInput example1)
          `shouldBe`
          Right 25
