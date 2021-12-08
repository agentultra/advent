module Advent.Y2021.Day6.Part1Spec where

import qualified Data.Vector as V
import Test.Hspec

import Advent.Y2021.Day6.Fish
import Advent.Y2021.Day6.Part1

spec :: Spec
spec = do
  fdescribe "Advent - Y2021 - Day 6 - Part 1" $ do
    describe "next" $ do
      context "where there are no 0" $ do
        it "should decrement fish" $ do
          (nextState . V.fromList $ [2, 3, 4]) `shouldBe` V.fromList [1, 2, 3]

      context "where there are guppies to make" $ do
        it "should append the guppies to the school of fish" $ do
          (nextState . V.fromList $ [0, 3, 4]) `shouldBe` V.fromList [6, 2, 3, 8]

    describe "runFishes" $ do
      context "given the example input" $ do
        it "should solve the puzzle" $ do
          (runFishes 18 . V.fromList $ [3, 4, 3, 1, 2])
            `shouldBe`
            V.fromList [6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8]

    describe "part1Solution" $ do
      it "should solve for the example" $ do
        (part1Solution . V.fromList $ [3, 4, 3, 1, 2]) `shouldBe` 5934
