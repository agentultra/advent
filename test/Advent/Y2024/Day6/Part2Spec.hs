module Advent.Y2024.Day6.Part2Spec where

import Test.Hspec

import Advent.Input
import Advent.Y2024.Day6.Input
import Advent.Y2024.Day6.Part2
import Advent.Y2024.Day6.Room

spec :: Spec
spec = do
  fdescribe "Advent - Y2024 -  - Part 2" $ do

    raw <- runIO $ readInput "data/2024/Day6-sample.txt"
    let (room, guardPos) = orElse "Invalid input" $ parseInput raw

    context "Given the sample input" $ do
      it "should find 6 loop positions" $ do
        (evalState answer (mkRoom guardPos room)) `shouldBe` 6
