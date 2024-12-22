module Advent.Y2024.Day12.Part2Spec where

import Advent.Y2024.Day12.Input
import Advent.Y2024.Day12.Part2
import qualified Data.Text.IO as T
import Test.Hspec

spec :: Spec
spec = do
  describe "Advent - Y2024 - Day 12 - Part 2" $ do
    describe "answer" $ do
      context "Given the sample input" $ do

        raw <- runIO $ T.readFile "data/2024/Day12-sample.txt"
        let grid = fromRight (error "Invalid input") $ getInput raw

        it "Returns 1206" $ do
          answer grid `shouldBe` 1206
