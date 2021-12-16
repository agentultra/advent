module Advent.Y2021.Day9.Part1Spec where

import Test.Hspec

import qualified Advent.Grid as G
import Advent.Y2021.Day9.Vents

spec :: Spec
spec = do
  describe "Advent - Y2021 - Day 9 - Part 1" $ do
    describe "lowPoints" $ do
      it "should find the low points from the example" $ do
        let (Just cave) = G.mkGrid exampleCave
        sort (lowPoints cave) `shouldBe` [0, 1, 5, 5]

exampleCave :: [[Int]]
exampleCave =
  [ [2,1,9,9,9,4,3,2,1,0]
  , [3,9,8,7,8,9,4,9,2,1]
  , [9,8,5,6,7,8,9,8,9,2]
  , [8,7,6,7,8,9,6,7,8,9]
  , [9,8,9,9,9,6,5,6,7,8]
  ]
