module Advent.Y2021.Day9.Part2Spec where

import Test.Hspec

import qualified Advent.Grid as G
import Advent.Y2021.Day9.Vents

spec :: Spec
spec = do
  fdescribe "Advent - Y2021 - Day 9 - Part 2" $ do
    it "should solve example" $ do
      (part2Solution . fmap fromList . fromList $ exampleCave)
        `shouldBe`
        1134

exampleCave :: [[Int]]
exampleCave =
  [ [2,1,9,9,9,4,3,2,1,0]
  , [3,9,8,7,8,9,4,9,2,1]
  , [9,8,5,6,7,8,9,8,9,2]
  , [8,7,6,7,8,9,6,7,8,9]
  , [9,8,9,9,9,6,5,6,7,8]
  ]
