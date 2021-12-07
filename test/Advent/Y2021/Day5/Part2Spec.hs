module Advent.Y2021.Day5.Part2Spec where

import qualified Data.Set as S
import Test.Hspec

import Advent.Y2021.Day5.Part2

import Advent.Y2021.Day5.Hydro

spec :: Spec
spec = do
  describe "Advent - Y2021 - Day 5 - Part 2" $ do
    it "should solve example input" $ do
      part2Solution exampleInput `shouldBe` 12

    describe "cardinal" $ do
      it "should plot diagonal lines" $ do
        (cardinal $ Line 1 1 3 3)
          `shouldBe`
          S.fromList [Vec2 (1, 1), Vec2 (2, 2), Vec2 (3, 3)]

exampleInput :: NonEmpty Line
exampleInput
  = fromList
  [ Line 0 9 5 9
  , Line 8 0 0 8
  , Line 9 4 3 4
  , Line 2 2 2 1
  , Line 7 0 7 4
  , Line 6 4 2 0
  , Line 0 9 2 9
  , Line 3 4 1 4
  , Line 0 0 8 8
  , Line 5 5 8 2
  ]

-- (0, 1), (1, 2), (2, 3)
