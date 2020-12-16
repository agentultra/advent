module Advent.Y2020.Day11.Part1Spec where

import qualified Data.HashMap.Strict as H
import Test.Hspec

import Advent.Y2020.Day11.Grid
import Advent.Y2020.Day11.Part1

testGrid :: Grid
testGrid = H.fromList
  [ ((0, 0), 'L'), ((1, 0), 'L'), ((2, 0), 'L'), ((3, 0), 'L')
  , ((0, 1), '.'), ((1, 1), 'L'), ((2, 1), '.'), ((3, 1), 'L')
  , ((0, 2), '.'), ((1, 2), '.'), ((2, 2), '.'), ((3, 2), 'L')
  , ((0, 3), 'L'), ((1, 3), 'L'), ((2, 3), 'L'), ((3, 3), '#')
  ]

testGrid' :: Grid
testGrid' = H.fromList
  [ ((0, 0), '#'), ((1, 0), 'L'), ((2, 0), 'L'), ((3, 0), 'L')
  , ((0, 1), '.'), ((1, 1), 'L'), ((2, 1), '.'), ((3, 1), 'L')
  , ((0, 2), '.'), ((1, 2), '.'), ((2, 2), '.'), ((3, 2), 'L')
  , ((0, 3), 'L'), ((1, 3), 'L'), ((2, 3), 'L'), ((3, 3), '#')
  ]

testGrid'' :: Grid
testGrid'' = H.fromList
  [ ((0, 0), 'L'), ((1, 0), 'L'), ((2, 0), 'L'), ((3, 0), 'L')
  , ((0, 1), '.'), ((1, 1), 'L'), ((2, 1), '.'), ((3, 1), 'L')
  , ((0, 2), '.'), ((1, 2), '.'), ((2, 2), '.'), ((3, 2), 'L')
  , ((0, 3), 'L'), ((1, 3), 'L'), ((2, 3), 'L'), ((3, 3), 'L')
  ]

spec :: Spec
spec = do
  describe "Advent - Y2020 - Day 11 - Part 1" $ do
    describe "Grid" $ do
      it "should return a valid neighbourhood" $ do
        neighborhood testGrid (1, 1)
          `shouldBe`
          Just ('L' :| ['L', 'L', '.', '.', '.', '.', '.'])

      it "should return a neighbourhood from a corner" $ do
        neighborhood testGrid (0, 0)
          `shouldBe`
          Just ('L' :| ['L', '.'])

      it "should return a number of occupied seats" $ do
        occupiedSeats testGrid (2, 2)
          `shouldBe`
          Just 1

      it "should apply the occupied rule" $ do
        unoccupied testGrid (0, 0)
          `shouldBe`
          Just testGrid'

      it "should apply the unoccupied rule" $ do
        occupied testGrid (3, 3)
          `shouldBe`
          Just testGrid''
