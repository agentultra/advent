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

testGrid'' :: Grid
testGrid'' = H.fromList
  [ ((0, 0), 'L'), ((1, 0), 'L'), ((2, 0), 'L'), ((3, 0), 'L')
  , ((0, 1), '.'), ((1, 1), 'L'), ((2, 1), '.'), ((3, 1), 'L')
  , ((0, 2), '.'), ((1, 2), '.'), ((2, 2), '.'), ((3, 2), 'L')
  , ((0, 3), 'L'), ((1, 3), 'L'), ((2, 3), 'L'), ((3, 3), 'L')
  ]

testGridOccupied'' :: Grid
testGridOccupied'' = H.fromList
  [ ((0, 0), '#'), ((1, 0), '#'), ((2, 0), '#'), ((3, 0), '#')
  , ((0, 1), '.'), ((1, 1), '#'), ((2, 1), '.'), ((3, 1), '#')
  , ((0, 2), '.'), ((1, 2), '.'), ((2, 2), '.'), ((3, 2), '#')
  , ((0, 3), '#'), ((1, 3), '#'), ((2, 3), '#'), ((3, 3), '#')
  ]

exampleGrid :: [Text]
exampleGrid = [ "L.LL.LL.LL"
              , "LLLLLLL.LL"
              , "L.L.L..L.."
              , "LLLL.LL.LL"
              , "L.LL.LL.LL"
              , "L.LLLLL.LL"
              , "..L.L....."
              , "LLLLLLLLLL"
              , "L.LLLLLL.L"
              , "L.LLLLL.LL"
              ]

exampleStep1 = [ "#.##.##.##"
               , "#######.##"
               , "#.#.#..#.."
               , "####.##.##"
               , "#.##.##.##"
               , "#.#####.##"
               , "..#.#....."
               , "##########"
               , "#.######.#"
               , "#.#####.##"
               ]

exampleStep2 = [ "#.LL.L#.##"
               , "#LLLLLL.L#"
               , "L.L.L..L.."
               , "#LLL.LL.L#"
               , "#.LL.LL.LL"
               , "#.LLLL#.##"
               , "..L.L....."
               , "#LLLLLLLL#"
               , "#.LLLLLL.L"
               , "#.#LLLL.##"
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

      it "should apply the unoccupied rule" $ do
        unoccupied testGrid (0, 0)
          `shouldBe`
          True

      it "should apply the occupied rule" $ do
        occupied testGrid (3, 3)
          `shouldBe`
          False

      it "should step an unoccupied grid to a fully occupied one" $ do
        step testGrid'' `shouldBe` testGridOccupied''

      it "should step the example grid" $ do
        (step $ parseGrid exampleGrid) `shouldBe` parseGrid exampleStep1

      it "should step the example grid two" $
        (step $ parseGrid exampleStep1) `shouldBe` parseGrid exampleStep2

      it "should solve the example problem" $ do
        (solve $ parseGrid exampleGrid) `shouldBe` 37
