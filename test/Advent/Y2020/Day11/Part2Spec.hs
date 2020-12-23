module Advent.Y2020.Day11.Part2Spec where

import Test.Hspec

import Advent.Y2020.Day11.Grid
import Advent.Y2020.Day11.Part2

exampleGrid1 :: Grid
exampleGrid1 =
  parseGrid $ [ ".......#."
              , "...#....."
              , ".#......."
              , "........."
              , "..#L....L"
              , "....#...."
              , ".....L..."
              , "#........"
              , "...#....."
              ]

exampleGrid2 :: Grid
exampleGrid2 =
  parseGrid $ [ "L.LL.LL.LL"
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

exampleGrid3 :: Grid
exampleGrid3 =
  parseGrid $ [ "#.##.##.##"
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

exampleGrid4 :: Grid
exampleGrid4 =
  parseGrid $ [ "#.LL.LL.L#"
              , "#LLLLLL.LL"
              , "L.L.L..L.."
              , "LLLL.LL.LL"
              , "L.LL.LL.LL"
              , "L.LLLLL.LL"
              , "..L.L....."
              , "LLLLLLLLL#"
              , "#.LLLLLL.L"
              , "#.LLLLL.L#"
              ]

hitTest1 :: Grid
hitTest1 = parseGrid ["#"]

spec :: Spec
spec = do
  describe "Advent - Y2020 - Day 11 - Part 2" $ do
    describe "Grid the Next Generation" $ do
      it "should find a occupied seat with a hit test" $ do
        hitTest exampleGrid1 (3, 4) (0, (-1)) `shouldBe` Just '#'

      it "should find an unoccupied seat with a hit test" $ do
        hitTest exampleGrid1 (3, 4) (1, 0) `shouldBe` Just 'L'

      it "should find the first object with the hit test" $ do
        hitTest exampleGrid1 (3, 4) (1, 1) `shouldBe` Just '#'

      it "should not count the cell we start" $ do
        hitTest hitTest1 (0, 0) (-1, -1) `shouldBe` Nothing

      it "should find all the visible occupied seats from the example" $ do
        visibleSeats exampleGrid1 (3, 4) Occupied `shouldBe` Just 7

      it "should find all the visible unoccupied seats from the example" $ do
        visibleSeats exampleGrid1 (3, 4) Unoccupied `shouldBe` Just 1

      it "should find 3 unoccupied seats at 0, 0 in exampleGrid2" $ do
        visibleSeats exampleGrid2 (0, 0) Unoccupied `shouldBe` Just 3

      -- it "should find the right number of occupied seats" $ do
      --   visibleSeats edgeCase1 (4, 4) `shouldBe` Just _

      it "should step the example" $ do
        visibleStep exampleGrid2 `shouldBe` exampleGrid3

      it "should step again" $ do
        visibleStep exampleGrid3 `shouldBe` exampleGrid4
