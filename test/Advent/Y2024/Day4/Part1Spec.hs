module Advent.Y2024.Day4.Part1Spec where

import Test.Hspec

import Advent.Grid
import Advent.Y2024.Day4.Part1
import Advent.Y2024.Day4.WordSearch
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V

smallGrid :: Grid Char
smallGrid
  = fromJust . mkGrid $
  [ "..X..."
  , ".SAMX."
  , ".A..A."
  , "XMAS.S"
  , ".X...."
  ]

largerGrid :: Grid Char
largerGrid
  = fromJust . mkGrid $
  [ "MMMSXXMASM"
  , "MSAMXMSMSA"
  , "AMXSXMAAMM"
  , "MSAMASMSMX"
  , "XMASAMXAMM"
  , "XXAMMXXAMA"
  , "SMSMSASXSS"
  , "SAXAMASAAA"
  , "MAMMMXMMMM"
  , "MXMXAXMASX"
  ]

crazyGrid :: Grid Char
crazyGrid
  = fromJust . mkGrid $
  [ "S..S..S"
  , ".A.A.A."
  , "..MMM.."
  , "SAMXMAS"
  , "..MMM.."
  , ".A.A.A."
  , "S..S..S"
  ]

crossGrid :: Grid Char
crossGrid
  = fromJust . mkGrid $
  [ "X.X."
  , ".MM."
  , "XMAS"
  , "..SS"
  ]

hSamxGrid :: Grid Char
hSamxGrid = fromJust . mkGrid $ ["SAMX"]

vSamxGrid :: Grid Char
vSamxGrid = fromJust . mkGrid $ [ "S", "A", "M", "X" ]

spec :: Spec
spec = do
  describe "Advent - Y2024 -  - Part 1" $ do
    describe "north" $ do
      context "Given the small example" $ do
        it "should find XMAS at 1,4" $ do
          north (1, 4) smallGrid `shouldBe` 1
        it "should not find XMAS at 0,4" $ do
          north (0, 4) smallGrid `shouldBe` 0
    describe "south" $ do
      context "Given the larger example" $ do
        it "should find XMAS at 9,4" $ do
          south (9,3) largerGrid `shouldBe` 1
    describe "west" $ do
      context "Given the small example" $ do
        it "should find XMAS at 4,1" $ do
          west (4, 1) smallGrid `shouldBe` 1
        it "should not find XMAS at 0,3" $ do
          west (0, 3) smallGrid `shouldBe` 0
        it "should not find XMAS at 3,3" $ do
          west (3, 3) smallGrid `shouldBe` 0
    describe "east" $ do
      context "Given the small example" $ do
        it "should find XMAS at 0,3" $ do
          east (0, 3) smallGrid `shouldBe` 1
    describe "southE" $ do
      context "Given the small example" $ do
        it "should find XMAS at 2,0" $ do
          southE (2, 0) smallGrid `shouldBe` 1
    describe "southW" $ do
      context "Given the larger example" $ do
        it "should find XMAS at 9,3" $ do
          southW (9, 3) largerGrid `shouldBe` 1
    describe "northW" $ do
      context "Given the larger example" $ do
        it "should find XMAS at 9,9" $ do
          northW (9, 9) largerGrid `shouldBe` 1
    describe "northE" $ do
      context "Given the larger example" $ do
        it "should find XMAS at 0,5" $ do
          northE (0, 5) largerGrid `shouldBe` 1
        it "should not find XMAS at 0,9" $ do
          northE (0, 9) largerGrid `shouldBe` 0
        it "should not find anything out of bounds" $ do
          northE (10, 10) largerGrid `shouldBe` 0

    describe "findXmas" $ do
      context "Given the larger example" $ do
        it "should count 2 XMAS at 9,3" $ do
          findXmas (9, 3) largerGrid `shouldBe` 2
        it "should count 1 at 0,5" $ do
          findXmas (0, 5) largerGrid `shouldBe` 1
      context "Given the crazy grid" $ do
        it "should count 8 XMAS at 3,3" $ do
          findXmas (3, 3) crazyGrid `shouldBe` 8

    describe "hits" $ do
      context "Given the small example" $ do
        it "should find 4 XMAS" $ do
          hits (== 'X') findXmas smallGrid `shouldBe` 4
      context "Given the larger example" $ do
        it "should find 18 XMAS" $ do
          hits (== 'X') findXmas largerGrid `shouldBe` 18
      context "Given the crazy grid" $ do
        it "should find 8 XMAS when we target 'X'" $ do
          hits (== 'X') findXmas crazyGrid `shouldBe` 8
        it "should find none when we target '.'" $ do
          hits (== '.') findXmas crazyGrid `shouldBe` 0
      context "Given the horizontal SAMX grid" $ do
        it "should find 1 hit" $ do
          hits (== 'X') findXmas hSamxGrid `shouldBe` 1
      context "Given the vertical SAMX grid" $ do
        it "should find 1 hit" $ do
          hits (== 'X') findXmas vSamxGrid `shouldBe` 1

      context "Given the cross grid" $ do
        it "should find 3" $ do
          hits (== 'X') findXmas crossGrid `shouldBe` 3
