module Advent.Y2021.Day9.Part2Spec where

import Test.Hspec

import qualified Advent.Grid as G
import Advent.Y2021.Day9.Vents

spec :: Spec
spec = do
  describe "Advent - Y2021 - Day 9 - Part 2" $ do
    describe "basinNeighbours" $ do
      it "should return the orthogonal points whose map value is < 9" $ do
        let (Just g) = G.mkGrid exampleCave
        (sort . basinNeighbours g $ (3, 2)) `shouldBe` sort [(3, 1), (4, 2), (3, 3), (2, 2)]

      it "should filter out points whose map value is >= 9" $ do
        let (Just g) = G.mkGrid exampleCave
        basinNeighbours g (5, 2) `shouldBe` [(4,2)]

      it "should only consider orthogonal elements on the closed plane" $ do
        let (Just g) = G.mkGrid exampleCave
        basinNeighbours g (9, 0) `shouldBe` [(8,0), (9, 1)]

    describe "basin" $ do
      context "given the top-right basin of the example" $ do
        it "should return a size of 9" $ do
          let (Just g) = G.mkGrid exampleCave
          basin g (9, 0) `shouldBe` 9

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
