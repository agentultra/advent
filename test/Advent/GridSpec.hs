module Advent.GridSpec where

import Data.Maybe
import qualified Data.Vector as V
import Test.Hspec

import Advent.Grid (Grid (..))
import qualified Advent.Grid as G

spec :: Spec
spec = do
  describe "Grid" $ do
    context "mkGrid" $ do
      it "should return Nothing if height is 0" $ do
        G.mkGrid @Int [] `shouldBe` Nothing

      it "should return Nothing if the width is 0" $ do
        G.mkGrid @Int [[]] `shouldBe` Nothing

      it "should store the grid is in row order" $ do
        let Just g = G.mkGrid [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        (V.concat . G.rows $ g) `shouldBe` _gridCells g

      it "should not change order of input" $ do
        let xs = ["abc", "def", "ghi"]
            g = fromJust $ G.mkGrid xs
            h = G.toList g
        h `shouldBe` xs

    context "get" $ do
      it "should return the element at 0, 0" $ do
        let Just g = G.mkGrid [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        G.get g 0 0 `shouldBe` Just 1

      it "should return the element at 2, 2" $ do
        let Just g = G.mkGrid [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        G.get g 2 2 `shouldBe` Just 9

      it "should return Nothing for out-of-bounds elements" $ do
        let Just g = G.mkGrid [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        G.get g 2 100 `shouldBe` Nothing
