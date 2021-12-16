module Advent.GridSpec where

import qualified Data.Vector as V
import Test.Hspec

import Advent.Grid

spec :: Spec
spec = do
  fdescribe "Grid" $ do
    context "mkGrid" $ do
      it "should return Nothing if height is 0" $ do
        mkGrid @Int [] `shouldBe` Nothing

      it "should return Nothing if the width is 0" $ do
        mkGrid @Int [[]] `shouldBe` Nothing

      it "should store the grid is in row order" $ do
        let Just g = mkGrid [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        (V.concat . gridRows $ g) `shouldBe` _gridCells g
