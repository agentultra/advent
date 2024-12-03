module Advent.Y2024.Day2.Part2Spec where

import Test.Hspec

import Advent.Y2024.Day2.Report

spec :: Spec
spec = do
  describe "Advent - Y2024 -  - Part 2" $ do
    describe "subset" $ do
      context "Given Report 1 2 7 8 9" $ do
        it "should return subsets" $ do
          let expected = [ Report $ 2 :| [7, 8, 9]
                         , Report $ 1 :| [7, 8, 9]
                         , Report $ 1 :| [2, 8, 9]
                         , Report $ 1 :| [2, 7, 9]
                         , Report $ 1 :| [2, 7, 8]
                         , Report $ 1 :| [2, 7, 8, 9]
                         ]
          subset (Report $ 1 :| [2, 7, 8, 9])
            `shouldBe` expected


    describe "dampenResult" $ do
      context "Report 7 6 4 2 1" $ do
        it "should be Safe" $ do
          (dampenResult $ Report (7 :| [6, 4, 2, 1]))
            `shouldBe` Safe

      context "Report 1 2 7 8 9" $ do
        it "should be Unsafe" $ do
          (dampenResult $ Report (1 :| [2, 7, 8, 9]))
            `shouldBe` Unsafe
