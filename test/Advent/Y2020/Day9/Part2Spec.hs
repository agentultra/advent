module Advent.Y2020.Day9.Part2Spec where

import Test.Hspec

import Advent.Y2020.Day9.Part2

spec :: Spec
spec = do
  describe "Advent - Y2020 - Day 9 - Part 2" $ do
    describe "crackXmas" $ do
      it "can solve the example" $ do
        crackXmas 127 [ 35
                      , 20
                      , 15
                      , 25
                      , 47
                      , 40
                      , 62
                      , 55
                      , 65
                      , 95
                      , 102
                      , 117
                      , 150
                      , 182
                      , 127
                      , 219
                      , 299
                      , 277
                      , 309
                      , 576
                      ] `shouldBe` Just 62
