module Advent.Y2022.Day6.Part2Spec where

import Test.Hspec

import Advent.Y2022.Day6.Part2

import Advent.Y2022.Day6.Signal

spec :: Spec
spec = do
  describe "findPacketMarkerIndex - Part 2" $ do
    it "should find a packet marker of size 14" $ do
      findPacketMarkerIndex 14 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` Just 19
