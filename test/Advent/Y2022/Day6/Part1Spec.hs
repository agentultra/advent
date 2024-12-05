module Advent.Y2022.Day6.Part1Spec where

import Test.Hspec

import Advent.Y2022.Day6.Part1

import Advent.Y2022.Day6.Signal

spec :: Spec
spec = do
  describe "isPacketMarker" $ do
    it "should be True for a valid packet marker" $ do
      isPacketMarker 4 "abcd" `shouldBe` True

    it "should be False for an invalid packet marker" $ do
      isPacketMarker 4 "cbcd" `shouldBe` False

  describe "findPacketMarkerIndex" $ do
    it "should solve the first example" $ do
      findPacketMarkerIndex 4 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` Just 5

    it "should solve the second example" $ do
      findPacketMarkerIndex 4 "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` Just 6

    it "should solve the third example" $ do
      findPacketMarkerIndex 4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
        `shouldBe` Just 10

    it "should solve the fourth example" $ do
      findPacketMarkerIndex 4 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
        `shouldBe` Just 11
