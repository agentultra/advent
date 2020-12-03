module Advent.Y2020.Day2.Part2Spec where

import Test.Hspec

import Advent.Y2020.Day2.Part2
import Advent.Y2020.Day2.Policy

spec :: Spec
spec = do
  describe "Advent - Y2020 - Day 2 - Part 2" $ do
    describe "checkExact" $ do
      it "is valid for the correct character in the first index" $ do
        --1-3 a: abcde
        let p = Policy (1, 3) 'a'
        checkExclusiveMatch p "abcde" `shouldBe` Right True

      it "is valid for the correct character in the second index" $ do
        --1-3 a: abcde
        let p = Policy (1, 3) 'a'
        checkExclusiveMatch p "cbade" `shouldBe` Right True

      it "is invalid if both positions do not match" $ do
        -- 1-3 b: cdefg (should fail)
        let p = Policy (1, 3) 'b'

        checkExclusiveMatch p "cdefg" `shouldBe` Left "checkExclusiveMatch failed: Policy (1,3) 'b' on \"cdefg\""

      it "should invalidate because input is not exclusive match" $ do
        -- 2-9 c: ccccccccc
        let p = Policy (2, 9) 'c'
        checkExclusiveMatch p "ccccccccc" `shouldBe` Left "checkExclusiveMatch failed: Policy (2,9) 'c' on \"ccccccccc\""
