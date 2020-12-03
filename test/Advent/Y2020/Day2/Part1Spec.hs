module Advent.Y2020.Day2.Part1Spec where

import Test.Hspec

import Advent.Y2020.Day2.Parse
import Advent.Y2020.Day2.Part1
import Advent.Y2020.Day2.Policy

spec :: Spec
spec = do
  describe "Advent - Y2020 - Day 2 - Part 1" $ do
    describe "check" $ do
      it "should validate a policy against valid input" $ do
        let p = Policy (1, 3) 'a'
        check p "abcdaa" `shouldBe` Right True

      it "should validate a policy against valid input if valid chars as suffix" $ do
        let p = Policy (1, 3) 'a'
        check p "bbbbaaa" `shouldBe` Right True

      it "should invalidate a policy against invalid input" $ do
        let p = Policy (1, 3) 'a'
        check p "cdefg" `shouldBe` Left "check failed: Policy (1,3) 'a' on \"cdefg\""

      it "should invalidate a policy against an input with too many chars" $ do
        let p = Policy (1, 3) 'a'
        check p "aaaaaaaaaaaaaaaa" `shouldBe` Left "check failed: Policy (1,3) 'a' on \"aaaaaaaaaaaaaaaa\""

      it "should invalidate a policy where no characters show up" $ do
        let p = Policy (1, 3) 'a'
        check p "bbbbbbb" `shouldBe` Left "check failed: Policy (1,3) 'a' on \"bbbbbbb\""

    describe "checkMany" $ do
      it "should return 0 for an empty list" $ do
        let empty = [] :: [(Policy Char, [Char])]
        checkMany check empty `shouldBe` 0

      it "should return 1 for one passing policy check" $ do
        let checks = [ (Policy (1, 2) 'a', "a")
                     , (Policy (2, 3) 'b', "aba")
                     ]
        checkMany check checks `shouldBe` 1

      it "should return 1 for the last passing policy check" $ do
        let checks = [ (Policy (2, 3) 'b', "aba")
                     , (Policy (1, 2) 'a', "a")
                     ]
        checkMany check checks `shouldBe` 1

    describe "parseInput" $ do
      it "should get the whole input string" $ do
        parseInput "15-16 v: vvvvvvvvnvvvvcvvvvgv\n"
          `shouldBe` Right [(Policy (15, 16) 'v', "vvvvvvvvnvvvvcvvvvgv")]
