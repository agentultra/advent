module Advent.Y2021.Day8.Part2Spec where

import qualified Data.Attoparsec.Text as A
import qualified Data.Set as S
import Test.Hspec

import Advent.Y2021.Day8.Digits
import Advent.Y2021.Day8.Parse

import qualified Data.Text.IO as T

spec :: Spec
spec = do
  fdescribe "Advent - Y2021 - Day 8 - Part 2" $ do
    describe "deduceDigits" $ do
      context "given the example signal pattern" $ do
        it "should deduce the digit map" $ do
          let Right display = A.parseOnly displayP
                "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf\n"
          deduceDigits (signalPatterns display)
            `shouldBe`
            [ (Digit $ S.fromList [C, A, G, E, D, B], 0)
            , (Digit $ S.fromList [C, D, F, G, E, B], 6)
            , (Digit $ S.fromList [C, E, F, A, B, D], 9)
            , (Digit $ S.fromList [F, B, C, A, D], 3)
            , (Digit $ S.fromList [G, C, D, F, A], 2)
            , (Digit $ S.fromList [C, D, F, B, E], 5)
            , (Digit $ S.fromList [A, C, E, D, G, F, B], 8)
            , (Digit $ S.fromList [D, A, B], 7)
            , (Digit $ S.fromList [E, A, F, B], 4)
            , (Digit $ S.fromList [A, B], 1)
            ]
    describe "numFrom" $ do
      context "give the sample signal pattern" $ do
        it "should return the correct integer" $ do
          let Right display = A.parseOnly displayP
                "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf\n"
          numFrom display `shouldBe` Just 5353
