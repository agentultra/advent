module Advent.Y2021.Day10.Part1Spec where

import Test.Hspec

import Advent.Y2021.Day10.Chunk

spec :: Spec
spec = do
  describe "Advent - Y2021 - Day 10 - Part 1" $ do
    describe "validChunk" $ do
      context "given the corrupt examples" $ do
        it "should find the right character in the first example" $ do
          validChunk (fromList "{([(<{}[<>[]}>{[]{[(<()>") `shouldBe` Corrupt '}'

        it "should find the right character in the second example" $ do
          validChunk (fromList "[[<[([]))<([[{}[[()]]]") `shouldBe` Corrupt ')'

        it "should find the right character in the third example" $ do
          validChunk (fromList "[{[{({}]{}}([{[{{{}}([]") `shouldBe` Corrupt ']'

        it "should find the right character in the fourth example" $ do
          validChunk (fromList "[<(<(<(<{}))><([]([]()") `shouldBe` Corrupt ')'

        it "should find the right character in the fifth example" $ do
          validChunk (fromList "<{([([[(<>()){}]>(<<{{") `shouldBe` Corrupt '>'
