module Advent.Y2020.Day7.Part1Spec where

import Data.Attoparsec.Text
import Test.Hspec

import Advent.Y2020.Day7.Part1
import Advent.Y2020.Day7.Parse

spec :: Spec
spec = do
  describe "Advent - Y2020 - Day 7 - Part 1" $ do
    describe "Parse" $ do
      it "can parse a plural bag" $ do
        let input = "bright olive bags "
        parseOnly bagP input `shouldBe` Right "bright olive"

      it "can parse a singular bag" $ do
        let input = "bright olive bag "
        parseOnly bagP input `shouldBe` Right "bright olive"

      it "can parse a plural bag with count" $ do
        let input = "5 dotted white bags"
        parseOnly countBagP input `shouldBe` Right (5, "dotted white")

      it "can parse a singular bag with count" $ do
        let input = "1 dotted white bag"
        parseOnly countBagP input `shouldBe` Right (1, "dotted white")

      it "can parse a non-empty contains list" $ do
        let input = "5 dotted white bags, 2 wavy lavender bags."
        let expected = [(5, "dotted white"), (2, "wavy lavender")]
        parseOnly containsP input `shouldBe` Right expected

      it "can parse an empty contains list" $ do
        let input = "no other bags."
        parseOnly containsP input `shouldBe` Right []

      it "can parse a bag node" $ do
        let input = "pale lavender bags contain 3 bright lavender bags, 5 wavy blue bags."
        let expected = ("pale lavender", [(3, "bright lavender"), (5, "wavy blue")])
        parseOnly bagNodeP input `shouldBe` Right expected

      it "can parse an input file" $ do
        let input = "bright olive bags contain 5 dotted white bags, 2 wavy lavender bags.\nshiny purple bags contain no other bags."
        let expected = [ ("bright olive", [(5, "dotted white"), (2, "wavy lavender")])
                       , ("shiny purple", [])]
        parseInput input `shouldBe` Right expected
