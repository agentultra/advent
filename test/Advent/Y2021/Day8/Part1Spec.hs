module Advent.Y2021.Day8.Part1Spec where

import Test.Hspec

import Advent.Y2021.Day8.Digits
import Advent.Y2021.Day8.Parse

import qualified Data.Text.IO as T

spec :: Spec
spec = do
  fdescribe "Advent - Y2021 - Day 8 - Part 1" $ do
    describe "part1Solution" $ do
      it "should calculate the right number from the first line of the example" $ do
        let Right ds = fromList
              <$> parseInput "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n"
        part1Solution ds `shouldBe` 2
      it "should calculate the right number from the first two lines of the example" $ do
        let Right ds = fromList
              <$> parseInput "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n"
        part1Solution ds `shouldBe` 5
      it "should solve the example input" $ do
        raw <- liftIO $ T.readFile "test/Advent/Y2021/Day8/example.txt"
        case parseInput raw of
          Left err -> fail err
          Right ds -> part1Solution (fromList ds) `shouldBe` 26
