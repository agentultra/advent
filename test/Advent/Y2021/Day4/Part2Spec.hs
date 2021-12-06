module Advent.Y2021.Day4.Part2Spec where

import qualified Data.Attoparsec.Text as A
import qualified Data.Text.IO as T
import Test.Hspec

import Advent.Y2021.Day4.Bingo
import Advent.Y2021.Day4.Parse

spec :: Spec
spec = do
  fdescribe "Advent - Y2021 - Part 2" $ do
    it "should solve the example input" $ do
      raw <- T.readFile "test/Advent/Y2021/Day4/example.txt"
      case A.parseOnly parseInput raw of
        Left err -> print err
        Right (picks, boards) ->
          part2Solution picks boards
          `shouldBe`
          1924
