module Advent.Y2024.Day5.Part1Spec where

import Test.Hspec

import Advent.Y2024.Day5.Input
import Advent.Y2024.Day5.Part1
import Advent.Y2024.Day5.Rule
import Advent.Y2024.Day5.Update
import Data.Either
import Data.List ((!!))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.IO as T

spec :: Spec
spec = do
  describe "Advent - Y2024 -  - Part 1" $ do

    raw <- runIO $ T.readFile "data/2024/Day5-sample.txt"
    let (rules, updates) = fromRight (error "Invalid input") $ getInput raw
        ruleMap = mkRuleMap rules

    describe "validUpdate" $ do
      context "Update 75,97,47,61,53" $ do
        it "it should break rule 97|75" $ do
          validUpdate ruleMap (Update (75 :| [97, 47, 61, 53]))
            `shouldBe` False
      context "Update 61,13,29" $ do
        it "should break rule 29|13" $ do
          validUpdate ruleMap (Update (61 :| [13, 29]))
            `shouldBe` False

    describe "validUpdates" $ do
      context "Given the sample input" $ do
        it "should return only valid updates" $ do
          validUpdates ruleMap updates
            `shouldBe`
            [ Update (75 :| [47, 61, 53, 29])
            , Update (97 :| [61, 53, 29, 13])
            , Update (75 :| [29, 13])
            ]
