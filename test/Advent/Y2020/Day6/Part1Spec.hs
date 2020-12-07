module Advent.Y2020.Day6.Part1Spec where

import Data.Attoparsec.Text
import qualified Data.Set as S
import Test.Hspec

import Advent.Y2020.Day6.DeclarationForm
import Advent.Y2020.Day6.Parse
import Advent.Y2020.Day6.Part1

spec :: Spec
spec = do
  describe "Advent - Y2020 - Day6 - Part 1" $ do
    describe "Parse" $ do
      it "can parse a single answer" $ do
        let input = "abcade\n"
        parseOnly answerP input
          `shouldBe`
          Right (S.fromList ['a', 'b', 'c', 'd', 'e'])

      it "can parse a group of answers" $ do
        let input = "a\nb\ncb\n\n"
        let expected
              = Group $ S.singleton 'a'
                :| [ S.singleton 'b'
                   , S.fromList ['c', 'b']
                   ]
        parseOnly groupAnswersP input `shouldBe` Right expected

      it "can parse many groups' answers" $ do
        let input = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb\n\n"
        let expected
              = [ Group (S.fromList ['a', 'b', 'c'] :| [])
                , Group (S.singleton 'a' :| [S.singleton 'b', S.singleton 'c'] )
                , Group (S.fromList ['a', 'b'] :| [S.fromList ['a', 'c']])
                , Group (S.singleton 'a' :| [ S.singleton 'a'
                                            , S.singleton 'a'
                                            , S.singleton 'a'])
                , Group (S.singleton 'b' :| [])
                ]
        parseInput input `shouldBe` Right expected

    describe "groupAnswer" $ do
      it "should return 11 based on the example" $ do
        let input = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb\n\n"
        sum . map groupAnswer <$> parseInput input
          `shouldBe` Right 11
