module Advent.Y2020.Day4.Part1Spec where

import Data.Attoparsec.Text
import qualified Data.Map as M
import Test.Hspec

import Advent.Y2020.Day4.Part1
import Advent.Y2020.Day4.Passport
import Advent.Y2020.Day4.Parse

spec :: Spec
spec = do
  describe "Advent - Y2020 - Day4 - Part 1" $ do
    describe "Parse" $ do
      it "can parse passport keys" $ do
        let input = "byr,iyr,eyr,hgt,hcl,ecl,pid,cid"
        parseOnly (passportKeyP `sepBy` char ',') input
          `shouldBe`
          Right [BYR, IYR, EYR, HGT, HCL, ECL, PID, CID]

      it "can parse a key/value pair ending with a newline" $ do
        let input = "byr:1982\n"
        parseOnly keyValueP input
          `shouldBe`
          Right (BYR, "1982")

      it "can parse a key/value pair ending with a space" $ do
        let input = "byr:1982 "
        parseOnly keyValueP input
          `shouldBe`
          Right (BYR, "1982")

      it "can parse a passport entry" $ do
        let input = "iyr:1956\nhcl:2385ac\nhgt:167in\necl:#9b82e8\neyr:2003\ncid:214 byr:2012 pid:483285062\n\n"
        parseOnly passportP input
          `shouldBe`
          (Right $ M.fromList [ (BYR, "2012")
                              , (IYR, "1956")
                              , (ECL, "#9b82e8")
                              , (EYR, "2003")
                              , (CID, "214")
                              , (HGT, "167in")
                              , (HCL, "2385ac")
                              , (PID, "483285062")
                              ])

      it "can parse several passport entries" $ do
        let input = "iyr:1956\nhcl:2385ac\nhgt:167in\necl:#9b82e8\neyr:2003\ncid:214 byr:2012 pid:483285062\n\niyr:1956\nhcl:2385ac\nhgt:167in\necl:#9b82e8\neyr:2003\ncid:214 byr:2012 pid:483285062\n\n"
        let expected = M.fromList [ (BYR, "2012")
                                  , (IYR, "1956")
                                  , (ECL, "#9b82e8")
                                  , (EYR, "2003")
                                  , (CID, "214")
                                  , (HGT, "167in")
                                  , (HCL, "2385ac")
                                  , (PID, "483285062")
                                  ]
        parsePassports input
          `shouldBe`
          Right [expected, expected]
