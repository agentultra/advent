module Advent.Y2020.Day4.Passport where

import Control.Error
import qualified Data.Map as M

newtype Year = Year Text
  deriving (Eq, Ord, Show)

newtype Height = Height Text
  deriving (Eq, Show)

newtype HairColor = HairColor Text
  deriving (Eq, Show)

newtype EyeColor = EyeColor Text
  deriving (Eq, Show)

newtype PassportID = PassportID Text
  deriving (Eq, Show)

newtype CountryID = CountryID Text
  deriving (Eq, Show)

data Passport
  = Passport
  { birthYear      :: Year
  , issueYear      :: Year
  , expirationYear :: Year
  , height         :: Height
  , hairColor      :: HairColor
  , eyeColor       :: EyeColor
  , passportId     :: PassportID
  , countryId      :: Maybe CountryID
  }
  deriving (Eq, Show)

data PassportKey
  = BYR
  | IYR
  | EYR
  | HGT
  | HCL
  | ECL
  | PID
  | CID
  deriving (Bounded, Enum, Eq, Ord, Show)

keyToText :: PassportKey -> Text
keyToText = \case
  BYR -> "byr"
  IYR -> "iyr"
  EYR -> "eyr"
  HGT -> "hgt"
  HCL -> "hcl"
  ECL -> "ecl"
  PID -> "pid"
  CID -> "cid"

keyFromText :: Text -> Maybe PassportKey
keyFromText = inverseMap keyToText

fromMap :: Map PassportKey Text -> Either String Passport
fromMap passportData = do
  byr <- note "Missing byr" $ Year <$> M.lookup BYR passportData
  iyr <- note "Missing iyr" $ Year <$> M.lookup IYR passportData
  eyr <- note "Missing eyr" $ Year <$> M.lookup EYR passportData
  hgt <- note "Missing hgt" $ Height <$> M.lookup HGT passportData
  hcl <- note "Missing hcl" $ HairColor <$> M.lookup HCL passportData
  ecl <- note "Missing ecl" $ EyeColor <$> M.lookup ECL passportData
  pid <- note "Missing pid" $ PassportID <$> M.lookup PID passportData
  let cid = CountryID <$> M.lookup CID passportData
  pure $ Passport byr iyr eyr hgt hcl ecl pid cid
