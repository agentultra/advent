module Advent.Y2020.Day4.Passport where

import Control.Error
import Data.Char
import qualified Data.Map as M
import qualified Data.Text as T

newtype Year = Year Int
  deriving (Eq, Ord, Show)

data Height = Height Int Unit
  deriving (Eq, Show)

newtype HairColor = HairColor Text
  deriving (Eq, Show)

newtype PassportID = PassportID Text
  deriving (Eq, Show)

newtype CountryID = CountryID Text
  deriving (Eq, Show)

data Passport byr iyr exy hgt hcl ecl pid cid
  = Passport
  { birthYear      :: byr
  , issueYear      :: iyr
  , expirationYear :: exy
  , height         :: hgt
  , hairColor      :: hcl
  , eyeColor       :: ecl
  , passportId     :: pid
  , countryId      :: Maybe cid
  }
  deriving (Eq, Show)

type TextPassport
  = Passport
    Text
    Text
    Text
    Text
    Text
    Text
    Text
    Text

type ValidPassport
  = Passport
    Year
    Year
    Year
    Height
    HairColor
    EyeColor
    PassportID
    CountryID

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

data Unit = Cm | In
  deriving (Bounded, Enum, Eq, Ord, Show)

unitToText :: Unit -> Text
unitToText = \case
  Cm -> "cm"
  In -> "in"

unitFromText :: Text -> Maybe Unit
unitFromText = inverseMap unitToText

heightFromText :: Text -> Maybe Height
heightFromText input = do
  (h, unitRaw) <- headMay . reads @Int $ T.unpack input
  unit <- unitFromText $ T.pack unitRaw
  case unit of
    Cm | h >= 150 && h <= 193 -> Just $ Height h unit
    In | h >= 59 && h <= 76   -> Just $ Height h unit
    _                         -> Nothing

hairColorFromText :: Text -> Maybe HairColor
hairColorFromText input = do
  (x, xs) <- T.uncons input
  if x == '#' && T.all isHexDigit xs
    then Just $ HairColor input
    else Nothing

data EyeColor
  = Amber
  | Blue
  | Brown
  | Gray
  | Green
  | Hazel
  | Other
  deriving (Bounded, Enum, Eq, Ord, Show)

eyeColorToText :: EyeColor -> Text
eyeColorToText = \case
  Amber -> "amb"
  Blue  -> "blu"
  Brown -> "brn"
  Gray  -> "gry"
  Green -> "grn"
  Hazel -> "hzl"
  Other -> "oth"

eyeColorFromText :: Text -> Maybe EyeColor
eyeColorFromText = inverseMap eyeColorToText

passportIdFromText :: Text -> Maybe PassportID
passportIdFromText input =
  if T.length input == 9 && T.all isDigit input
  then Just $ PassportID input
  else Nothing

yearFromText :: (Int, Int) -> Text -> Maybe Year
yearFromText (min', max') input =
  case eitherToMaybe $ readInt input of
    Nothing -> Nothing
    Just yr
      | yr >= min' && yr <= max' -> Just $ Year yr
      | otherwise                -> Nothing

fromMap :: Map PassportKey Text -> Either String TextPassport
fromMap passportData = do
  byr <- note "Missing byr" $ M.lookup BYR passportData
  iyr <- note "Missing iyr" $ M.lookup IYR passportData
  eyr <- note "Missing eyr" $ M.lookup EYR passportData
  hgt <- note "Missing hgt" $ M.lookup HGT passportData
  hcl <- note "Missing hcl" $ M.lookup HCL passportData
  ecl <- note "Missing ecl" $ M.lookup ECL passportData
  pid <- note "Missing pid" $ M.lookup PID passportData
  let cid = M.lookup CID passportData
  pure $ Passport byr iyr eyr hgt hcl ecl pid cid

validFromMap :: Map PassportKey Text -> Either String ValidPassport
validFromMap passportData = do
  byr <- note "Missing byr" $ yearFromText (1920, 2002) =<< M.lookup BYR passportData
  iyr <- note "Missing iyr" $ yearFromText (2010, 2020) =<< M.lookup IYR passportData
  eyr <- note "Missing eyr" $ yearFromText (2020, 2030) =<< M.lookup EYR passportData
  hgt <- note "Missing hgt" $ heightFromText =<< M.lookup HGT passportData
  hcl <- note "Missing hcl" $ hairColorFromText =<< M.lookup HCL passportData
  ecl <- note "Missing ecl" $ eyeColorFromText =<< M.lookup ECL passportData
  pid <- note "Missing pid" $ passportIdFromText =<< M.lookup PID passportData
  let cid = CountryID <$> M.lookup CID passportData
  pure $ Passport byr iyr eyr hgt hcl ecl pid cid
