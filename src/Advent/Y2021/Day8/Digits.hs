module Advent.Y2021.Day8.Digits where

import Data.List hiding (sum)

import qualified Data.Set as S
import qualified Data.Text as T

data Segment
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  deriving (Eq, Ord, Show)

newtype Digit = Digit { getDigit :: Set Segment }
  deriving (Eq, Ord, Show)

numSegments :: Digit -> Int
numSegments (Digit segments) = length segments

data Display
  = Display
  { signalPatterns :: [Digit]
  , outputValues   :: [Digit]
  }
  deriving (Eq, Show)

easyDigits :: Int -> Bool
easyDigits x
  | x == 2    = True -- 1
  | x == 4    = True -- 4
  | x == 3    = True -- 7
  | x == 7    = True -- 8
  | otherwise = False

deduceDigits :: [Digit] -> [(Digit, Int)]
deduceDigits ds =
  let (easy, rest) = partition (easyDigits . numSegments) ds
  in go (initDigitMap easy) rest
  where
    initDigitMap :: [Digit] -> [(Digit, Int)]
    initDigitMap ds' = do
      d <- ds'
      case numSegments d of
        2 -> pure (d, 1)
        4 -> pure (d, 4)
        3 -> pure (d, 7)
        7 -> pure (d, 8)
        _ -> error $ T.pack "OOF"

    go :: [(Digit, Int)] -> [Digit] -> [(Digit, Int)]
    go digitMap [] = digitMap
    go digitMap (d:ds')
      | numSegments d == 6 =
        case lookupKey 4 digitMap of
          Nothing -> error $ T.pack "WAT"
          Just n4 ->
            if length (getDigit n4 `S.intersection` getDigit d) == 4
            then go ((d, 9) : digitMap) ds'
            else case lookupKey 7 digitMap of
              Nothing -> error $ T.pack "WAT2"
              Just n7 ->
                if length (getDigit n7 `S.intersection` getDigit d) == 3
                then go ((d, 0) : digitMap) ds'
                else go ((d, 6) : digitMap) ds'
      | numSegments d == 5 =
        case lookupKey 1 digitMap of
          Nothing -> error $ T.pack "WAT3"
          Just n1 ->
            if length (getDigit n1 `S.intersection` getDigit d) == 2
            then go ((d, 3) : digitMap) ds'
            else case lookupKey 4 digitMap of
              Nothing -> error $ T.pack "WAT4"
              Just n4 ->
                if length (getDigit n4 `S.intersection` getDigit d) == 3
                then go ((d, 5) : digitMap) ds'
                else go ((d, 2) : digitMap) ds'
      | otherwise = go digitMap ds'

numFrom :: Display -> Maybe Int
numFrom (Display sigs outs) =
  let mappings = deduceDigits sigs
  in fmap digitToInt . mapM (`lookup` mappings) $ outs
  where
    digitToInt :: [Int] -> Int
    digitToInt ds = sum [ n * (10 ^ i) | (n, i) <- zip @Int @Int (reverse ds) [0..] ]

part1Solution :: NonEmpty Display -> Int
part1Solution = go 0 . toList
  where
    go :: Int -> [Display] -> Int
    go n [] = n
    go n (d:ds) = go (n + numEasyDigits d) ds

    numEasyDigits :: Display -> Int
    numEasyDigits = foldl' (\acc n -> if easyDigits n then acc + 1 else acc) 0 . map numSegments . outputValues

part2Solution :: NonEmpty Display -> Int
part2Solution = go 0 . toList
  where
    go :: Int -> [Display] -> Int
    go n [] = n
    go n (d:ds) = case numFrom d of
      Nothing -> error $ T.pack "WHOOPS, BAD INPUT"
      Just n' -> go (n + n') ds
