module Advent.Y2021.Day8.Digits where

data Segment
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  deriving (Eq, Ord, Show)

newtype Digit = Digit { getDigit :: [Segment] }
  deriving (Eq, Ord, Show)

numSegments :: Digit -> Int
numSegments (Digit segments) = length segments

data Display
  = Display
  { signalPatterns :: [Digit]
  , outputValues   :: [Digit]
  }
  deriving (Eq, Show)

part1Solution :: NonEmpty Display -> Int
part1Solution = go 0 . toList
  where
    go :: Int -> [Display] -> Int
    go n [] = n
    go n (d:ds) = go (n + numEasyDigits d) ds

    numEasyDigits :: Display -> Int
    numEasyDigits = foldl' (\acc n -> if easyDigits n then acc + 1 else acc) 0 . map numSegments . outputValues

    easyDigits :: Int -> Bool
    easyDigits x
      | x == 2    = True -- 1
      | x == 4    = True -- 4
      | x == 3    = True -- 7
      | x == 7    = True -- 8
      | otherwise = False
