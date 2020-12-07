module Advent.Y2020.Day5.SeatCode where

import qualified Data.Text as T

data Code = F | B | L | R
  deriving (Bounded, Enum, Eq, Show)

codeToChar :: Code -> Char
codeToChar = \case
  F -> 'F'
  B -> 'B'
  L -> 'L'
  R -> 'R'

textToCode :: Char -> Maybe Code
textToCode = inverseMap codeToChar

data SeatCode = SeatCode Code Code Code Code Code Code Code Code Code Code
  deriving(Eq, Show)

-- | There are some further requirements on valid SeatCode's but I'm
-- going to assume the input is well-formed for this puzzle..
seatCodeFromText :: Text -> Maybe SeatCode
seatCodeFromText input
  | T.length input /= 10 = Nothing
  | otherwise = case traverse textToCode $ T.unpack input of
      Nothing -> Nothing
      Just [a, b, c, d, e, f, g, h, i, j] ->
        -- We'd want to check here, somehow, that the first 7
        -- parameters are either an F or a B; the last three should be
        -- either an L or an R.
        Just $ SeatCode a b c d e f g h i j

-- These are almost exactly the same. I feel like there ought to be a Fix (,)
-- algebra.

findRow :: SeatCode -> Maybe Int
findRow (SeatCode a b c d e f g _ _ _) = go (0, 127) [a, b, c, d, e, f, g]
  where
    go :: (Int, Int) -> [Code] -> Maybe Int
    go _ [] = Nothing
    go r [F] = Just $ uncurry min r
    go r [B] = Just $ uncurry max r
    go (lo, hi) (F:cs) = go (lo, lo + ((hi - lo) `div` 2)) cs
    go (lo, hi) (B:cs) = go (lo + (((hi - lo) `div` 2) + 1), hi) cs

findColumn :: SeatCode -> Maybe Int
findColumn (SeatCode _ _ _ _ __ _ _ a b c) = go (0, 7) [a, b, c]
  where
    go :: (Int, Int) -> [Code] -> Maybe Int
    go _ [] = Nothing
    go r [L] = Just $ uncurry min r
    go r [R] = Just $ uncurry max r
    go (lo, hi) (L:cs) = go (lo, lo + ((hi - lo) `div` 2)) cs
    go (lo, hi) (R:cs) = go (lo + (((hi - lo) `div` 2) + 1), hi) cs

findSeatId :: SeatCode -> Maybe Int
findSeatId code = do
  row <- findRow code
  col <- findColumn code
  pure $ row * 8 + col
