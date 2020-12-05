module Prelude
  ( module Relude
  , module Relude.Extra.Enum
  , pairs
  , readDecimals
  , readInt
  , readSignedDecimals
  , readRationals
  , readDoubles
  )
  where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import Relude
import Relude.Extra.Enum

-- | Read a comma-separated list of values
readValuesAs :: T.Reader a -> Text -> Either String [a]
readValuesAs reader txt = map fst <$> traverse reader (T.split (== ',') txt)

-- | Parse a comma-separated list of @Int@
readDecimals :: Text -> Either String [Int]
readDecimals = readValuesAs T.decimal

-- | Parse a comma-separated list of signed @Int@
readSignedDecimals :: Text -> Either String [Int]
readSignedDecimals = readValuesAs $ T.signed T.decimal

-- | Parse a list of @Rational@
readRationals :: Text -> Either String [Rational]
readRationals = readValuesAs T.rational

-- | Parse a list of @Double@
readDoubles :: Text -> Either String [Double]
readDoubles = readValuesAs T.double

readInt :: Text -> Either String Int
readInt = read <$> T.decimal
  where
    read (Right (i, _)) = Right i
    read (Left err) = Left err

-- | Break a list into consecutive pairs with a possible remainder for
-- uneven lists
pairs :: [a] -> ([(a, a)], Maybe a)
pairs xs
  | null xs = ([], Nothing)
  | even $ length xs = (go xs, Nothing)
  | otherwise = bimap go listToMaybe $ splitAt (length xs - 1) xs
  where
    go :: [a] -> [(a, a)]
    go [] = []
    go [_] = []
    go (x:y:xs) = (x, y) : go xs
