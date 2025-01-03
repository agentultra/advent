module Prelude
  ( module Relude
  , module Relude.Extra.Enum
  , chunks
  , eitherToMaybe
  , lookupKey
  , maybeTrue
  , orElse
  , pairs
  , readDecimals
  , readInt
  , readSignedDecimals
  , readRationals
  , readDoubles
  , windows
  )
  where

import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Relude
import Relude.Extra.Enum

-- | Read a comma-separated list of values
readValuesAs :: T.Reader a -> Text -> Either String [a]
readValuesAs rdr txt = map fst <$> traverse rdr (T.split (== ',') txt)

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

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x

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
    go (x:y:xs') = (x, y) : go xs'

maybeTrue :: (a -> Bool) -> a -> Maybe a
maybeTrue p x
  | p x       = Just x
  | otherwise = Nothing

orElse :: Text -> Either b a -> a
orElse err (Left _) = error err
orElse _ (Right result) = result

-- | Like 'Data.List.lookup' but find the key based on the value
lookupKey :: Eq a => a -> [(b, a)] -> Maybe b
lookupKey _ [] = Nothing
lookupKey k (x:xs)
  | k == snd x = Just $ fst x
  | otherwise  = lookupKey k xs

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

-- | Like 'transpose' from Data.List except this will discard elements
-- less than 'n'.
transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

windows :: Int -> [a] -> [[a]]
windows n = transpose' . take n . tails
