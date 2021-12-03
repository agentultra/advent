module Advent.Y2021.Day3.Diagnostic where

import Data.Char
import Data.List hiding (head)
import qualified Data.Text as T

mostCommon :: String -> Maybe Char
mostCommon xs =
  case group . sort $ xs of
    [] -> Nothing
    xs' -> viaNonEmpty head . maximumBy (comparing length) $ xs'

-- | Convert a string of /binary digits/.
binToInt :: String -> Int
binToInt = foldl' (\acc c -> acc * 2 + digitToInt c) 0

complement :: String -> Maybe String
complement = sequence . fmap compl
  where
    compl '1' = Just '0'
    compl '0' = Just '1'
    compl _   = Nothing

part1Solution :: NonEmpty Text -> Maybe Int
part1Solution bins = do
  epsilon <- sequence . fmap mostCommon . transpose . fmap T.unpack . toList $ bins
  gamma <- complement epsilon
  let epsilon' = binToInt epsilon
      gamma' = binToInt gamma
  pure $ epsilon' * gamma'
