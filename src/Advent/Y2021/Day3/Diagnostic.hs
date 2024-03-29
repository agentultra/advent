{-# LANGUAGE DeriveFunctor#-}

module Advent.Y2021.Day3.Diagnostic where

import Data.Char
import Data.List hiding (head)
import qualified Data.Text as T
import Prelude hiding (tail)

data Commonality a
  = Most a
  -- ^ Will contain the exclusively most-common element
  | Same
  -- ^ All elements are equally common
  | None
  -- ^ There are no common elements
  deriving (Eq, Functor, Show)

instance Applicative Commonality where
  pure = Most
  Most f <*> Most a = Most $ f a
  Most _ <*> Same = Same
  Most _ <*> None = None
  Same <*> _ = Same
  None <*> _ = None

instance Monad Commonality where
  Most a >>= f = f a
  Same   >>= _ = Same
  None   >>= _ = None

maybeCommonality :: Commonality a -> Maybe a
maybeCommonality (Most a) = Just a
maybeCommonality _        = Nothing

commonalityFromMaybe :: Maybe a -> Commonality a
commonalityFromMaybe (Just x) = Most x
commonalityFromMaybe _        = None

mostCommon :: String -> Commonality Char
mostCommon xs =
  case group . sort $ xs of
    [] -> None
    xs' ->
      case withLength <$> xs' of
        [] -> None
        [(_, x)] -> commonalityFromMaybe . viaNonEmpty head $ x
        withLengths ->
          if allSame . map fst $ withLengths
          then Same
          else case viaNonEmpty head . snd .  maximumBy (comparing fst) $ withLengths of
                 Nothing -> error $ T.pack "Should never happen..."
                 Just x -> Most x
  where
    withLength x = (length x, x)
    allSame = and . (zipWith (==) <*> tail)

-- | Convert a string of /binary digits/.
binToInt :: String -> Int
binToInt = foldl' (\acc c -> acc * 2 + digitToInt c) 0

complement :: String -> Maybe String
complement = mapM compl
  where
    compl '1' = Just '0'
    compl '0' = Just '1'
    compl _   = Nothing

oxygenRating :: [String] -> Maybe String
oxygenRating xs = go xs 0
  where
    go :: [String] -> Int -> Maybe String
    go [] _  = Nothing
    go [x] _ = Just x
    go xs' i  = do
      let commons = map mostCommon . transpose $ xs'
      case commons !! i of
        Most x -> go (filter (\n -> n !! i == x) xs') (i + 1)
        Same   -> go (filter (\n -> n !! i == '1') xs') (i + 1)
        None   -> error $ T.pack "Invariant violated"

scrubberRating :: [String] -> Maybe String
scrubberRating xs = go xs 0
  where
    go :: [String] -> Int -> Maybe String
    go [] _  = Nothing
    go [x] _ = Just x
    go xs' i  = do
      let commons = map mostCommon . transpose $ xs'
      case commons !! i of
        -- We want the _least common_ in this case and exploit the
        -- fact that we know we're only dealing with '0' and '1'
        Most '1' -> go (filter (\n -> n !! i == '0') xs') (i + 1)
        Most '0' -> go (filter (\n -> n !! i == '1') xs') (i + 1)
        Most _   -> error $ T.pack "Invariant violated"
        Same     -> go (filter (\n -> n !! i == '0') xs') (i + 1)
        None     -> error $ T.pack "Invariant violated"

part1Solution :: NonEmpty Text -> Maybe Int
part1Solution bins = do
  epsilon <- mapM (maybeCommonality . mostCommon) . transpose . fmap T.unpack . toList $ bins
  gamma <- complement epsilon
  let epsilon' = binToInt epsilon
      gamma' = binToInt gamma
  pure $ epsilon' * gamma'

part2Solution :: NonEmpty Text -> Maybe Int
part2Solution bins = do
  let ns = T.unpack <$> toList bins
  oxygen  <- binToInt <$> oxygenRating ns
  scrubber <- binToInt <$> scrubberRating ns
  pure $ oxygen * scrubber
