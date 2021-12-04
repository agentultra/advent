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
                 Nothing -> error "Should never happen..."
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

part1Solution :: NonEmpty Text -> Maybe Int
part1Solution bins = do
  epsilon <- mapM (maybeCommonality . mostCommon) . transpose . fmap T.unpack . toList $ bins
  gamma <- complement epsilon
  let epsilon' = binToInt epsilon
      gamma' = binToInt gamma
  pure $ epsilon' * gamma'
