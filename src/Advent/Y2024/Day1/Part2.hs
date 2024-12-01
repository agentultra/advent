{-# LANGUAGE OverloadedStrings #-}

module Advent.Y2024.Day1.Part2 where

import Advent.Y2024.Day1.Input
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text.IO as T

solution :: IO ()
solution = do
  raw <- T.readFile "data/2024/Day1.txt"
  let ns = orElse "Could not parse input" $ getInput raw
      (ls, hist)
        = bimap Set.fromList histogram
        . unzip $ ns
      answer = foldl' (\a x -> a + x * fromHist x hist) 0 ls
  print answer

histogram :: Ord a => [a] -> Map a Int
histogram = foldl' count mempty
  where
    count :: Ord a => Map a Int -> a -> Map a Int
    count hist x = Map.alter alterCount x hist

    alterCount :: Maybe Int -> Maybe Int
    alterCount Nothing  = Just 1
    alterCount (Just x) = Just $ x + 1

fromHist :: Int -> Map Int Int -> Int
fromHist x h = Maybe.fromMaybe 0 $ x `Map.lookup` h

orElse :: Text -> Either b a -> a
orElse err (Left _) = error err
orElse _ (Right result) = result
