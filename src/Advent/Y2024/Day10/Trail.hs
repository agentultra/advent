{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Y2024.Day10.Trail where

import Advent.Grid (Grid, (.+.))
import qualified Advent.Grid as Grid
import qualified Data.List as List

newtype TrailMap = TrailMap { getTrailMap :: Grid Int }
  deriving (Eq, Show)

newtype Trailhead = Trailhead (Int, Int)
  deriving (Eq, Show)

getTrailheads :: TrailMap -> [Trailhead]
getTrailheads (TrailMap trailMap)
  = map Trailhead
  . Grid.findIndices (== 0)
  $ trailMap

findTrails :: Trailhead -> TrailMap -> Int
findTrails (Trailhead trailHead) trailMap
  = go trailMap 0 (validDirections trailHead trailMap)
  where
    go :: TrailMap -> Int -> [(Int, Int)] -> Int
    go _ count [] = count
    go tm count (x:xs) =
      case Grid.getAt trailMap.getTrailMap x of
        Nothing -> go trailMap count xs
        Just v | v == 9 -> go trailMap (count + 1) xs
        Just _ | otherwise ->
                 go tm count . List.nub $ xs ++ (validDirections x trailMap)

findDistinctTrails :: Trailhead -> TrailMap -> Int
findDistinctTrails (Trailhead trailHead) trailMap
  = go trailMap 0 (validDirections trailHead trailMap)
  where
    go :: TrailMap -> Int -> [(Int, Int)] -> Int
    go _ count [] = count
    go tm count (x:xs) =
      case Grid.getAt trailMap.getTrailMap x of
        Nothing -> go trailMap count xs
        Just v | v == 9 -> go trailMap (count + 1) xs
        Just _ | otherwise ->
                 go tm count $ xs ++ (validDirections x trailMap)

validDirections :: (Int, Int) -> TrailMap -> [(Int, Int)]
validDirections p tm = do
  d <- Grid.directions
  v <- maybeToList $ Grid.getAt tm.getTrailMap p
  let offsetP = p .+. Grid.offset d
  v' <- maybeToList $ Grid.getAt tm.getTrailMap offsetP

  guard $ v + 1 == v'

  pure offsetP
