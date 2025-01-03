{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Advent.Y2024.Day12.Garden where

import Advent.Grid (Direction, Grid, (.+.))
import qualified Advent.Grid as Grid
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import Lens.Micro.Mtl
import Lens.Micro.TH

mkVisited :: Grid Char -> Map (Int, Int) Bool
mkVisited = Map.fromList . flip zip (repeat False) . Grid.indices

pick :: Map (Int, Int) Bool -> Maybe (Int, Int)
pick = listToMaybe . map fst . filter (not . snd) . Map.assocs

allDone :: Map (Int, Int) Bool -> Bool
allDone = and . Map.elems

data RegionSearch a
  = RegionSearch
  { _visited        :: Map (Int, Int) Bool
  , _grid           :: Grid Char
  , _neighbours     :: (Int, Int) -> Grid Char -> [((Int, Int), Maybe Char)]
  , _perimeterSum   :: Monoid a => (Int, Int) -> [((Int, Int), Maybe Char)] -> a
  , _perimeterTotal :: Monoid a => a -> Int
  , _totalSum       :: Int
  }

makeLenses ''RegionSearch

search :: (Monoid a, Show a) => State (RegionSearch a) Int
search = do
  vs <- use visited
  if allDone vs
    then use totalSum >>= pure
    else do
    let x = fromMaybe (error "WAAAAT") $ pick vs
    cost <- floodFill x
    totalSum += cost
    search

floodFill :: (Monoid a, Show a) => (Int, Int) -> State (RegionSearch a) Int
floodFill x = go [x] [x] 0 mempty
  where
    go :: (Monoid a, Show a) => [(Int, Int)] -> [(Int, Int)] -> Int -> a -> State (RegionSearch a) Int
    go [] _ area perimeter = do
      pt <- use perimeterTotal
      pure $ area * pt perimeter
    go (y:ys) internal area perimeter = do
      g <- use grid
      ps <- use perimeterSum
      neighboursF <- use neighbours
      let ns = neighboursF y g
          c = fromMaybe (error "couldn't get cell") $ Grid.getAt g y
          perimeter' = perimeter <> ps y (filter (isPerimeterCell c) ns)
          area' = area + 1
          stack' = map fst . filter (unvisitedInternalCell c internal) $ ns
      visited %= (\v -> foldl' (updateVisited c) v ((y, Just c) : ns))
      go (stack' ++ ys) (internal ++ stack') area' perimeter'

    isPerimeterCell :: Char -> ((Int, Int), Maybe Char) -> Bool
    isPerimeterCell _ (_, Nothing) = True
    isPerimeterCell c (_, Just y) = c /= y

    unvisitedInternalCell :: Char -> [(Int, Int)] -> ((Int, Int), Maybe Char) -> Bool
    unvisitedInternalCell _ _ (_, Nothing)       = False
    unvisitedInternalCell c internal (p, Just x) = p `notElem` internal && c == x

    updateVisited :: Char -> Map (Int, Int) Bool -> ((Int, Int), Maybe Char) -> Map (Int, Int) Bool
    updateVisited _ v (_, Nothing)           = v
    updateVisited c v ((n, Just m)) | c == m = Map.update (const $ Just True) n v
    updateVisited _ v _                      = v
