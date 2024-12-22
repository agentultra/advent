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

data RegionSearch
  = RegionSearch
  { _visited        :: Map (Int, Int) Bool
  , _grid           :: Grid Char
  , _neighbours     :: (Int, Int) -> Grid Char -> [Maybe ((Int, Int), Char)]
  , _perimeterSum   :: [Maybe ((Int, Int), Char)] -> Int
  , _perimeterTotal :: Int -> Int
  , _totalSum       :: Int
  }

makeLenses ''RegionSearch

search :: State RegionSearch Int
search = do
  vs <- use visited
  if allDone vs
    then use totalSum >>= pure
    else do
    let x = fromMaybe (error "WAAAAT") $ pick vs
    cost <- floodFill x
    totalSum += cost
    search

floodFill :: (Int, Int) -> State RegionSearch Int
floodFill x = go [x] [x] 0 0
  where
    go :: [(Int, Int)] -> [(Int, Int)] -> Int -> Int -> State RegionSearch Int
    go [] _ area perimeter = do
      pt <- use perimeterTotal
      pure $ area * pt perimeter
    go (y:ys) internal area perimeter = do
      g <- use grid
      ps <- use perimeterSum
      neighboursF <- use neighbours
      let ns = neighboursF y g
          c = fromMaybe (error "couldn't get cell") $ Grid.getAt g y
          perimeter' = perimeter + ps (filter (isPerimeterCell c) ns)
          area' = area + 1
          ncs = catMaybes ns
          stack' = map fst . filter (unvisitedInternalCell c internal) $ ncs
      visited %= (\v -> foldl' (updateVisited c) v (Just (y, c) : ns))
      go (stack' ++ ys) (internal ++ map fst ncs) area' perimeter'

    isPerimeterCell :: Char -> Maybe ((Int, Int), Char) -> Bool
    isPerimeterCell _ Nothing = True
    isPerimeterCell c (Just (_, y)) = c /= y

    unvisitedInternalCell :: Char -> [(Int, Int)] -> ((Int, Int), Char) -> Bool
    unvisitedInternalCell c internal (p, x) = p `notElem` internal && c == x

    updateVisited :: Char -> Map (Int, Int) Bool -> Maybe ((Int, Int), Char) -> Map (Int, Int) Bool
    updateVisited _ v Nothing                = v
    updateVisited c v (Just (n, m)) | c == m = Map.update (const $ Just True) n v
    updateVisited _ v _                      = v
