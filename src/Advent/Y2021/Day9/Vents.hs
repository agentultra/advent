module Advent.Y2021.Day9.Vents where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Advent.Grid (Grid (..))
import qualified Advent.Grid as G

type CaveFloor = Grid Int
type Point = (Int, Int)

floorNeighbours :: CaveFloor -> Point -> [(Point, Int)]
floorNeighbours flr = mapMaybe (\p -> (p,) <$> uncurry (G.get flr) p) . neighbours flr

neighbours :: CaveFloor -> Point -> [Point]
neighbours (Grid w h _) (x, y) = filter inBounds coords
  where
    inBounds (x', y') =
      x' >= 0 && x' < w && y' >= 0 && y' < h
    coords = [ (x-1, y)
             , (x, y - 1)
             , (x+1, y)
             , (x, y+1)
             ]

lowPoints :: CaveFloor -> [Point]
lowPoints flr@(Grid w h _) = do
  x <- [0..w-1]
  y <- [0..h-1]
  case G.get flr x y of
    Nothing -> error $ T.pack "WAT"
    Just c ->
      if all ((> c) . snd) $ floorNeighbours flr (x, y)
      then pure (x, y)
      else []

basinNeighbours :: CaveFloor -> Point -> [Point]
basinNeighbours flr = map fst . filter ((< 9) . snd) . floorNeighbours flr

basin :: CaveFloor -> Point -> Int
basin flr pos = go 1 (S.singleton pos) (basinNeighbours flr pos)
  where
    go :: Int -> Set Point -> [Point] -> Int
    go size _ [] = size
    go size visited (p:frontier) =
      if p `S.member` visited
      then go size visited frontier
      else let ns = filter (not . (`S.member` visited)) . basinNeighbours flr $ p
           in go (size + 1) (p `S.insert` visited) (ns ++ frontier)

part1Solution :: NonEmpty (NonEmpty Int) -> Int
part1Solution xs = case G.mkGrid . map toList . toList $ xs of
  Nothing -> error $ T.pack "Bad Input"
  Just g  -> sum . mapMaybe (fmap (+1) . uncurry (G.get g)) . lowPoints $ g

part2Solution :: NonEmpty (NonEmpty Int) -> Int
part2Solution xs = case G.mkGrid . map toList . toList $ xs of
  Nothing -> error $ T.pack "Bad Input"
  Just g  -> multiply . take 3 . reverse . sort . map (basin g) . lowPoints $ g
  where
    multiply = foldl' (*) 1
