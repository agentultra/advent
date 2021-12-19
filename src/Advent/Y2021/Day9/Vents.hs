module Advent.Y2021.Day9.Vents where

import qualified Data.Text as T

import Advent.Grid (Grid (..))
import qualified Advent.Grid as G

type CaveFloor = Grid Int

floorNeighbours :: CaveFloor -> Int -> Int -> [Int]
floorNeighbours flr x = mapMaybe (uncurry (G.get flr)) . neighbours x

neighbours :: Int -> Int -> [(Int, Int)]
neighbours x y = [ (x-1, y)
                 , (x, y - 1)
                 , (x+1, y)
                 , (x, y+1)
                 ]

lowPoints :: CaveFloor -> [(Int, Int)]
lowPoints flr@(Grid w h _) = do
  x <- [0..w-1]
  y <- [0..h-1]
  case G.get flr x y of
    Nothing -> error $ T.pack "WAT"
    Just c ->
      if all (> c) $ floorNeighbours flr x y
      then pure (x, y)
      else []

part1Solution :: NonEmpty (NonEmpty Int) -> Int
part1Solution xs = case G.mkGrid . map toList . toList $ xs of
  Nothing -> error $ T.pack "Bad Input"
  Just g  -> sum . mapMaybe (fmap (+1) . uncurry (G.get g)) . lowPoints $ g
