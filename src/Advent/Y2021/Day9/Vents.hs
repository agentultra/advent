module Advent.Y2021.Day9.Vents where

import qualified Data.Text as T

import Advent.Grid (Grid (..))
import qualified Advent.Grid as G

type CaveFloor = Grid Int

neighbours :: CaveFloor -> Int -> Int -> [Int]
neighbours flr x y =
  catMaybes [ G.get flr (x-1) y
            , G.get flr x (y-1)
            , G.get flr (x+1) y
            , G.get flr x (y+1)
            ]

lowPoints :: CaveFloor -> [Int]
lowPoints flr@(Grid w h _) = do
  x <- [0..w-1]
  y <- [0..h-1]
  case G.get flr x y of
    Nothing -> error $ T.pack "WAT"
    Just c ->
      if all (> c) $ neighbours flr x y
      then pure c
      else []

part1Solution :: NonEmpty (NonEmpty Int) -> Int
part1Solution xs = case G.mkGrid . map toList . toList $ xs of
  Nothing -> error $ T.pack "Bad Input"
  Just g  -> sum . map (+1) . lowPoints $ g
