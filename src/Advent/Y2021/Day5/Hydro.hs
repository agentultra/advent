{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Advent.Y2021.Day5.Hydro where

import Data.Set (Set)
import qualified Data.Set as S

newtype Vec2 = Vec2 { getVec2 :: (Int, Int) }
  deriving (Eq, Hashable, Ord, Show)

data Line
  = Line
  { _x1 :: Int
  , _y1 :: Int
  , _x2 :: Int
  , _y2 :: Int
  }
  deriving (Eq, Show)

orthogonal :: Line -> Set Vec2
orthogonal (Line x1 y1 x2 y2)
  | x1 == x2  =
    let miny = min y1 y2
        maxy = max y1 y2
    in S.fromList [ Vec2 (x1, y) | y <- [miny..maxy] ]
  | y1 == y2  =
    let minx = min x1 x2
        maxx = max x1 x2
    in S.fromList [ Vec2 (x, y1) | x <- [minx..maxx] ]
  | otherwise = S.empty

intersections :: (Eq a, Ord a) => NonEmpty (Set a) -> Set a
intersections (x :| xs) = go S.empty x xs
  where
    go :: (Eq a, Ord a) => Set a -> Set a -> [Set a] -> Set a
    go acc _ [] = acc
    go acc used (y:ys) =
      let vs = S.intersection y used
      in if S.null vs
         then go acc (S.union y used) ys
         else go (S.union vs acc) (S.union y used) ys

part1Solution :: NonEmpty Line -> Int
part1Solution = S.size . intersections . fmap orthogonal
