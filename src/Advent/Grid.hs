module Advent.Grid where

import Data.List.Split (chunksOf)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude hiding (get)

data Grid a
  = Grid
  { _gridWidth  :: Int
  , _gridHeight :: Int
  , _gridCells  :: Vector a
  }
  deriving (Eq, Show)

mkGrid :: [[a]] -> Maybe (Grid a)
mkGrid ps = do
  h <- maybeTrue (> 0) $ length ps
  w <- length <$> viaNonEmpty head ps
  if w < 1
    then Nothing
    else do
    let b = V.fromList $ concat ps
    pure $ Grid w h b

rows :: Grid a -> [Vector a]
rows (Grid w _ cells) =
  [ V.slice i w cells | i <- [0,w..(length cells - w)] ]

cols :: Grid a -> [Vector a]
cols (Grid w _ cells) =
  let rs = chunksOf w . V.toList $ cells
  in map V.fromList . transpose $ rs

-- | Return a value within the bounds of the Grid
get :: Grid a -> Int -> Int -> Maybe a
get (Grid w h cells) x y
  | x >= 0 && x < w && y >= 0 && y < h = cells V.!? (y * w + x)
  | otherwise = Nothing

set :: Grid a -> Int -> Int -> a -> Maybe (Grid a)
set (Grid w h cells) x y v
  | x >= 0 && x < w && y >= 0 && y < h =
    Just . Grid w h $ cells V.// [(y * w + x, v)]
  | otherwise = Nothing

-- | Return a list of the valid indices
indices :: Grid a -> [(Int, Int)]
indices g =
  [ (x, y)
  | y <- [0..(_gridHeight g)-1]
  , x <- [0..(_gridWidth g)-1]
  ]

toList :: Grid a -> [[a]]
toList g =
  [ catMaybes [ get g x y | x <- [0..(_gridWidth g)-1] ]
  | y <- [0..(_gridHeight g)-1]
  ]
