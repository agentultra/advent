module Advent.Grid where

import Data.List.Split (chunksOf)
import Data.Vector (Vector)
import qualified Data.Vector as V

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
get (Grid w _ cells) x y = cells V.!? (y * w + x)
