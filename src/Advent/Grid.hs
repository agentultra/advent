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

gridRows :: Grid a -> [Vector a]
gridRows (Grid w _ cells) =
  [ V.slice i w cells | i <- [0,w..(length cells - w)] ]

gridCols :: Grid a -> [Vector a]
gridCols (Grid w _ cells) =
  let rows = chunksOf w . V.toList $ cells
  in map V.fromList . transpose $ rows
