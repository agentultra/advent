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

instance Functor Grid where
  f `fmap` (Grid w h cells) = Grid w h (f `fmap` cells)

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

getAt :: Grid a -> (Int, Int) -> Maybe a
getAt g (x, y) = get g x y

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

-- | Find the index of the element
findIndex :: (a -> Bool) -> Grid a -> Maybe (Int, Int)
findIndex p (Grid w _ cells) = do
  m <- V.findIndex p cells
  pure (m `rem` w, m `div` w)

findIndices :: (a -> Bool) -> Grid a -> [(Int, Int)]
findIndices p (Grid w _ cells) = V.toList . V.map toGridIndex . V.findIndices p $ cells
  where
    toGridIndex :: Int -> (Int, Int)
    toGridIndex m = (m `rem` w, m `div` w)

toList :: Grid a -> [[a]]
toList g =
  [ catMaybes [ get g x y | x <- [0..(_gridWidth g)-1] ]
  | y <- [0..(_gridHeight g)-1]
  ]

data Direction
  = North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest
  deriving (Bounded, Enum, Eq, Ord, Show)

orthogonal :: [Direction]
orthogonal = [North, East, South, West]

diagonal :: [Direction]
diagonal = [NorthEast, SouthEast, SouthWest, NorthWest]

cardinal :: [Direction]
cardinal = universe

offset :: Direction -> (Int, Int)
offset North     = (0, -1)
offset NorthEast = (1, -1)
offset East      = (1,  0)
offset SouthEast = (1, 1)
offset South     = (0,  1)
offset SouthWest = (-1, 1)
offset West      = (-1, 0)
offset NorthWest = (-1, -1)

(.+.) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)

getDirection :: (Int, Int) -> Direction -> Grid a -> Maybe a
getDirection pos dir grid =
  let (x, y) = pos .+. offset dir
  in get grid x y
