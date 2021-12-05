module Advent.Y2021.Day4.Bingo where

import Data.List.Split (chunksOf)
import qualified Data.Text as T
import Data.Vector (Vector, (!), (!?), (//))
import qualified Data.Vector as V

data Cell = Cell { cellPicked :: Bool, cellNum :: Int }
  deriving (Eq, Show)

cell :: Int -> Cell
cell = Cell False

data Board
  = Board
  { _boardGrid   :: Vector Cell
  , _boardWidth  :: Int
  , _boardHeight :: Int
  }
  deriving (Eq, Show)

-- | @ps@ should be /square/.
--
-- 'Nothing' is returned if @width /= height@ but doesn't validate
-- that all rows are equal in @width@.
--
-- All cell numbers should form a unique set but we don't validate
-- that either.
mkBoard :: [[Cell]] -> Maybe Board
mkBoard ps = do
  h <- maybeTrue (> 0) $ length ps
  w <- length <$> viaNonEmpty head ps
  if w /= h
    then Nothing
    else do
    let b = V.fromList $ concat ps
    pure $ Board b w h

maybeTrue :: (a -> Bool) -> a -> Maybe a
maybeTrue p x
  | p x       = Just x
  | otherwise = Nothing

-- | Query for the first 'Cell' matching @n@ on the board.
boardGet :: Int -> Board -> Maybe Cell
boardGet n (Board grid _ _) = V.find (\(Cell _ cn) -> cn == n) grid

-- | Set the first 'Cell' matching @n@ to picked.
pick :: Int -> Board -> Board
pick n b@(Board grid w h) =
  case V.findIndex (== cell n) grid of
    Nothing -> b
    Just i -> Board (grid // [(i, Cell True n)]) w h

boardRows :: Board -> [Vector Cell]
boardRows (Board grid w _) =
  --[V.slice i w grid | i <- [0,(w-1)..(length grid - w)]]
  map V.fromList . chunksOf w . V.toList $ grid

boardCols :: Board -> [Vector Cell]
boardCols (Board grid w _) =
  let rows = chunksOf w . V.toList $ grid
  in map V.fromList . transpose $ rows

allPicked :: Vector Cell -> Bool
allPicked = V.all cellPicked

unPicked :: Board -> Vector Cell
unPicked (Board grid _ _) = V.filter (not . cellPicked) grid

hasWon :: Board -> Bool
hasWon b
  = any allPicked (boardRows b)
  || any allPicked (boardCols b)

play :: [Int] -> State [Board] (Bool, Int)
play [] = pure (False, 0)
play (p:ps) = do
  boards <- get
  let boards' = map (pick p) boards
  case find hasWon boards' of
    Nothing -> do
      put boards'
      play ps
    Just b  ->
      trace (show $ length boards') $ pure (True, p * (V.sum . V.map cellNum . unPicked $ b))

part1Solution :: [Int] -> [Board] -> (Bool, Int)
part1Solution picks = evalState (play picks)
  -- let (r, s) = runState (play picks) boards
  -- in trace (show $ filter hasWon s) $ r
