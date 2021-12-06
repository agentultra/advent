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

-- | Set the first 'Cell' matching @n@ to picked.
pick :: Int -> Board -> Board
pick n b@(Board grid w h) =
  case V.findIndex (== cell n) grid of
    Nothing -> b
    Just i -> Board (grid // [(i, Cell True n)]) w h

boardRows :: Board -> [Vector Cell]
boardRows (Board grid w _) =
  [ V.slice i w grid | i <- [0,w..(length grid - w)] ]

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

boardScore :: Board -> Int
boardScore = V.sum . V.map cellNum . unPicked

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
      pure (True, p * boardScore b)

part1Solution :: [Int] -> [Board] -> (Bool, Int)
part1Solution picks = evalState (play picks)

data SquidGame
  = SquidGame
  { squidBoards :: [Board]
  , winningBoards :: [Board]
  , lastPick :: Int
  }
  deriving (Eq, Show)

play2 :: [Int] -> State SquidGame Int
play2 [] = do
  (SquidGame _ winners p) <- get
  case viaNonEmpty head winners of
    Nothing -> error $ T.pack "No winners"
    Just w  -> pure $ p * boardScore w
play2 (p:ps) = do
  (SquidGame boards winners _) <- get
  let boards' = map (pick p) boards
      wins    = filter hasWon boards'
      remaining = filter (not . hasWon) boards'
      winners' = reverse wins ++ winners
  put $ SquidGame remaining winners' p
  if null remaining
    then case viaNonEmpty head winners' of
           Nothing -> error $ T.pack "WAT"
           Just w -> pure $ p * boardScore w
    else play2 ps

part2Solution :: [Int] -> [Board] -> Int
part2Solution picks boards = evalState (play2 picks) $ SquidGame boards [] 0
