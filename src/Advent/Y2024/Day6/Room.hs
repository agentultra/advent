{-# LANGUAGE TemplateHaskell #-}

module Advent.Y2024.Day6.Room where

import Advent.Grid (Grid)
import qualified Advent.Grid as Grid
import Control.Monad.State.Strict
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

data Tile = Floor | Wall
  deriving (Eq, Show)

-- | Parse a Tile from a Char
--
-- The @^@ character is used when parsing the room data to store the
-- guard position and can be considered a Floor tile.
fromChar :: Char -> Maybe Tile
fromChar '#' = Just Wall
fromChar '.' = Just Floor
fromChar '^' = Just Floor
fromChar _   = Nothing

data Facing = North | East | South | West
  deriving (Eq, Show)

data GuardCommand
  = TurnLeft
  | TurnRight
  | MoveForward
  deriving (Eq, Show)

data Room
  = Room
  { _guardPos        :: (Int, Int)
  , _guardFacing     :: Facing
  , _guardPath       :: [(Int, Int)]
  , _potentialBlocks :: Int
  , _roomGrid        :: Grid Tile
  , _done            :: Bool
  }
  deriving (Eq, Show)

makeLenses ''Room

mkRoom :: (Int, Int) -> Grid Tile -> Room
mkRoom pos grid
  = Room
  { _guardPos        =  pos
  , _guardFacing     = North
  , _guardPath       = [pos]
  , _potentialBlocks = 0
  , _roomGrid        = grid
  , _done            = False
  }

(.+.) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)

checkCollision :: State Room (Maybe Tile)
checkCollision = do
  pos <- use guardPos
  facing <- use guardFacing
  grid <- use roomGrid
  let (x', y') = pos .+. (fromFacing facing)
  pure $ Grid.get grid x' y'

fromFacing :: Facing -> (Int, Int)
fromFacing = \case
  North -> (0, -1)
  East -> (1, 0)
  South -> (0, 1)
  West -> (-1, 0)

facingRight :: Facing -> Facing
facingRight = \case
  North -> East
  East  -> South
  South -> West
  West  -> North

moveForward :: State Room ()
moveForward = do
  pos <- use guardPos
  facing <- use guardFacing
  path <- use guardPath
  let nextPos = pos .+. (fromFacing facing)
  guardPath .= nextPos : path
  guardPos .= nextPos

turnRight :: State Room ()
turnRight = do
  facing <- use guardFacing
  guardFacing .= facingRight facing

moveGuard :: State Room ()
moveGuard = do
  peekTile <- checkCollision
  facing <- use guardFacing
  pos <- use guardPos
  path <- use guardPath
  let posRight = pos .+. (fromFacing $ facingRight facing)
  case peekTile of
    Nothing -> done .= True
    Just t | t == Wall -> turnRight >> moveForward
    Just t | t == Floor -> do
               when (posRight `elem` path) $ do
                 potentialBlocks += 1
               moveForward
