{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Advent.Y2024.Day6.Room where

import Advent.Grid (Grid)
import qualified Advent.Grid as Grid
import qualified Data.Map as Map
import qualified Data.Set as Set
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
  deriving (Eq, Ord, Show)

data Room
  = Room
  { _guardPos        :: (Int, Int)
  , _guardFacing     :: Facing
  , _guardPath       :: Map (Int, Int) (Set Facing)
  , _potentialBlocks :: Int
  , _roomGrid        :: Grid Tile
  , _done            :: Bool
  }
  deriving (Eq, Show)

makeLenses ''Room

mkRoom :: (Int, Int) -> Grid Tile -> Room
mkRoom pos grid
  = Room
  { _guardPos        = pos
  , _guardFacing     = North
  , _guardPath       = Map.singleton pos $ Set.singleton North
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
  let (x', y') = pos .+. fromFacing facing
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
  let nextPos = pos .+. fromFacing facing
  guardPath .= Map.alter (updatePath facing) pos path
  guardPos .= nextPos

updatePath :: Facing -> Maybe (Set Facing) -> Maybe (Set Facing)
updatePath facing Nothing = Just $ Set.singleton facing
updatePath facing (Just facings) = Just $ facing `Set.insert` facings

turnRight :: State Room ()
turnRight = do
  pos <- use guardPos
  facing <- use guardFacing
  let facing' = facingRight facing
  guardFacing .= facing'
  guardPath %= \path -> Map.alter (updatePath facing) pos path

moveGuard :: State Room ()
moveGuard = do
  peekTile <- checkCollision
  case peekTile of
    Nothing -> do
      pos <- use guardPos
      facing <- use guardFacing
      guardPath %= \path -> Map.alter (updatePath facing) pos path
      done .= True
    Just t ->
      case t of
        Wall  -> turnRight
        Floor -> moveForward

isDone :: Room -> Bool
isDone room = room ^. done

runSimulationUntil :: (Room -> Bool) -> State Room ()
runSimulationUntil p = do
  r <- get
  if p r
    then pure ()
    else moveGuard >> runSimulationUntil p
