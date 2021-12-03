module Advent.Y2021.Day2.Sub where

import Prelude hiding (Down)

data Direction
  = Forward
  | Down
  | Up
  deriving (Eq, Show)

data Command = Command Direction Int
  deriving (Eq, Show)

data Submarine
  = Submarine
  { submarineX   :: Int
  -- ^ 0 .. n goes from left to right
  , submarineY   :: Int
  -- ^ 0 .. n goes from the surface to the depths
  , submarineAim :: Int
  }
  deriving (Eq, Show)

navigate :: (Submarine -> Command -> Submarine) -> NonEmpty Command -> Submarine
navigate f = foldl' f initSub
  where
    initSub = Submarine 0 0 0

moveSub :: Submarine -> Command -> Submarine
moveSub (Submarine x y a) (Command Forward amt) = Submarine (x + amt) y a
moveSub (Submarine x y a) (Command Down amt)    = Submarine x (y + amt) a
moveSub (Submarine x y a) (Command Up amt)      = Submarine x (y - amt) a

aimSub :: Submarine -> Command -> Submarine
aimSub (Submarine x y a) (Command Down amt)    = Submarine x y (a + amt)
aimSub (Submarine x y a) (Command Up amt)      = Submarine x y (a - amt)
aimSub (Submarine x y a) (Command Forward amt) = Submarine (x + amt) (y + (a * amt)) a

part1Solution :: NonEmpty Command -> Int
part1Solution cmds =
  let (Submarine x y _) = navigate moveSub cmds
  in x * y

part2Solution :: NonEmpty Command -> Int
part2Solution cmds =
  let (Submarine x y _) = navigate aimSub cmds
  in x * y
