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
  { submarineX :: Int
  -- ^ 0 .. n goes from left to right
  , submarineY :: Int
  -- ^ 0 .. n goes from the surface to the depths
  }
  deriving (Eq, Show)

navigate :: NonEmpty Command -> Submarine
navigate = foldl' moveSub initSub
  where
    initSub = Submarine 0 0
    moveSub :: Submarine -> Command -> Submarine
    moveSub (Submarine x y) (Command Forward amt) = Submarine (x + amt) y
    moveSub (Submarine x y) (Command Down amt)    = Submarine x (y + amt)
    moveSub (Submarine x y) (Command Up amt)      = Submarine x (y - amt)

part1Solution :: NonEmpty Command -> Int
part1Solution cmds =
  let (Submarine x y) = navigate cmds
  in x * y
