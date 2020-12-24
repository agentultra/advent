module Advent.Y2020.Day12.Ship where

import Data.Fixed

data Cardinal = North | South | East | West
  deriving (Eq, Show)

data Relative = Left' | Right'
  deriving (Eq, Show)

data Translation = Forward
  deriving (Eq, Show)

newtype Degree = Degree { unDegree :: Float }
  deriving (Eq, Show)

degree :: Float -> Degree
degree = Degree . (`mod'` 360.0)

data Instruction a b where
  Move :: Cardinal -> Int -> Instruction a b
  Turn :: Relative -> Degree -> Instruction a b
  Go   :: Translation -> Int -> Instruction a b

data Ship
  = Ship
  { orientation :: Degree
  , positionX   :: Int
  , positionY   :: Int
  }
  deriving (Eq, Show)

defaultShip :: Ship
defaultShip = Ship (degree 90) 0 0

command :: Instruction a b -> Ship -> Ship
command (Move cardinal amount) ship = case cardinal of
  North -> ship { positionY = positionY ship - amount }
  East  -> ship { positionX = positionX ship + amount }
  South -> ship { positionY = positionY ship + amount }
  West  -> ship { positionX = positionX ship - amount }

command (Turn relative amount) ship = case relative of
  Left'  -> ship
    { orientation = degree $ unDegree (orientation ship) - unDegree amount }
  Right' -> ship
    { orientation = degree $ unDegree (orientation ship) + unDegree amount }

command (Go Forward amount) ship =
  let (dx, dy) = bimap (* fromIntegral amount) (* fromIntegral amount)
                 . unitV2 $ orientation ship
  in ship { positionX = positionX ship + truncate dx
          , positionY = positionY ship - truncate dy
          }

processCommands :: Ship -> NonEmpty (Instruction a b) -> Ship
processCommands = foldl' (flip command)

distance :: Ship -> Int
distance Ship {..} = abs positionX + abs positionY

-- | Return the unit vector of an angle
unitV2 :: Degree -> (Float, Float)
unitV2 deg = let r = (pi / 2) - deg2rad deg in (cos r, sin r)

deg2rad :: Degree -> Float
deg2rad (Degree d) = d * 0.01745329 -- roughly pi/180
