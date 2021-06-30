{-# LANGUAGE DerivingStrategies #-}

module Advent.Y2020.Day13.Bus where

import Data.Function
import Data.List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

newtype Bus = Bus { fromBus :: Int }
  deriving stock (Show)

data BusState = InTransit | Stopped
  deriving (Eq, Show)

busSchedule :: Int -> Bus -> (Bus, [(Int, BusState)])
busSchedule start b@(Bus busId) =
  (b, [ (t, if t `mod` busId == 0 then Stopped else InTransit) | t <- [start..] ])

soonestBusFromTime :: Int -> NonEmpty Bus -> (Bus, Int)
soonestBusFromTime start busses = minimumBy (compare `on` snd) . catSchedules . map (\(b, route) -> (b, find ((== Stopped) . snd) route)) . foldMap (pure . busSchedule start) $ busses
  where
    catSchedules :: [(Bus, Maybe (Int, BusState))] -> [(Bus, Int)]
    catSchedules [] = []
    catSchedules ((b, Just (t, _)):rest) = (b, t) : catSchedules rest
    catSchedules ((_, Nothing):rest) = catSchedules rest

part1Solution :: Int -> Bus -> Int -> Int
part1Solution start (Bus bus) time = (time - start) * bus
