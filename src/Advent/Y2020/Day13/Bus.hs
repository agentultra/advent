{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}

module Advent.Y2020.Day13.Bus where

import Data.Bifunctor
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
soonestBusFromTime start = minimumBy (compare `on` snd) . catSchedules . map (second (find ((== Stopped) . snd))) . foldMap (pure . busSchedule start)
  where
    catSchedules :: [(Bus, Maybe (Int, BusState))] -> [(Bus, Int)]
    catSchedules [] = []
    catSchedules ((b, Just (t, _)):rest) = (b, t) : catSchedules rest
    catSchedules ((_, Nothing):rest) = catSchedules rest

part1Solution :: Int -> Bus -> Int -> Int
part1Solution start (Bus bus) time = (time - start) * bus

part2Solution :: Int -> NonEmpty Bus -> Int
part2Solution start = _ . filter nonXBusses . zip [0..] . concatMap (flattenSchedule . busSchedule start)
  where
    flattenSchedule :: (Bus, [(Int, BusState)]) -> [(Bus, (Int, BusState))]
    flattenSchedule (b, schedule) = map (b,) schedule

    -- Only keep the Bus' with a non-X or non-"-1" Bus ID
    nonXBusses :: (Integer, (Bus, (Int, BusState))) -> Bool
    nonXBusses (_, (Bus (-1), t)) = False
    nonXBusses _ = True
