module Advent.Y2022.Day6.Signal where

import Data.List
import qualified Data.Text as T

isPacketMarker :: Int -> String -> Bool
isPacketMarker packetSize = (== packetSize) . length . nub

findPacketMarkerIndex :: Int -> Text -> Maybe Int
findPacketMarkerIndex packetSize = go packetSize . windows packetSize . T.unpack
  where
    go :: Int -> [String] -> Maybe Int
    go _ [] = Nothing
    go ix (p:ps)
      | isPacketMarker packetSize p = Just ix
      | otherwise                   = go (ix + 1) ps
