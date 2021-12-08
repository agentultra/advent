module Advent.Y2021.Day6.Fish where

import Data.Vector (Vector)
import qualified Data.Vector as V

nextState :: Vector Int -> Vector Int
nextState fish =
  let guppies = V.replicate (V.length . V.filter (== 0) $ fish) 8
  in ageFish fish V.++ guppies
  where
    ageFish :: Vector Int -> Vector Int
    ageFish = V.map (\f -> if f == 0 then 6 else f - 1)

runFishes :: Int -> Vector Int -> Vector Int
runFishes 0 fishes = fishes
runFishes n fishes = runFishes (n - 1) . nextState $ fishes

part1Solution :: Vector Int -> Int
part1Solution = V.length . runFishes 80
