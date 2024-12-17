module Advent.Y2024.Day11.Stone where

import Control.Monad.State.Strict
import Data.Bifunctor
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Text.Read (read)

blink :: String -> [String]
blink x
  | x == "0" = ["1"]
  | even $ length x =
      let (l, r) = List.splitAt (length x `div` 2) x
      in [show (read @Int r), show (read @Int l)]
  | otherwise = [show (read x * 2024)]

blinkStones :: Int -> [String] -> Int
blinkStones blinks stones
  = evalState doBlink (blinks, Map.fromList . zip stones $ repeat 1)

doBlink :: State (Int, Map String Int) Int
doBlink = do
  (count, cache) <- get
  if count == 0
    then pure . sum . Map.elems $ cache
    else do
    let stones = [ (stone', c)
                 | (stone, c) <- Map.toList cache
                 , stone' <- blink stone
                 ]
    put (count - 1, Map.fromListWith (+) stones)
    doBlink
