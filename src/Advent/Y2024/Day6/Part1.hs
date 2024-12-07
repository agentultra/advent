module Advent.Y2024.Day6.Part1 where

import Advent.Input
import Advent.Y2024.Day6.Input
import Advent.Y2024.Day6.Room
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Lens.Micro
import Lens.Micro.Mtl

answer :: State Room Int
answer = do
  isDone <- use done
  path <- use guardPath
  if isDone
    then pure $ Map.size path
    else moveGuard >> answer

solution :: IO ()
solution = do
  raw <- readInput "data/2024/Day6.txt"
  let (room, guardPos) = orElse "Invalid input" $ parseInput raw
  print $ (`evalState` (mkRoom guardPos room)) answer
