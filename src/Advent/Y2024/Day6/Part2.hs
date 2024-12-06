module Advent.Y2024.Day6.Part2 where

import Advent.Input
import Advent.Y2024.Day6.Input
import Advent.Y2024.Day6.Room
import Control.Monad.State.Strict
import qualified Data.Set as Set
import Lens.Micro
import Lens.Micro.Mtl

answer :: State Room Int
answer = do
  isDone <- use done
  blocks <- use potentialBlocks
  if isDone
    then pure blocks
    else moveGuard >> answer

solution :: IO ()
solution = do
  raw <- readInput "data/2024/Day6.txt"
  let (room, guardPos) = orElse "Invalid input" $ parseInput raw
  print $ (`evalState` (mkRoom guardPos room)) answer
