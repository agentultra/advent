module Advent.Y2024.Day6.Part2 where

import qualified Advent.Grid as Grid
import Advent.Input
import Advent.Y2024.Day6.Input
import Advent.Y2024.Day6.Room
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Lens.Micro
import Lens.Micro.Mtl

answer :: State Room Int
answer = do
  original <- get
  let prime = execState (runSimulationUntil isDone) original
  let multiverse
        = map (placeObstruction original)
        . filter (/= original ^. guardPos)
        . Map.keys
        $ prime ^. guardPath
  pure . length . filter id . map checkForLoop $ multiverse

placeObstruction :: Room -> (Int, Int) -> Room
placeObstruction room (x, y) =
  room & roomGrid %~ \g -> fromJust $ Grid.set g x y Wall

checkForLoop :: Room -> Bool
checkForLoop = evalState runCheck

runCheck :: State Room Bool
runCheck = do
  moveGuard
  isDone <- use done
  if isDone
    then pure False
    else do
    pos <- use guardPos
    facing <- use guardFacing
    path <- use guardPath
    case pos `Map.lookup` path of
      Nothing -> runCheck
      Just facings ->
        if facing `Set.member` facings
        then pure True
        else runCheck

solution :: IO ()
solution = do
  raw <- readInput "data/2024/Day6.txt"
  let (room, guardPos) = orElse "Invalid input" $ parseInput raw
  print $ (`evalState` (mkRoom guardPos room)) answer
