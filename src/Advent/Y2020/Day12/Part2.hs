module Advent.Y2020.Day12.Part2 where

import qualified Data.List.NonEmpty as N
import qualified Data.Text.IO as T

import Advent.Y2020.Day12.Parse
import Advent.Y2020.Day12.Ship

solution :: IO ()
solution = do
  input <- T.readFile "data/2020/Day12.txt"
  case parseInput input of
    Left err -> putStrLn err
    Right cmds -> print
                  $ distance
                  . snd
                  . processWayPointCommands (defaultWayPoint, Ship (Degree 0.0) 0 0)
                  . N.fromList
                  $ cmds
