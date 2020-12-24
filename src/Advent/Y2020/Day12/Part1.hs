module Advent.Y2020.Day12.Part1 where

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
                  . processCommands defaultShip
                  . N.fromList
                  $ cmds
