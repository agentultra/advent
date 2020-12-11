module Advent.Y2020.Day8.Part1 where

import qualified Data.Text.IO as T

import Advent.Y2020.Day8.Computer
import Advent.Y2020.Day8.Parse

solution :: IO ()
solution = do
  input <- T.readFile "data/2020/Day8.txt"
  case parseInput input of
    Left err  -> putStrLn err
    Right prg ->
      print $ loopStateAccumulator $ execState detectLoop $ initialLoopState prg
