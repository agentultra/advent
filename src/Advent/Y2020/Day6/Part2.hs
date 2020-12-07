module Advent.Y2020.Day6.Part2 where

import qualified Data.Text.IO as T

import Advent.Y2020.Day6.DeclarationForm
import Advent.Y2020.Day6.Parse

solution :: IO ()
solution = do
  input <- T.readFile "data/2020/Day6.txt"
  case parseInput input of
    Left err -> putStrLn err
    Right groups -> print $ sum . map correctGroupAnswer $ groups
