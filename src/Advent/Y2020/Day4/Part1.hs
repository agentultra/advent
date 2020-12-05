module Advent.Y2020.Day4.Part1 where

import qualified Data.Attoparsec.Text as A
import qualified Data.Text.IO as T

import Advent.Y2020.Day4.Parse

solution :: IO ()
solution = do
  input <- T.readFile "data/2020/Day4.txt"
  case parsePassports input of
    Left err -> putStrLn err
    Right passports -> print passports
