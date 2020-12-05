module Advent.Y2020.Day4.Part2 where

import qualified Data.Text.IO as T

import Advent.Y2020.Day4.Parse
import Advent.Y2020.Day4.Passport

solution :: IO ()
solution = do
  input <- T.readFile "data/2020/Day4.txt"
  case parsePassports input of
    Left err -> putStrLn err
    Right passports -> print $ length . rights . map validFromMap $ passports
