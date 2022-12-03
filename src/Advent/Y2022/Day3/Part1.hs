{-# LANGUAGE OverloadedStrings #-}

module Advent.Y2022.Day3.Part1 where

import Data.Char
import Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T

solution :: IO ()
solution = do
  raw <- T.readFile "data/2022/Day3.txt"
  let s = L.sum . map (convert . L.head . nub . uncurry intersect . parseCompartments) . T.lines $ raw
  print s
  where
    parseCompartments :: Text -> (String, String)
    parseCompartments txt =
      let mid = T.length txt `div` 2
      in splitAt mid . T.unpack $ txt

    convert :: Char -> Int
    convert c
      | isLower c = ord c - 96
      | isUpper c = ord c - 38
      | otherwise = error "Invalid input"
