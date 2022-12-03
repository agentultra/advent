{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Advent.Y2022.Day2.Part1 where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Advent.Y2022.Day2.Rps

solution :: IO ()
solution = do
  raw <- T.readFile "data/2022/Day2.txt"
  let game = Game . map parseThrow . T.lines $ raw
      s = challengerGameScore game
  print s
  where
    parseThrow :: Text -> Throw
    parseThrow txt =
      Throw
      (parseOpponentShape . T.head $ txt)
      (parseChallengerShape . T.last $ txt)
