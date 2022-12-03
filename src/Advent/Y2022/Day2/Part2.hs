{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Advent.Y2022.Day2.Part2 where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Advent.Y2022.Day2.Rps

solution :: IO ()
solution = do
  raw <- T.readFile "data/2022/Day2.txt"
  let guide = map parseThrow . T.lines $ raw
      game = secretStrategy guide
      s = challengerGameScore game
  print s
  where
    parseThrow :: Text -> (Shape, Result)
    parseThrow txt =
      ( parseOpponentShape . T.head $ txt
      , parseExpectedResult . T.last $ txt
      )

    secretStrategy :: [(Shape, Result)] -> Game
    secretStrategy = Game . map evalStrategy

    evalStrategy :: (Shape, Result) -> Throw
    evalStrategy = \case
      (Rock, Win) -> Throw Rock Paper
      (Rock, Draw) -> Throw Rock Rock
      (Rock, Lose) -> Throw Rock Scissors
      (Paper, Win) -> Throw Paper Scissors
      (Paper, Draw) -> Throw Paper Paper
      (Paper, Lose) -> Throw Paper Rock
      (Scissors, Win) -> Throw Scissors Rock
      (Scissors, Draw) -> Throw Scissors Scissors
      (Scissors, Lose) -> Throw Scissors Paper
