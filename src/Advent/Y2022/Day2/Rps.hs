{-# LANGUAGE LambdaCase #-}

module Advent.Y2022.Day2.Rps where

data Shape
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Show)

shapeScore :: Shape -> Int
shapeScore = \case
  Rock     -> 1
  Paper    -> 2
  Scissors -> 3

data Result = Win | Lose | Draw  deriving (Eq, Show)

resultScore :: Result -> Int
resultScore = \case
  Win  -> 6
  Lose -> 0
  Draw -> 3

data Throw
  = Throw
  { opponent   :: Shape
  , challenger :: Shape
  }
  deriving (Eq, Show)

throwResult :: Throw -> Result
throwResult (Throw opponentShape challengerShape) =
  case (opponentShape, challengerShape) of
    (Rock, Paper)     -> Win
    (Rock, Scissors)  -> Lose
    (Paper, Scissors) -> Win
    (Paper, Rock)     -> Lose
    (Scissors, Rock)  -> Win
    (Scissors, Paper) -> Lose
    _                 -> Draw

throwScore :: (Throw -> Shape) -> Throw -> Int
throwScore player throw
  = (resultScore . throwResult $ throw)
  + (shapeScore . player $ throw)

challengerScore :: Throw -> Int
challengerScore = throwScore challenger

data Game = Game [Throw]
  deriving (Eq, Show)

challengerGameScore :: Game -> Int
challengerGameScore (Game throws) = sum . map challengerScore $ throws
