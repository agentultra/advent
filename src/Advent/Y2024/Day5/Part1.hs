module Advent.Y2024.Day5.Part1 where

import Advent.Y2024.Day5.Input
import Advent.Y2024.Day5.Rule
import Advent.Y2024.Day5.Update
import Data.Either
import qualified Data.List.NonEmpty as NE
import Data.Text.IO as T

import qualified Debug.Trace as Debug

validUpdate :: RuleMap -> Update -> Bool
validUpdate rmap (Update pages) =
  let ps = NE.toList pages
  in and
  . map validate
  . map (\(ix, target) -> (left ix ps, target))
  . zip [0..] $ ps
  where
    left :: Int -> [a] -> [a]
    left n = take n

    validate :: ([Int], Int) -> Bool
    validate (preceedingPages, target) = all isBefore . map (\page -> before rmap page target) $ preceedingPages

validUpdates :: RuleMap -> [Update] -> [Update]
validUpdates rmap = filter (validUpdate rmap)

answer :: RuleMap -> [Update] -> Int
answer ruleMap
  = sum
  . catMaybes
  . map middle
  . filter (validUpdate ruleMap)

solution :: IO ()
solution = do
  raw <- T.readFile "data/2024/Day5.txt"
  let (rs, updates) = fromRight (error "Invalid input") $ getInput raw
      ruleMap       = mkRuleMap rs
  print $ answer ruleMap updates
