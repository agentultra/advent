module Advent.Y2024.Day5.Part2 where

import Advent.Y2024.Day5.Part1 (validUpdate)
import Advent.Y2024.Day5.Input
import Advent.Y2024.Day5.Rule
import Advent.Y2024.Day5.Update
import Data.Either
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.IO as T

data PageTree
  = Leaf
  | Node PageTree Int PageTree
  deriving (Eq, Show)

insertPage :: Int -> RuleMap -> PageTree -> PageTree
insertPage p rmap = \case
  Leaf -> Node Leaf p Leaf
  Node l q r -> case before rmap p q of
    Before -> Node (insertPage p rmap l) q r
    None   -> Node l q (insertPage p rmap r)

fromUpdate :: RuleMap -> Update -> PageTree
fromUpdate rmap (Update pages)
  = foldl' intoTree Leaf
  . NE.toList $ pages
  where
    intoTree :: PageTree -> Int -> PageTree
    intoTree pt p = insertPage p rmap pt

toUpdate :: PageTree -> Maybe Update
toUpdate ptree = Update <$> (NE.nonEmpty . go $ ptree)
  where
    go :: PageTree -> [Int]
    go Leaf = []
    go (Node l x r) = go l ++ [x] ++ go r

fixUpdate :: RuleMap -> Update -> Maybe Update
fixUpdate rmap = toUpdate . fromUpdate rmap

answer :: RuleMap -> [Update] -> Int
answer ruleMap
  = sum
  . catMaybes
  . map middle
  . catMaybes
  . map (fixUpdate ruleMap)

solution :: IO ()
solution = do
  raw <- T.readFile "data/2024/Day5.txt"
  let (rs, updates) = fromRight (error "Invalid input") $ getInput raw
      ruleMap = mkRuleMap rs
      invalidUpdates = filter (not . validUpdate ruleMap) updates
  print $ answer ruleMap invalidUpdates
