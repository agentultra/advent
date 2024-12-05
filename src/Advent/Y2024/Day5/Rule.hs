{-# LANGUAGE LambdaCase #-}

module Advent.Y2024.Day5.Rule where

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import qualified Debug.Trace as Debug

-- | Determines ordering of pairs
--
-- @(x, y)@ means that @x@ should sort before @y@, not necessarily
-- consecutively.
newtype Rule = Rule (Int, Int)
  deriving (Eq, Show)

data RuleOrdering
  = Before
  | None
  deriving (Eq, Show)

isBefore :: RuleOrdering -> Bool
isBefore = \case
  Before -> True
  None   -> False

-- | A RuleMap of @Rule (x, y)@ maps @y -> NonEmpty x@
--
-- Used to query whether @x@ should strictly preceed @y@
newtype RuleMap = RuleMap { getRuleMap :: Map Int (Set Int) }
  deriving (Eq, Show)

insertRule :: Rule -> RuleMap -> RuleMap
insertRule (Rule (x, y)) (RuleMap rmap)
  = RuleMap $ Map.alter insertBefore y rmap
  where
    insertBefore :: Maybe (Set Int) -> Maybe (Set Int)
    insertBefore Nothing   = Just $ Set.singleton x
    insertBefore (Just xs) = Just $ x `Set.insert` xs

mkRuleMap :: NonEmpty Rule -> RuleMap
mkRuleMap = go (RuleMap mempty) . NE.toList
  where
    go :: RuleMap -> [Rule] -> RuleMap
    go rmap [] = rmap
    go rmap (r:rs) = go (insertRule r rmap) rs

findRule :: Int -> RuleMap -> Maybe (Set Int)
findRule r (RuleMap rmap) = case r `Map.lookup` rmap of
  Nothing -> Nothing
  Just xs -> pure xs

before :: RuleMap -> Int -> Int -> RuleOrdering
before rmap x y = case findRule y rmap of
  Just xs | x `Set.member` xs -> Before
  _                           -> None
