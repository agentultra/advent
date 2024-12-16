{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Original implementation: https://johnwhiles.com/posts/implementing-a-trie

module Advent.Trie where

import qualified Data.Map.Strict as Map
import Prelude hiding (empty, get)

data Trie a
  = Node [a] (Map a (Trie a))
  | Empty (Map a (Trie a))
  deriving (Eq, Show)

empty :: Ord a => Trie a
empty = Empty mempty

get :: (Applicative m, Monoid (m [a])) => Trie a -> m [a]
get = \case
  Empty _  -> mempty
  Node v _ -> pure v

children :: Trie a -> Map a (Trie a)
children = \case
  Empty c  -> c
  Node _ c -> c

setChildren :: Trie a -> Map a (Trie a) -> Trie a
setChildren (Node s _) = Node s
setChildren (Empty _)  = Empty

insert :: forall a. Ord a => [a] -> Trie a -> Trie a
insert xs = go xs
  where
    go :: [a] -> Trie a -> Trie a
    go [] t = Node xs $ children t
    go (y:ys) t =
      let cs = children t
      in case Map.lookup y (children t) of
        Nothing -> setChildren t $ Map.insert y (go ys empty) cs
        Just child -> setChildren t $ Map.insert y (go ys child) cs

paths :: Trie a -> [[a]]
paths t = get t <> foldMap paths (children t)
