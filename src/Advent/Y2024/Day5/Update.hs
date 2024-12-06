module Advent.Y2024.Day5.Update where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NE

newtype Update = Update (NonEmpty Int)
  deriving (Eq, Show)

middle :: Update -> Maybe Int
middle (Update pages) =
  let ps = NE.toList pages
      mid = length ps `div` 2
  in case List.splitAt mid ps of
    (_, []) -> Nothing
    (_, (x:xs)) -> Just x
