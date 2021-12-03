module Advent.Y2021.Day3.Binary where

import Data.Vector (Vector)
import qualified Data.Vector as V

newtype Word = Word { getWord :: Vector  }
  deriving (Eq, Show)
