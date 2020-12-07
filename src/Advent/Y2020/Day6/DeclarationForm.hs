module Advent.Y2020.Day6.DeclarationForm where

import qualified Data.Set as S

type Answer = Set Char

newtype Group = Group (NonEmpty Answer)
  deriving (Eq, Show)

groupAnswer :: Group -> Int
groupAnswer (Group answers) = S.size $ foldr S.union S.empty answers

correctGroupAnswer :: Group -> Int
correctGroupAnswer (Group answers)
  = S.size $ foldr S.intersection (head answers) (tail answers)
