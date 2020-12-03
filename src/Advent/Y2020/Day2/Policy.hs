module Advent.Y2020.Day2.Policy where

import Data.List

data Policy a = Policy (Int, Int) a
  deriving (Eq, Show)

check :: Policy Char -> String -> Either String Bool
check p@(Policy (lo, hi) x) input = within lo hi . length . filter (== x) $ input
  where
    within :: Int -> Int -> Int -> Either String Bool
    within min' max' z
      | z >= min' && z <= max' = pure True
      | otherwise = Left $ "check failed: " ++ show p ++ " on \"" ++ input ++ "\""

-- | XOR of the @Policy@ matches for the character
checkExclusiveMatch :: Policy Char -> String -> Either String Bool
checkExclusiveMatch p@(Policy (ix, jx) x) input
  | (input !! (ix - 1) == x) `xor` (input !! (jx - 1) == x) = Right True
  | otherwise = Left $ "checkExclusiveMatch failed: " ++ show p ++ " on \"" ++ input ++ "\""

checkMany
  :: (Policy Char -> String -> Either String Bool)
  -> [(Policy Char, String)] -> Int
checkMany f = length . rights . map (uncurry f)
