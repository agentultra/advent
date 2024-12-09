module Advent.List where

combinations2 :: [a] -> [(a, a)]
combinations2 xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

combinations :: Int -> [a] -> [[a]]
combinations = (. subsequences) . filter . (. length) . (==)

powerCombinations :: [a] -> [[a]]
powerCombinations ns = concat [ combinations i ns | i <- [1..length ns] ]
