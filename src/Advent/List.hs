module Advent.List where

combinations2 :: [a] -> [(a, a)]
combinations2 xs = [(x, y) | (x:ys) <- tails xs, y <- ys]
