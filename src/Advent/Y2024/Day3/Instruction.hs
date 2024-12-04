module Advent.Y2024.Day3.Instruction where

data Instruction
  = Mul Int Int
  | Do
  | Dont
  deriving (Eq, Show)

data EvalState = Enabled | Disabled
  deriving (Eq, Show)

eval :: [Instruction] -> Int
eval = go Enabled 0
  where
    go :: EvalState -> Int -> [Instruction] -> Int
    go _ n [] = n
    go Enabled n (Mul x y:instrs)  = go Enabled (n + x * y) instrs
    go Disabled n (Mul _ _:instrs) = go Disabled n instrs
    go Enabled n (Do:instrs)       = go Enabled n instrs
    go Disabled n (Do:instrs)      = go Enabled n instrs
    go Enabled n (Dont:instrs)     = go Disabled n instrs
    go Disabled n (Dont:instrs)    = go Disabled n instrs
