module Advent.Y2020.Day8.Asm where

import Data.Vector (Vector, (!?), (//))
import qualified Data.Vector as V

data OpCode = Nop | Jmp | Acc
  deriving (Eq, Ord, Show)

type Instr = (OpCode, Int)

newtype Program = Program (Vector Instr)
  deriving (Eq, Show)

programFromList :: [Instr] -> Program
programFromList = Program . V.fromList

patch :: Program -> (Instr -> Instr) -> Int -> Maybe Program
patch (Program prg) p pc
  = prg !? pc >>= \instr -> pure . Program $ prg // [(pc, p instr)]
