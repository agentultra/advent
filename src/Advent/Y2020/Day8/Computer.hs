module Advent.Y2020.Day8.Computer where

import qualified Data.Set as S
import Data.Vector ((!?))
import qualified Data.Vector as V

import Advent.Y2020.Day8.Asm

data LoopState
  = LoopState
  { loopStateAccumulator :: Int
    -- ^ Value accumulator
  , loopStatePc          :: Int
    -- ^ Program counter
  , loopStateSeen        :: Set (Int, Instr)
    -- ^ The set of (pc, instruction) pairs to detect the loop
  , loopStateProgram     :: Program
    -- ^ The loaded program memory
  }
  deriving (Eq, Show)

initialLoopState :: [Instr] -> LoopState
initialLoopState prg
  = LoopState 0 0 S.empty $ programFromList prg

data ProgramResult = InfiniteLoop | Terminated | OutOfBounds
  deriving (Enum, Eq, Show)

detectLoop :: State LoopState ProgramResult
detectLoop = do
  s@(LoopState acc pc seen (Program prg)) <- get
  case prg !? pc of
    Nothing -> pure OutOfBounds
    Just (op, i) | (pc, (op, i)) `S.member` seen -> pure InfiniteLoop
                 | pc == V.length prg - 1 ->
                   if op == Acc
                     then put s { loopStateAccumulator = acc + i } >> pure Terminated
                     else pure Terminated
                 | otherwise -> case op of
                     Nop -> do
                       put s { loopStatePc = pc + 1 }
                       detectLoop
                     Acc -> do
                       put s { loopStateAccumulator = acc + i
                             , loopStatePc = pc + 1
                             , loopStateSeen = S.insert (pc, (op, i)) seen
                             }
                       detectLoop
                     Jmp -> do
                       put s { loopStatePc = pc + i
                             , loopStateSeen = S.insert (pc, (op, i)) seen
                             }
                       detectLoop
