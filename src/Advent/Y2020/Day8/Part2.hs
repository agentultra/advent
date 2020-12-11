module Advent.Y2020.Day8.Part2 where

import qualified Data.Set as S
import qualified Data.Text.IO as T

import Advent.Y2020.Day8.Asm
import Advent.Y2020.Day8.Computer
import Advent.Y2020.Day8.Parse

allVariations :: [Instr] -> [Program]
allVariations instrs = map programFromList $ genVariations instrs
  where
    genVariations ((Nop, i):is) = ((Jmp, i):is):(((Nop, i):) <$> genVariations is)
    genVariations ((Jmp, i):is) = ((Nop, i):is):(((Jmp, i):) <$> genVariations is)
    genVariations ((Acc, i):is) = ((Acc, i):is):(((Acc, i):) <$> genVariations is)
    genVariations [] = [[]]

runProgram :: Program -> Maybe Int
runProgram prg = case runState detectLoop $ LoopState 0 0 S.empty prg of
  (Terminated, state) -> Just $ loopStateAccumulator state
  (InfiniteLoop, _)   -> Nothing
  (OutOfBounds, _)    -> Nothing

solution :: IO ()
solution = do
  input <- T.readFile "data/2020/Day8.txt"
  case parseInput input of
    Left err  -> putStrLn err
    Right prg -> do
      -- ew. don't @ me bro.
      let result = [ result
                   | result <- map runProgram (allVariations prg)
                   , isJust result
                   ]
      case listToMaybe result of
        Nothing -> putStrLn "Nope"
        Just (Just result) -> print result
