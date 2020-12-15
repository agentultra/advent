module Advent.Y2020.Day9.Part2 where

import Data.Vector (Vector)
import qualified Data.Vector as V

import Advent.Input

data SearchState
  = SearchState
  { i  :: !Int
  , j  :: !Int
  , t  :: !Int
  , xs :: Vector Int
  }

crackXmas :: Int -> [Int] -> Maybe Int
crackXmas target nums
  = let ns = V.fromList nums
    in evalState searchForSum $ SearchState 0 1 target ns
  where
    searchForSum :: State SearchState (Maybe Int)
    searchForSum = do
      s@(SearchState i j t xs) <- get
      let slice = V.slice i j xs
          hit   = V.sum slice
      if t == hit
        then pure $ Just $ V.minimum slice + V.maximum slice
        else if i + j < V.length xs
             then put s { j = j + 1 } >> searchForSum
             else put s { i = i + 1, j = 1 } >> searchForSum

solution :: IO ()
solution = do
  input <- readInput "data/2020/Day9.txt"
  case traverse readInt input of
    Left err   -> putStrLn err
    Right nums -> case crackXmas 731031916 nums of
      Nothing -> putStrLn "Nope"
      Just x  -> print x
