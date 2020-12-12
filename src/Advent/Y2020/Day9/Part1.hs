module Advent.Y2020.Day9.Part1 where

import Advent.Input
import Advent.List

-- | Collect a value from the list and the _n_ values preceding it
prior :: Int -> [a] -> [(a, [a])]
prior n
  = catMaybes
  . map uncons
  . filter (\l -> length l == (n + 1))
  . map (reverse . (\l -> drop (length l - (n + 1)) l))
  . inits

sum2 :: Num a => (a, a) -> a
sum2 (x, y) = x + y

validXmas :: (Int, [Int]) -> Either Int Int
validXmas (target, nums) = case find ((== target) . sum2) $ combinations2 nums of
  Nothing -> Left target
  Just _  -> Right target

findInvalidXmas :: [Int] -> Maybe Int
findInvalidXmas = listToMaybe . lefts . map validXmas . prior 25

solution :: IO ()
solution = do
  input <- readInput "data/2020/Day9.txt"
  case traverse readInt input of
    Left err   -> putStrLn err
    Right nums -> print $ maybe (-1) identity $ findInvalidXmas nums
