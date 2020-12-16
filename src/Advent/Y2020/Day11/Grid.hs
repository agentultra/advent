module Advent.Y2020.Day11.Grid where

import qualified Data.HashMap.Strict as H

-- | A grid with (x, y) starting at (0, 0) in the "top left" corner
type Grid = HashMap (Int, Int) Char

step :: Grid -> Grid
step = undefined

unoccupied :: Grid -> (Int, Int) -> Maybe Grid
unoccupied grid coord = flipUnoccupied . H.lookup coord $ grid
  where
    flipUnoccupied (Just 'L')
      | occupiedSeats grid coord == Just 0 = Just $ H.update (const $ Just '#') coord grid
      | otherwise = Nothing
    flipUnoccupied _ = Nothing

occupied :: Grid -> (Int, Int) -> Maybe Grid
occupied grid coord = flipOccupied . H.lookup coord $ grid
  where
    flipOccupied (Just '#')
      | occupiedSeats grid coord <= Just 4 = Just $ H.update (const $ Just 'L') coord grid
      | otherwise = Nothing
    flipOccupied _ = Nothing

neighborhood :: Grid -> (Int, Int) -> Maybe (NonEmpty Char)
neighborhood grid (x, y) = do
  _ <- H.lookup (x, y) grid
  let ns = fromList
        $ catMaybes [ H.lookup (x-1, y-1) grid :: Maybe Char
                    , H.lookup (x, y-1) grid
                    , H.lookup (x+1, y-1) grid
                    , H.lookup (x+1, y) grid
                    , H.lookup (x+1, y+1) grid
                    , H.lookup (x, y+1) grid
                    , H.lookup (x-1, y+1) grid
                    , H.lookup (x-1, y) grid
                    ]
  pure ns

occupiedSeats :: Grid -> (Int, Int) -> Maybe Int
occupiedSeats grid coord = countOccupiedSeats <$> neighborhood grid coord
  where
    countOccupiedSeats :: NonEmpty Char -> Int
    countOccupiedSeats
      = foldl' (\count ch -> if '#' == ch then count + 1 else count) 0
