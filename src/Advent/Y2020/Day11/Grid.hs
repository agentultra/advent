module Advent.Y2020.Day11.Grid where

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

-- | A grid with (x, y) starting at (0, 0) in the "top left" corner
type Grid = HashMap (Int, Int) Char

step :: Grid -> Grid
step grid = H.foldlWithKey' (applyRule grid) H.empty grid
  where
    applyRule g g' c 'L'
      | unoccupied g c = H.insert c '#' g'
      | otherwise      = H.insert c 'L' g'
    applyRule g g' c '#'
      | occupied g c = H.insert c 'L' g'
      | otherwise    = H.insert c '#' g'
    applyRule _ g' c cell = H.insert c cell g'

unoccupied :: Grid -> (Int, Int) -> Bool
unoccupied grid coord = flipUnoccupied . H.lookup coord $ grid
  where
    flipUnoccupied (Just 'L') = occupiedSeats grid coord == Just 0
    flipUnoccupied _ = False

occupied :: Grid -> (Int, Int) -> Bool
occupied grid coord = flipOccupied . H.lookup coord $ grid
  where
    flipOccupied (Just '#') = occupiedSeats grid coord >= Just 4
    flipOccupied _ = False

visibleStep :: Grid -> Grid
visibleStep grid = H.foldlWithKey' (applyRule grid) H.empty grid
  where
    applyRule g g' c 'L'
      | visibleUnoccupied g c = H.insert c '#' g'
      | otherwise             = H.insert c 'L' g'
    applyRule g g' c '#'
      | visibleOccupied g c = H.insert c 'L' g'
      | otherwise           = H.insert c '#' g'
    applyRule _ g' c cell = H.insert c cell g'

visibleUnoccupied :: Grid -> (Int, Int) -> Bool
visibleUnoccupied grid coord = flipUnoccupied . H.lookup coord $ grid
  where
    flipUnoccupied (Just 'L') = visibleSeats grid coord Occupied == Just 0
    flipUnoccupied _ = False

visibleOccupied :: Grid -> (Int, Int) -> Bool
visibleOccupied grid coord = flipOccupied . H.lookup coord $ grid
  where
    flipOccupied (Just '#') = visibleSeats grid coord Occupied >= Just 5
    flipOccupied _ = False

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

visibleNeighborhood :: Grid -> (Int, Int) -> Maybe (NonEmpty Char)
visibleNeighborhood grid c = do
  _ <- H.lookup c grid
  pure
    $ fromList
    $ catMaybes [ hitTest grid c (-1, -1) :: Maybe Char
                , hitTest grid c (0, -1)
                , hitTest grid c (1, -1)
                , hitTest grid c (1, 0)
                , hitTest grid c (1, 1)
                , hitTest grid c (0, 1)
                , hitTest grid c (-1, 1)
                , hitTest grid c (-1, 0)
                ]

occupiedSeats :: Grid -> (Int, Int) -> Maybe Int
occupiedSeats grid coord = countOccupiedSeats <$> neighborhood grid coord
  where
    countOccupiedSeats :: NonEmpty Char -> Int
    countOccupiedSeats
      = foldl' (\count ch -> if '#' == ch then count + 1 else count) 0

countOfOccupied :: Grid -> Int
countOfOccupied = H.foldlWithKey' countOccupied 0
  where
    countOccupied count _ '#' = count + 1
    countOccupied count _ _   = count

type V2 = (Int, Int)

hitTest :: Grid -> (Int, Int) -> V2 -> Maybe Char
hitTest g (x, y) (dx, dy) = do
  let coord' = (x+dx, y+dy)
  cell <- H.lookup coord' g
  if cell /= '.'
    then pure cell
    else hitTest g coord' (dx, dy)

data SeatState = Occupied | Unoccupied deriving (Eq, Show)

visibleSeats :: Grid -> (Int, Int) -> SeatState -> Maybe Int
visibleSeats grid coord seatState
  = countVisibleSeats seatState <$> visibleNeighborhood grid coord
  where
    countVisibleSeats :: SeatState -> NonEmpty Char -> Int
    countVisibleSeats Occupied
      = foldl' (\count ch -> if '#' == ch then count + 1 else count) 0
    countVisibleSeats Unoccupied
      = foldl' (\count ch -> if 'L' == ch then count + 1 else count) 0

parseGrid :: [Text] -> Grid
parseGrid input = H.fromList [ ((x, y), c)
                             | (y, row) <- zip [0..] input
                             , (x, c) <- zip [0..] (T.unpack row)
                             ]
