module Advent.Y2021.Day9.Vents where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Advent.Grid (Grid (..))
import qualified Advent.Grid as G

type CaveFloor = Grid Int

floorNeighbours :: CaveFloor -> Int -> Int -> [Int]
floorNeighbours flr x = mapMaybe (uncurry (G.get flr)) . neighbours x

neighbours :: Int -> Int -> [(Int, Int)]
neighbours x y = [ (x-1, y)
                 , (x, y - 1)
                 , (x+1, y)
                 , (x, y+1)
                 ]

lowPoints :: CaveFloor -> [(Int, Int)]
lowPoints flr@(Grid w h _) = do
  x <- [0..w-1]
  y <- [0..h-1]
  case G.get flr x y of
    Nothing -> error $ T.pack "WAT"
    Just c ->
      if all (> c) $ floorNeighbours flr x y
      then pure (x, y)
      else []

-- low point: (basin size, frontier points)
type BasinMap = Map (Int, Int) (Int, [(Int, Int)])

basins :: CaveFloor -> [Int]
basins flr = map fst . M.elems . go . initBasins $ flr
  where
    initBasins :: CaveFloor -> BasinMap
    initBasins
      = M.fromList
      . map (\x -> (x, (1, [x])))
      . lowPoints

    go :: BasinMap -> BasinMap
    go basinMap
      | allEmptyFrontiers basinMap = basinMap
      | otherwise                  = go $ M.map updateFrontier basinMap

    updateFrontier :: (Int, [(Int, Int)]) -> (Int, [(Int, Int)])
    updateFrontier (basinSize, [])     = (basinSize, [])
    updateFrontier (basinSize, (x:xs)) =
      let bs = basinNeighbours x
      in (basinSize + length bs, bs ++ xs)

    basinNeighbours :: (Int, Int) -> [(Int, Int)]
    basinNeighbours (x, y)
      = map fst
      . filter ((< 9) . snd)
      . zip (neighbours x y) $ floorNeighbours flr x y

    allEmptyFrontiers :: BasinMap -> Bool
    allEmptyFrontiers = all (null . snd) . M.elems

part1Solution :: NonEmpty (NonEmpty Int) -> Int
part1Solution xs = case G.mkGrid . map toList . toList $ xs of
  Nothing -> error $ T.pack "Bad Input"
  Just g  -> sum . mapMaybe (fmap (+1) . uncurry (G.get g)) . lowPoints $ g

part2Solution :: NonEmpty (NonEmpty Int) -> Int
part2Solution xs = case G.mkGrid . map toList . toList $ xs of
  Nothing -> error $ T.pack "Bad Input"
  Just g  -> multiply . take 3 . reverse . sort . basins $ g
  where
    multiply = foldl' (*) 1
