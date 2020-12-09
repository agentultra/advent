module Advent.Y2020.Day7.Part1 where

import Data.Graph
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Data.Text.IO as T

import Advent.Y2020.Day7.Bag
import Advent.Y2020.Day7.Parse

type BagTree = HashMap Bag [Bag]

bagTree :: [BagNode] -> BagTree
bagTree nodeList = go H.empty nodeList
  where
    go bagTree [] = bagTree
    go bagTree ((node, nodes):bs) =
      let bags = [ (snd $ n, node) | n <- nodes ]
          merged = foldl' mergeBags bagTree bags
      in go merged bs
      where
        mergeBags t (k, v) = H.insertWith (<>) k [v] t

findNumBags :: Bag -> BagTree -> Int
findNumBags b bagTree = length $ go b bagTree
  where
    go :: Bag -> BagTree -> Set Bag
    go b' t =
      case H.lookup b' t of
        Nothing    -> S.empty
        Just bags' -> S.unions (S.fromList bags' : map (`go` t) bags')

solution :: IO ()
solution = do
  input <- T.readFile "data/2020/Day7.txt"
  case parseInput input of
    Left err -> putStrLn err
    Right bagNodes -> print $ findNumBags "shiny gold" $ bagTree bagNodes
