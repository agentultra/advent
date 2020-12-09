module Advent.Y2020.Day7.Part1 where

import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Data.Text.IO as T

import Advent.Y2020.Day7.Bag
import Advent.Y2020.Day7.Parse

type BagTree = HashMap Bag Contains

bagTree :: [BagNode] -> BagTree
bagTree nodeList = go H.empty nodeList
  where
    go bagTree [] = bagTree
    go bagTree ((node, nodes):bs) =
      case H.lookup node bagTree of
        Nothing -> go (H.insert node nodes bagTree) bs
        Just _ -> go (H.update (\bs' -> Just $ bs' <> nodes) node bagTree) bs

findNumBags :: Bag -> BagTree -> Int
findNumBags b bagTree = length $ go b bagTree
  where
    go :: Bag -> BagTree -> Set Bag
    go b' t =
      case H.lookup b' t of
        Nothing   -> S.empty
        Just bags ->
          let bags' = map snd bags
          in S.unions (S.fromList bags' : map (`go` t) bags')

solution :: IO ()
solution = do
  input <- T.readFile "data/2020/Day7.txt"
  case parseInput input of
    Left err -> putStrLn err
    Right bagNodes -> print $ findNumBags "shiny gold" $ bagTree bagNodes
