{-# LANGUAGE OverloadedStrings #-}

module Advent.Y2022.Day5.Part1 where

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Advent.Y2022.Day5.Stack

solution :: IO ()
solution = do
  raw <- T.readFile "data/2022/Day5.txt"
  let (headerLines, rest) = span (T.isPrefixOf "[") . T.lines $ raw
      initWarehouse = parseWarehouse headerLines
      commands = parseCommands . drop 2 $ rest
      warehouse = executePlan moveStack9000 initWarehouse commands
      s = stackTops warehouse
  print s
