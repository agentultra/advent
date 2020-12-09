module Advent.Y2020.Day7.Bag where

type Bag      = Text
type Contains = [(Int, Bag)]
type BagNode  = (Bag, Contains)
