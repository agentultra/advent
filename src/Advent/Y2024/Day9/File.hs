{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Advent.Y2024.Day9.File where

import Control.Monad.ST
import Data.Char
import qualified Data.List as List
import qualified Data.Map as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Prelude hiding (swap)

newtype FreeMap = FreeMap { getFreeMap :: Map Int Int }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

minIndexOf :: Int -> FreeMap -> Maybe Int
minIndexOf size = fmap fst . List.find fitsSizeOf . M.assocs . getFreeMap
  where
    fitsSizeOf :: (Int, Int) -> Bool
    fitsSizeOf (_, y) = size <= y

compact :: (forall s. Int -> Int -> V.MVector s Int -> ST s ()) -> Vector Int -> Vector Int
compact f disk = runST $ do
  vm <- V.thaw disk
  f 0 (V.length disk - 1) vm
  V.unsafeFreeze vm

swap :: forall s. Int -> Int -> V.MVector s Int -> ST s ()
swap lo hi v
  | lo < hi = do
      lv <- v `MV.read` lo
      hv <- v `MV.read` hi
      case (lv, hv) of
        ((-1), hc) | hc >= 0 -> MV.swap v lo hi >> swap (lo+1) (hi-1) v
        (lc, (-1)) | lc >= 0 -> swap (lo+1) (hi-1) v
        ((-1), (-1)) -> swap lo (hi-1) v
        (lc, hc) | lc >= 0 && hc >= 0 -> swap (lo+1) hi v
        (_, _) -> error "compact: invalid values detected"
  | otherwise = pure ()

block :: forall s. Map Int Int -> Int -> Int -> V.MVector s Int -> ST s ()
block freeMap lo hi v
  | lo < hi = undefined
  | otherwise = pure ()

checksum :: Vector Int -> Int
checksum = V.ifoldl' csum 0
  where
    csum :: Int -> Int -> Int -> Int
    csum acc ix c
      | c >= 0 = acc + (c * ix)
      | otherwise = acc
