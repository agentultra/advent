{-# LANGUAGE RankNTypes #-}

module Advent.Y2024.Day9.File where

import Control.Monad.ST
import Data.Char
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Prelude hiding (swap)

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

checksum :: Vector Int -> Int
checksum = V.ifoldl' csum 0
  where
    csum :: Int -> Int -> Int -> Int
    csum acc ix c
      | c >= 0 = acc + (c * ix)
      | otherwise = acc
