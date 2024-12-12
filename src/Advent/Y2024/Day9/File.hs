{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}

module Advent.Y2024.Day9.File where

import Control.Monad.ST
import Data.Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Prelude hiding (swap)

newtype FreeMap = FreeMap { getFreeMap :: Map Int Int }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

minIndexOf :: Int -> FreeMap -> Maybe Int
minIndexOf size = fmap fst . List.find fitsSizeOf . Map.assocs . getFreeMap
  where
    fitsSizeOf :: (Int, Int) -> Bool
    fitsSizeOf (_, y) = size <= y

updateFree :: Int -> Int -> FreeMap -> FreeMap
updateFree ix size = FreeMap . Map.alter modifyFree ix . getFreeMap
  where
    modifyFree :: Maybe Int -> Maybe Int
    modifyFree (Just x) | x < size = Just $ size - x
    modifyFree (Just x) | x == size = Nothing
    modifyFree (Just x) | x > size = error "updateFree: tried to over-allocate a free block"
    modifyFree Nothing = error "updateFree: tried to modify a non-existent free block"

data FileMeta
  = FileMeta
  { fileIndex :: Int
  , loBlock   :: Int
  , hiBlock   :: Int
  }
  deriving (Eq, Show)

filesize :: FileMeta -> Int
filesize (FileMeta _ lo hi) = hi - lo

moveFileBlock :: forall s. Int -> FileMeta -> V.MVector s Int -> ST s ()
moveFileBlock ix fileMeta disk = do
  forM_ [0..filesize fileMeta - 1] $ \offset ->
    MV.swap disk (ix+offset) ((fileMeta.loBlock)+offset)

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

block :: forall s. FreeMap -> [FileMeta] -> Int -> Int -> V.MVector s Int -> ST s ()
block _ [] _ _ _ = pure ()
block freeMap (file:flist) lo hi v = do
  let fsize = filesize file
  case minIndexOf fsize freeMap of
    Nothing -> block freeMap flist lo hi v
    Just freeIndex -> do
      moveFileBlock freeIndex file v
      let freeMap' = updateFree freeIndex fsize freeMap
      block freeMap' flist lo hi v

checksum :: Vector Int -> Int
checksum = V.ifoldl' csum 0
  where
    csum :: Int -> Int -> Int -> Int
    csum acc ix c
      | c >= 0 = acc + (c * ix)
      | otherwise = acc
