module Advent.Y2021.Day6.Fish where

import Control.Monad.ST
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV

nextState :: Vector Int -> Vector Int
nextState fish =
  let guppies = V.replicate (V.length . V.filter (== 0) $ fish) 8
  in ageFish fish V.++ guppies
  where
    ageFish :: Vector Int -> Vector Int
    ageFish = V.map (\f -> if f == 0 then 6 else f - 1)

runFishes :: Int -> Vector Int -> Vector Int
runFishes 0 fishes = fishes
runFishes n fishes = runFishes (n - 1) . nextState $ fishes

runFishes' :: Int -> [Int] -> Int
runFishes' days fishes = V.sum $ go days . initSchool $ fishes
  where
    -- The school counts how many fish we see at each age.  The
    -- indices represent the age and the values are the sum of the
    -- fish at that age.
    initSchool :: [Int] -> Vector Int
    initSchool fs = let school = V.replicate 9 0 in
      V.modify (initFish fs) school

    initFish :: [Int] -> MVector m Int -> ST m ()
    initFish fs school = do
      forM_ fs $ \f -> do
        sf <- MV.read school f
        MV.write school f (sf + 1)

    go :: Int -> Vector Int -> Vector Int
    go 0 school = school
    go n school = go (n - 1) . stepSchool $ school

    -- When we step the state of the school we decrement the ages of
    -- all fish by moving down the sums by one index.
    stepSchool :: Vector Int -> Vector Int
    stepSchool school =
      let guppies = V.unsafeTake 1 school
          rest    = V.unsafeTail school
      in V.modify stepGuppies (rest V.++ guppies)

    -- The special case are the guppies.  When we add them to the 8
    -- column by the step function we add the sum of the new guppies
    -- with the adults so that all the fish age in the same number of
    -- steps.
    stepGuppies :: MVector m Int -> ST m ()
    stepGuppies school = do
      adults <- MV.read school 6
      newGuppies <- MV.read school 8
      MV.write school 6 (adults + newGuppies)

part1Solution :: Vector Int -> Int
part1Solution = V.length . runFishes 80

part2Solution :: Vector Int -> Int
part2Solution = runFishes' 256 . V.toList
