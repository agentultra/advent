{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Y2021.Day5.Part1Spec where

import qualified Data.Set as S
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Advent.Y2021.Day5.Hydro

spec :: Spec
spec = do
  fdescribe "Advent - Y2021 - Part 1" $ do
    describe "orthogonal" $ do
      it "should project the set of Vec2 for 1,1 -> 1,3" $ do
        (orthogonal $ Line 1 1 1 3)
          `shouldBe`
          S.fromList [Vec2 (1, 1), Vec2 (1, 2), Vec2 (1, 3)]

      it "should project the set of Vec2 for 9,7 -> 7,7" $ do
        (orthogonal $ Line 9 7 7 7)
          `shouldBe`
          (S.fromList [Vec2 (9, 7), Vec2 (8, 7), Vec2 (7, 7)])

      it "should respect the inverse line" $ do
        (orthogonal $ Line 9 7 7 7)
          `shouldBe`
          (orthogonal $ Line 7 7 9 7)

    describe "intersections" $ do
      context "when there are overlapping elements between sets" $ do
        it "should return the intersection of all subsets" $ do
          let sets = fromList [ S.fromList [1, 2, 3]
                              , S.fromList [4, 5, 6]
                              , S.fromList [1, 7, 3]
                              ]
              expected = S.fromList [1, 3]
          (intersections @Int sets) `shouldBe` expected

        it "should return only the 'overlapping' points of two lines" $ do
          let lines = fromList [ Line 0 9 5 9, Line 0 9 2 9 ]
              expected = S.fromList [ Vec2 (0, 9), Vec2 (1, 9), Vec2 (2, 9) ]
          (intersections . fmap orthogonal $ lines)
            `shouldBe`
            expected

      prop "is a subset" $ \(sets :: NonEmptyList (Set Int)) -> do
        let xs = getNonEmpty sets
        (intersections (fromList xs) `S.isSubsetOf` (S.unions xs))
          `shouldBe`
          True

    describe "part1Solution" $ do
      it "should contain overlapping lines" $ do
        let lines = fromList [ Line 0 9 5 9, Line 0 9 2 9 ]
        part1Solution lines `shouldBe` 3

      it "should solve example input" $ do
        part1Solution exampleInput `shouldBe` 5

exampleInput :: NonEmpty Line
exampleInput
  = fromList
  [ Line 0 9 5 9
  , Line 8 0 0 8
  , Line 9 4 3 4
  , Line 2 2 2 1
  , Line 7 0 7 4
  , Line 6 4 2 0
  , Line 0 9 2 9
  , Line 3 4 1 4
  , Line 0 0 8 8
  , Line 5 5 8 2
  ]
