{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Y2022.Day4.Part1Spec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Advent.Y2022.Day4.Interval

instance Arbitrary Interval where
  arbitrary = do
    x <- arbitrary
    y <- abs <$> arbitrary
    pure $ Interval x (x + y)

spec :: Spec
spec = do
  describe "overlaps" $ do
    prop "commutative" $ \i1 i2 ->
      (i1 `overlaps` i2) `shouldBe` (i2 `overlaps` i1)
