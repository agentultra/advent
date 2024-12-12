module Advent.Y2024.Day9.Part2Spec where

import Test.Hspec

import Advent.Y2024.Day9.File
import Advent.Y2024.Day9.Input
import Advent.Y2024.Day9.Part2
import qualified Data.Text.IO as T
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "Advent - Y2024 -  - Part 2" $ do
    describe "compact with the block strategy" $ do

      raw <- runIO $ T.readFile "data/2024/Day9-sample.txt"
      let (disk, free, files) = fromRight (error "Invalid input") $ getInput raw

      context "Given the example input" $ do
        it "should compact file blocks" $ do
          let result = compact (block free files) disk
              expected = V.fromList [ 0,0,9,9,2,1,1,1,7,7
                                    , 7,(-1),4,4,(-1),3,3
                                    , 3,(-1),(-1),(-1),(-1)
                                    , 5,5,5,5,(-1),6,6,6,6
                                    , (-1),(-1),(-1),(-1)
                                    , (-1),8,8,8,8,(-1),(-1)
                                    ]
          putStrLn $ showDisk result
          result `shouldBe` expected

        it "should calculate expected checksum" $ do
          (checksum . compact (block free files) $ disk)
            `shouldBe` 2858
