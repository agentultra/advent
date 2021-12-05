module Advent.Y2021.Day4.Part1Spec where

import qualified Data.Attoparsec.Text as A
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Test.Hspec

import Advent.Y2021.Day4.Parse
import Advent.Y2021.Day4.Part1
import Advent.Y2021.Day4.Bingo

spec :: Spec
spec = do
  fdescribe "Advent - Y2021 - Part 1" $ do
    describe "boardRows" $ do
      it "should return the rows as they appear in the board" $ do
        let b = Board example1 5 5
        (V.concat . boardRows $ b) `shouldBe` _boardGrid b

    describe "hasWon" $ do
      it "should return True for a full row of picked numbers" $ do
        let (Just b) = mkBoard
              [ [(Cell True 14), (Cell True 21), (Cell True 7)]
              , [cell 10, cell 18, cell 3]
              , [cell 1, cell 2, cell 9]
              ]
        hasWon b `shouldBe` True

      it "should return True for a full column of picked numbers" $ do
        let (Just b) = mkBoard
              [ [(Cell True 1), cell 2, cell 3]
              , [(Cell True 4), cell 5, cell 6]
              , [(Cell True 7), cell 8, cell 9]
              ]
        hasWon b `shouldBe` True

      it "should return True for a larger column of picked numbers" $ do
        let (Just b) = mkBoard
              [ [cell 10, (Cell True 1), cell 2, cell 3]
              , [cell 20, (Cell True 4), cell 5, cell 6]
              , [cell 30, (Cell True 7), cell 8, cell 9]
              , [cell 40, (Cell True 11), cell 12, cell 13]
              ]
        hasWon b `shouldBe` True

      it "should trivially be True when both a column and row are picked" $ do
        let (Just b) = mkBoard
              [ [(Cell True 1), (Cell True 2), (Cell True 3)]
              , [(Cell True 4), cell 5, cell 6]
              , [(Cell True 7), cell 8, cell 9]
              ]
        hasWon b `shouldBe` True

      it "should be False when no row or column are picked" $ do
        let (Just b) = mkBoard
              [ [(Cell True 1), cell 2, cell 3]
              , [cell 4, (Cell True 5), cell 6]
              , [cell 7, cell 8, (Cell True 9)]
              ]
        hasWon b `shouldBe` False

      context "when the board is small" $ do
        it "should still return True iff a row is picked" $ do
          let (Just b) = mkBoard [ [ Cell True 1, Cell True 2]
                                 , [ cell 3, cell 4]
                                 ]
          hasWon b `shouldBe` True

        it "should still return True iff a column is picked" $ do
          let (Just b) = mkBoard [ [ Cell True 1, cell 2 ]
                                 , [ Cell True 3, cell 4 ]
                                 ]
          hasWon b `shouldBe` True

        it "should return False if a cell is picked but no row or column is filled" $ do
          let (Just b) = mkBoard [ [ Cell True 1, cell 2 ]
                                 , [ cell 3, cell 4 ]
                                 ]
          hasWon b `shouldBe` False

      it "should return False when no clear row or column is picked" $ do
        let b = Board example1 5 5
        hasWon b `shouldBe` False

    describe "play" $ do
      it "should eliminate picked numbers from played boards" $ do
        let (Just b1) = mkBoard [ [cell 1, cell 2]
                                , [cell 3, cell 4]
                                ]
            (Just b2) = mkBoard [ [cell 1, cell 8]
                                , [cell 9, cell 10]
                                ]
            played = execState (play [1, 10]) [b1, b2]
            notPicked = V.concat $ map unPicked played
            expected = V.fromList [cell 2, cell 3, cell 4, cell 8, cell 9]
        notPicked `shouldBe` expected

      it "should eliminate numbers from the right boards" $ do
        let (Just b1) = mkBoard [ [cell 1, cell 2]
                                , [cell 3, cell 4]
                                ]
            (Just b2) = mkBoard [ [cell 1, cell 8]
                                , [cell 9, cell 10]
                                ]
            [_, b2'] = execState (play [1, 10]) [b1, b2]
            notPicked = unPicked b2'
            expected = V.fromList [cell 8, cell 9]
        notPicked `shouldBe` expected

      it "should play until a board wins" $ do
        let (Just b1) = mkBoard [ [cell 1, cell 2]
                                , [cell 3, cell 4]
                                ]
            (Just b2) = mkBoard [ [cell 1, cell 8]
                                , [cell 9, cell 10]
                                ]
            result = evalState (play [1, 9]) [b1, b2]
        result `shouldBe` (True, (8 + 10) * 9)

      it "should pick the first board that wins" $ do
        let (Just b1) = mkBoard [ [cell 1, cell 2]
                                , [cell 3, cell 4]
                                ]
            (Just b2) = mkBoard [ [cell 1, cell 8]
                                , [cell 9, cell 10]
                                ]
            (Just b3) = mkBoard [ [cell 1, cell 8]
                                , [cell 9, cell 12]
                                ]
            result = evalState (play [1, 9]) [b1, b2, b3]
        result `shouldBe` (True, (8 + 10) * 9)

    describe "pick" $ do
      it "should eliminate the picked number from the set of unpicked numbers" $ do
        let (Just b1) = mkBoard [ [cell 1, cell 2]
                                , [cell 3, cell 4]
                                ]
            expected = V.fromList [cell 2, cell 3, cell 4]
        (unPicked . pick 1 $ b1) `shouldBe` expected

    describe "unPicked" $ do
      it "should not include picked cells" $ do
        let (Just b1) = mkBoard [ [ cell 1, Cell True 2 ]
                                , [ cell 3, Cell True 4 ]
                                ]
            expected = V.fromList [ cell 1, cell 3 ]
        unPicked b1 `shouldBe` expected

    describe "example input" $ do
      it "should return the right number of boards" $ do
        raw <- T.readFile "test/Advent/Y2021/Day4/example.txt"
        case A.parseOnly parseInput raw of
          Left err -> error "Failed parse"
          Right (_, boards) -> length boards `shouldBe` 3

      it "should parse the board correctly" $ do
        raw <- T.readFile "test/Advent/Y2021/Day4/example.txt"
        case A.parseOnly parseInput raw of
          Left err -> error "Failed parse"
          Right (_, boards) -> do
            case viaNonEmpty head boards of
              Nothing -> error "No boards parsed"
              Just b1 -> do
                let (Just b2) = mkBoard
                      [ [ cell 22, cell 13, cell 17, cell 11, cell 0]
                      , [ cell 8, cell 2, cell 23, cell 4, cell 24 ]
                      , [ cell 21, cell 9, cell 14, cell 16, cell 7 ]
                      , [ cell 6, cell 10, cell 3, cell 18, cell 5 ]
                      , [ cell 1, cell 12, cell 20, cell 15, cell 19 ]
                      ]
                b1 `shouldBe` b2

      it "should solve the example" $ do
        raw <- T.readFile "test/Advent/Y2021/Day4/example.txt"
        case A.parseOnly parseInput raw of
          Left err -> error "Failed parse"
          Right (picks, boards) -> do
            part1Solution picks boards
            `shouldBe`
            (True, 4512)

-- No row or column has been picked
example1 :: Vector Cell
example1 = V.fromList
  [ Cell {cellPicked = False, cellNum = 22}
  , Cell {cellPicked = False, cellNum = 13}
  , Cell {cellPicked = True, cellNum = 17}
  , Cell {cellPicked = True, cellNum = 11}
  , Cell {cellPicked = True, cellNum = 0}
  --
  , Cell {cellPicked = False, cellNum = 8}
  , Cell {cellPicked = True, cellNum = 2}
  , Cell {cellPicked = True, cellNum = 23}
  , Cell {cellPicked = True, cellNum = 4}
  , Cell {cellPicked = True, cellNum = 24}
  --
  , Cell {cellPicked = True, cellNum = 21}
  , Cell {cellPicked = True, cellNum = 9}
  , Cell {cellPicked = True, cellNum = 14}
  , Cell {cellPicked = False, cellNum = 16}
  , Cell {cellPicked = True, cellNum = 7}
  --
  , Cell {cellPicked = False, cellNum = 6}
  , Cell {cellPicked = False, cellNum = 10}
  , Cell {cellPicked = False, cellNum = 3}
  , Cell {cellPicked = False, cellNum = 18}
  , Cell {cellPicked = True, cellNum = 5}
  --
  , Cell {cellPicked = False, cellNum = 1}
  , Cell {cellPicked = False, cellNum = 12}
  , Cell {cellPicked = False, cellNum = 20}
  , Cell {cellPicked = False, cellNum = 15}
  , Cell {cellPicked = False, cellNum = 19}
  ]
