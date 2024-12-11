module Advent.Y2024.Day9.Input where

import Advent.Y2024.Day9.File
import Data.Attoparsec.Text hiding (take)
import Data.Char
import qualified Data.List.NonEmpty as NE
import Data.Vector (Vector)
import qualified Data.Vector as V

data BlockKind = FileKind | FreeKind
  deriving (Eq, Show)

data ParseState
  = ParseState
  { fileIndex  :: Int
  , blockKind  :: BlockKind
  , diskVector :: Vector Int
  }
  deriving (Eq, Show)

parseBlock :: StateT ParseState Parser ()
parseBlock = do
  bKind  <- gets blockKind
  case bKind of
    FileKind -> parseFileBlock
    FreeKind -> parseFreeBlock

parseFileBlock :: StateT ParseState Parser ()
parseFileBlock = do
  ps <- get
  index <- gets fileIndex
  disk <- gets diskVector
  sizeChar <- lift digit
  let size  = digitToInt sizeChar
      disk' | size == 0 = disk
            | otherwise = V.concat [disk, V.replicate size $ index]
  put $ ps { fileIndex = index + 1, blockKind = FreeKind, diskVector = disk' }

parseFreeBlock :: StateT ParseState Parser ()
parseFreeBlock = do
  ps <- get
  disk <- gets diskVector
  size <- digitToInt <$> lift digit
  let disk' = V.concat [disk,  V.replicate size (-1)]
  put $ ps { blockKind = FileKind, diskVector = disk' }

parseBlocks :: StateT ParseState Parser ()
parseBlocks = many1' parseBlock >> pure ()

getInput :: Text -> Either String (Vector Int)
getInput raw = diskVector <$> parseOnly (execStateT parseBlocks $ ParseState 0 FileKind V.empty) raw
