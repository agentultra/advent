{-# LANGUAGE OverloadedRecordDot #-}

module Advent.Y2024.Day9.Input where

import Advent.Y2024.Day9.File hiding (fileIndex)
import Data.Attoparsec.Text hiding (take)
import Data.Char
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Vector (Vector)
import qualified Data.Vector as V

data BlockKind = FileKind | FreeKind
  deriving (Eq, Show)

data ParseState
  = ParseState
  { fileIndex  :: Int
  , blockKind  :: BlockKind
  , diskVector :: Vector Int
  , freeMap    :: FreeMap
  , fileList   :: [FileMeta]
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
  flist <- gets fileList
  sizeChar <- lift digit
  let size  = digitToInt sizeChar
      diskLn = V.length disk
      disk' | size == 0 = disk
            | otherwise = V.concat [disk, V.replicate size $ index]
      flist' = (FileMeta index diskLn $ diskLn + (size - 1)) : flist
  put $ ps { fileIndex = index + 1
           , blockKind = FreeKind
           , diskVector = disk'
           , fileList = flist'
           }

parseFreeBlock :: StateT ParseState Parser ()
parseFreeBlock = do
  ps <- get
  disk <- gets diskVector
  free <- gets freeMap
  size <- digitToInt <$> lift digit
  let disk' = V.concat [disk,  V.replicate size (-1)]
      freeMap' = FreeMap $ M.insert (V.length disk) size free.getFreeMap
  put $ ps { blockKind = FileKind, diskVector = disk', freeMap = freeMap' }

parseBlocks :: StateT ParseState Parser ()
parseBlocks = many1' parseBlock >> pure ()

getInput :: Text -> Either String (Vector Int, FreeMap, [FileMeta])
getInput raw
  = getResults <$> parseOnly statefulParser raw
  where
    statefulParser :: Parser ParseState
    statefulParser
      = execStateT parseBlocks $ ParseState 0 FileKind V.empty mempty mempty

    getResults :: ParseState -> (Vector Int, FreeMap, [FileMeta])
    getResults s = (s.diskVector, s.freeMap, s.fileList)
