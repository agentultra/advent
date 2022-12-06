{-# LANGUAGE OverloadedStrings #-}

module Advent.Y2022.Day5.Stack where

import Data.Char
import qualified Data.Map as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Read as T

newtype Stack = Stack { unStack :: Vector Char }
  deriving (Eq, Show)

singleton :: Char -> Stack
singleton c = Stack $ V.singleton c

getStack :: Int -> Stack -> (Maybe Stack, Stack)
getStack n = toStacks . V.splitAt n . unStack
  where
    toStacks :: (Vector Char, Vector Char) -> (Maybe Stack, Stack)
    toStacks (slice, rest)
      | V.null slice = (Nothing, Stack rest)
      | otherwise    = (Just . Stack $ slice, Stack rest)

putStack :: Stack -> Stack -> Stack
putStack (Stack xs) (Stack ys) = Stack $ xs V.++ ys

emplace :: Char -> Stack -> Stack
emplace c (Stack cs) = Stack $ cs `V.snoc` c

moveStack9000 :: Int -> Stack -> Stack -> (Stack, Stack)
moveStack9000 n xs ys =
  case getStack n xs of
    (Just (Stack toMove), xs') -> (xs', putStack (Stack . V.reverse $ toMove) ys)
    (Nothing, xs')     -> (xs', ys)

moveStack9001 :: Int -> Stack -> Stack -> (Stack, Stack)
moveStack9001 n xs ys =
  case getStack n xs of
    (Just toMove, xs') -> (xs', putStack toMove ys)
    (Nothing, xs')     -> (xs', ys)

type Mover = Int -> Stack -> Stack -> (Stack, Stack)

newtype Warehouse = Warehouse { getStacks :: Map Int Stack }
  deriving (Eq, Show)

emptyWarehouse :: Warehouse
emptyWarehouse = Warehouse M.empty

parseWarehouse :: [Text] -> Warehouse
parseWarehouse = foldl' parseLine emptyWarehouse
  where
    parseLine :: Warehouse -> Text -> Warehouse
    parseLine (Warehouse stacks) txt =
      let newStacks = parseStackLine txt
      in Warehouse . foldl' updateStacks stacks $ newStacks

    parseStackLine :: Text -> [(Int, Char)]
    parseStackLine
      = mapMaybe (\(ix, txt) -> case find isAlpha txt of
                     Nothing -> Nothing
                     Just c  -> Just (ix, c))
      . zip [1..]
      . chunks 4
      . T.unpack

    updateStacks :: Map Int Stack -> (Int, Char) -> Map Int Stack
    updateStacks stacks (ix, c) = M.alter (handleStack c) ix stacks

    handleStack :: Char -> Maybe Stack -> Maybe Stack
    handleStack c (Just s) = Just . emplace c $ s
    handleStack c Nothing  = Just . singleton $ c

data StackCommand
  = Move
  { moveAmount    :: Int
  , moveFromStack :: Int
  , moveToStack   :: Int
  }
  deriving (Eq, Show)

parseCommand :: Text -> StackCommand
parseCommand txt =
  case T.decimal . T.dropWhile (not . isDigit) $ txt of
    Left err -> error $ "Invalid input: " <> T.pack err <> "(" <> txt <> ")"
    Right (amt, txt') ->
      case T.decimal . T.dropWhile (not . isDigit) $ txt' of
        Left _ -> error "Invalid input"
        Right (from, txt'') ->
          case T.decimal . T.dropWhile (not . isDigit) $ txt'' of
            Left _ -> error "Invalid input"
            Right (to, _) -> Move amt from to

parseCommands :: [Text] -> [StackCommand]
parseCommands = map parseCommand

executePlan :: Mover -> Warehouse -> [StackCommand] -> Warehouse
executePlan moveStack = foldl' updateWarehouse
  where
    updateWarehouse :: Warehouse -> StackCommand -> Warehouse
    updateWarehouse (Warehouse stacks) (Move amt from to) =
      let fromStack = stacks M.! from
          toStack = stacks M.! to
          (fromStack', toStack') = moveStack amt fromStack toStack
      in Warehouse
         $ M.fromList
         [ (from, fromStack')
         , (to, toStack')
         ] `M.union` stacks

stackTops :: Warehouse -> String
stackTops (Warehouse stacks)
  = filter isAlpha
  . map (fromMaybe ' ' . V.headM)
  . filter (not . V.null)
  . map unStack
  . M.elems
  $ stacks
