module Advent.Y2021.Day10.Chunk where

import qualified Data.Set as S
import qualified Data.Text as T

opening :: Set Char
opening = S.fromList ['(', '[', '{', '<']

closing :: Set Char
closing = S.fromList [')', ']', '}', '>']

data Result
  = Corrupt Char
  | Incomplete [Char]
  | Valid
  deriving (Eq, Show)

-- | An valid chunk opens and closes with the matching brace pairs
validChunk :: NonEmpty Char -> Result
validChunk (x :| xs) = go [x] xs
  where
    go :: [Char] -> [Char] -> Result
    go stack [] = if null stack then Valid else Incomplete stack
    go stack (y:ys)
      | y `S.member` opening = go (y : stack) ys
      | y `S.member` closing = case stack of
          []     -> Corrupt y
          (z:zs) -> let m = matches z y in
            if m == Valid then go zs ys else m
      | otherwise = error $ T.pack "Invalid input"

    matches :: Char -> Char -> Result
    matches '(' ')' = Valid
    matches '[' ']' = Valid
    matches '{' '}' = Valid
    matches '<' '>' = Valid
    matches  _   c  = Corrupt c

corruptScore :: Result -> Int
corruptScore (Corrupt c)
  | c == ')'  = 3
  | c == ']'  = 57
  | c == '}'  = 1197
  | c == '>'  = 25137
  | otherwise = error $ T.pack "Invalid input"
corruptScore _ = 0

completionScore :: Char -> Int
completionScore c
  | c == '('  = 1
  | c == '['  = 2
  | c == '{'  = 3
  | c == '<'  = 4
  | otherwise = error $ T.pack "Invalid input"

autocompleteScore :: Result -> Int
autocompleteScore (Incomplete cs) = foldl' score 0 cs
  where
    score :: Int -> Char -> Int
    score s c = s * 5 + completionScore c
autocompleteScore _ = error $ T.pack "Autocomplete invalid result"

part1Solution :: NonEmpty Text -> Maybe Int
part1Solution
  = fmap (sum . map (corruptScore . validChunk))
  . mapM nonEmptyString
  . toList

part2Solution :: NonEmpty Text -> Maybe Int
part2Solution = solution <=< convertInput
  where
    convertInput = mapM nonEmptyString . toList

    solution
      = middleOf
      . sort
      . map autocompleteScore
      . filter isIncomplete
      . map validChunk

    isIncomplete (Incomplete _) = True
    isIncomplete _              = False

-- Utils

nonEmptyString :: Text -> Maybe (NonEmpty Char)
nonEmptyString txt
  | T.null txt = Nothing
  | otherwise  = Just . fromList . T.unpack $ txt

middleOf :: [a] -> Maybe a
middleOf [] = Nothing
middleOf xs = viaNonEmpty head . drop (length xs `div` 2) $ xs
