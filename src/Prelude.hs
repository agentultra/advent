module Prelude
  ( module Relude
  , readNums
  )
  where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import Relude

readNums :: Text -> Either String [Int]
readNums t = map fst <$> traverse (T.decimal @Int) (T.split (== ',') t)
