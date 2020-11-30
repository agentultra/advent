module Advent.Input where

import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Parse each line of a file into a list
readInput
  :: Text -- ^ A file path
  -> IO [Text]
readInput filename = T.lines <$> T.readFile (T.unpack filename)
