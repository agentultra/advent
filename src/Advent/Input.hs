module Advent.Input where

import qualified Data.Text as T
import qualified Data.Text.IO as T

readInput :: Text -> IO [Text]
readInput filename = T.lines <$> T.readFile (T.unpack filename)
