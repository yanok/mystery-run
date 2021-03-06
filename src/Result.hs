module Result where

import Data.ByteString (ByteString)
import System.Exit

data Result = Result { getResult :: Maybe (ExitCode, ByteString, ByteString)}
  deriving (Show, Eq, Ord)
