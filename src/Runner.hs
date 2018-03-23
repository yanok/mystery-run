{-# LANGUAGE OverloadedStrings #-}
module Runner where

import           Data.ByteString           (ByteString)
import           System.Exit               (ExitCode)
import           System.Process.ByteString (readProcessWithExitCode)
import           System.Timeout            (timeout)

import           Classifier
import           Implementation

data RawResult = RawResult {getRaw :: Maybe (ExitCode, ByteString, ByteString)}
  deriving (Show, Eq, Ord)

runOn :: Classifier -> Implementation -> IO RawResult
runOn (Classifier fn) (ImplementationNode interpreter) =
  RawResult <$> timeout 5000000 (readProcessWithExitCode "node" [interpreter, fn] "")
