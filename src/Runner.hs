{-# LANGUAGE OverloadedStrings #-}
module Runner where

import           Data.ByteString           (ByteString)
import           System.Exit               (ExitCode)
import           System.Process.ByteString (readProcessWithExitCode)

import           Classifier
import           Implementation

data RawResult = RawResult {getRaw :: (ExitCode, ByteString, ByteString)}
  deriving (Show, Eq, Ord)

runOn :: Classifier -> Implementation -> IO RawResult
runOn (Classifier fn) (ImplementationNode interpreter) =
  RawResult <$> readProcessWithExitCode "node" [interpreter, fn] ""
