module Main where

import           System.Environment    (getArgs)
import           System.FilePath.Glob  (glob)
import           System.FilePath.Posix (takeFileName)

import           Classifier
import           Classify
import           Implementation

main :: IO ()
main = do
  is <- fmap ImplementationNode <$> getArgs
  cs <- fmap (Classifier . takeFileName) <$> glob "[0-9][0-9].py"
  classifyIO cs is
  return ()
