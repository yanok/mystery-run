module Implementation where

newtype Implementation = ImplementationNode FilePath
  deriving (Eq, Ord, Show)
