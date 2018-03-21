{-# LANGUAGE DeriveTraversable #-}
module Signature where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Par.Class (spawn_)
import           Control.Monad.Par.IO    (IVar, ParIO)
import           Data.Function           (on)
import           Data.List               (groupBy, sortBy)

import           Classifier
import           Clasterization
import           Implementation
import           PreProcess
import           Result
import           Runner

newtype Signature a = Signature { getSig :: [ (Implementation, a) ]}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

restrictSignature :: Signature Result -> [Implementation] -> Signature Result
restrictSignature (Signature sig) is = Signature $ filter (flip elem is . fst) sig

isClassifier :: Signature Result -> Bool
isClassifier (Signature []) = True -- vacously
isClassifier (Signature ((i,r):rs)) = not $ all ((== r) . snd) rs

-- isClassifier s = length (clasterize s) > 1

clasterize :: Signature Result -> Clasterization Implementation
clasterize sig = fst <$> clasterizationBy (compare `on` snd) (getSig sig)

isIsolated :: Signature Result -> Maybe Implementation
isIsolated s | Clasterization [[i],_] <- clasterize s = Just i
             | Clasterization [_,[i]] <- clasterize s = Just i
             | otherwise                              = Nothing

classifies :: Signature Result -> [Implementation]
classifies = uniques . clasterize

buildSignature :: [Implementation] -> Classifier -> ParIO (Signature (IVar Result))
buildSignature is c = do
  rs <- mapM (spawn_ . liftIO . fmap preProcess . runOn c) is
  return $ Signature $ zip is rs
