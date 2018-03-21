{-# LANGUAGE DeriveFunctor #-}
module Clasterization where

import Data.Function (on)
import Data.List (groupBy, sortBy)

newtype Clasterization a = Clasterization [[a]]
  deriving (Eq,Show,Functor)

clasterizationBy :: (a -> a -> Ordering) -> [a] -> Clasterization a
clasterizationBy cmp xs
  = Clasterization $ groupBy (\x y -> cmp x y == EQ) $ sortBy cmp xs

uniques :: Clasterization a -> [a]
uniques (Clasterization cs) = map head $ filter ((== 1) . length) cs
