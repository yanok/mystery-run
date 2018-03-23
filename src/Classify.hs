module Classify where

import           Control.Arrow           (second)
import           Control.Monad.Par.Class (get)
import           Control.Monad.Par.IO    (runParIO)
import           Control.Monad.Writer
import           Data.Function           (on)
import           Data.List               (groupBy, sortBy, (\\))

import           Classifier
import           Clasterization
import           Implementation
import           PreProcess
import           Result
import           Runner
import           Signature

classify
    :: Ord a
    => Int
    -> [(Classifier, Signature a)]
    -> [Implementation]
    -> Writer [String] [Implementation]
classify round cs is = do
  tell ["Round " ++ show round ++ ": trying to classify between " ++ show is]
  cs' <- filterClassifiers cs
  cs'' <- filterSame cs'
  ciss <- forM cs'' $ \(c,sig) -> do
    let cis = classifies sig
    tell ["Round " ++ show round ++ ": " ++ show c ++ " classifies " ++ show cis]
    return cis
  case concat ciss of
    [] -> do
      tell ["Nothing new found"]
      case cs'' of
        [] -> do
          tell ["No more classifiers"]
          return is
        [(c,s)] -> do
          tell ["The last classifier cut implementations into the following classes:"]
          let Clasterization cls = clasterize s
          tell [show $ cls]
          return is
        ((_,Signature s):_) -> do
          tell ["But some non-trivial classifiers still present, try building the composite one"]
          let rs = map (map snd . getSig . snd) cs
              rs' = zip (map fst s) rs
          classify (succ round) [(Classifier "composite", Signature rs')] is
    found -> do
      let leftIs = is \\ found
      tell ["Implementations left not strictly classified: " ++ show leftIs]
      case leftIs of
        [_] -> do tell ["Can figure out the last one by exclusion"]
                  return []
        [] -> return []
        _ -> do
          classify (succ round) (map (second $ flip restrictSignature leftIs) cs'') leftIs

filterClassifiers
    :: Ord a
    => [(Classifier, Signature a)]
    -> Writer [String] [(Classifier, Signature a)]
filterClassifiers cs = flip filterM cs $ \(c,s) ->
  if isClassifier s
  then return True
  else do tell [show c ++ " doesn't classify at all"]
          return False

filterSame
    :: Ord a
    => [(Classifier, Signature a)]
    -> Writer [String] [(Classifier, Signature a)]
filterSame cs = do
  let Clasterization css = clasterizationBy (compare `on` snd) cs
  forM css $ \eqCs -> case eqCs of
    [] -> error "impossible"
    [c] -> return c
    (c:cs') -> do
      forM_ cs' $ \c' ->
         tell ["Redundant " ++ show (fst c') ++
               " has the same results as " ++ show (fst c)]
      return c

classifyIO :: [Classifier] -> [Implementation] -> IO Bool
classifyIO cs is = do
  sigs <- runParIO $ do
    sigsIvs <- mapM (buildSignature is) cs
    traverse (traverse get) sigsIvs
  let (r, msgs) = runWriter $ classify 1 (zip cs sigs) is
  mapM_ putStrLn msgs
  case r of
    [] -> do putStrLn "Successfully classified everything"
             return True
    _ -> do putStrLn $ "Failed to classify between " ++ show r
            return False
