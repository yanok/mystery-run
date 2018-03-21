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
    :: Int
    -> [(Classifier, Signature Result)]
    -> [Implementation]
    -> Writer [String] [Implementation]
classify round cs is = do
  tell ["Round " ++ show round ++ ": trying to classify between " ++ show is]
  ciss <- forM cs $ \(c,sig) -> do
    let cis = classifies sig
    tell ["Round " ++ show round ++ ": " ++ show c ++ " classifies " ++ show cis]
    return cis
  case concat ciss of
    [] -> do tell ["Nothing new found"]
             return is
    found -> do
      let leftIs = is \\ found
      tell ["Implementations left not strictly classified: " ++ show leftIs]
      case leftIs of
        [_] -> do tell ["Can figure out the last one by exclusion"]
                  return []
        [] -> return []
        _ -> classify (succ round) (map (second $ flip restrictSignature leftIs) cs) leftIs

filterClassifiers
    :: [(Classifier, Signature Result)]
    -> Writer [String] [(Classifier, Signature Result)]
filterClassifiers cs = flip filterM cs $ \(c,s) ->
  if isClassifier s
  then return True
  else do tell [show c ++ " doesn't classify at all"]
          return False

filterSame
    :: [(Classifier, Signature Result)]
    -> Writer [String] [(Classifier, Signature Result)]
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
  let (r, msgs) = runWriter $ do
        cs' <- filterClassifiers $ zip cs sigs
        cs'' <- filterSame cs'
        classify 1 cs'' is
  mapM_ putStrLn msgs
  case r of
    [] -> do putStrLn "Successfully classified everything"
             return True
    _ -> do putStrLn $ "Failed to classify between " ++ show r ++
              ". " ++ show (length r - 1) ++ " more classifiers missing"
            return False
