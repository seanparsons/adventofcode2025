{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module AdventOfCode.Utils where

import Text.Read (readMaybe)
import Data.Time
import Text.Printf (printf)
import Data.Hashable
import qualified Data.HashTable.ST.Basic as H
import Control.Monad.ST
import Debug.Trace
import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception (bracket_)
import GHC.Conc (getNumCapabilities)
import qualified Data.HashMap.Strict as M
import Data.List

data AOCUncomputedResult = AOCUncomputedResult
                         { aocDay :: Int
                         , aocPart :: Int
                         , aocResult :: IO Int
                         }

data AOCComputedResult = AOCComputedResult
                       { aocUncomputedResult :: AOCUncomputedResult
                       , aocComputedResult :: !Int
                       , aocComputedTimeMillis :: !Double
                       }

parseInt :: String -> Either String Int
parseInt possibleInt = maybe (Left $ "Invalid int: " <> possibleInt) Right $ readMaybe possibleInt

parseInteger :: String -> Either String Integer
parseInteger possibleInteger = maybe (Left $ "Invalid integer: " <> possibleInteger) Right $ readMaybe possibleInteger

computeResult :: AOCUncomputedResult -> IO AOCComputedResult
computeResult uncomputedResult = do
  before <- getCurrentTime
  !result <- aocResult uncomputedResult
  after <- getCurrentTime
  let timeDiff = diffUTCTime after before
  let timeDiffMillis = realToFrac timeDiff * 1000 :: Double
  return AOCComputedResult { aocUncomputedResult = uncomputedResult, aocComputedResult = result, aocComputedTimeMillis = timeDiffMillis }

presentResult :: AOCComputedResult -> IO ()
presentResult AOCComputedResult{..} = do
  let AOCUncomputedResult{..} = aocUncomputedResult
  let formattedDay = printf "%02d" aocDay :: String
  let formattedTime = " (" <> (printf "%f" aocComputedTimeMillis :: String) <> "ms)"
  putStrLn $ "Day " <> formattedDay <> " - " <> show aocPart <> ": " <> show aocComputedResult <> formattedTime

computedResultSortValue :: AOCComputedResult -> (Int, Int)
computedResultSortValue AOCComputedResult{..} =
  let AOCUncomputedResult{..} = aocUncomputedResult
  in  (aocDay, aocPart)

presentResults :: [AOCUncomputedResult] -> IO ()
presentResults uncomputedResults = do
  results <- mapConcurrentlyBounded computeResult uncomputedResults
  let sortedResults = sortOn computedResultSortValue results
  mapM_ presentResult sortedResults

lookupFromTableOrDefault :: (Hashable k) => H.HashTable s k v -> k -> ST s v -> ST s v
lookupFromTableOrDefault hashCache key defaultValue = do
  lookupResult <- H.lookup hashCache key
  case lookupResult of
          Just value -> pure value
          Nothing -> do
            value <- defaultValue
            H.insert hashCache key value
            pure value

mapConcurrentlyBounded :: Traversable t => (a -> IO b) -> t a -> IO (t b)
mapConcurrentlyBounded f t = do
  caps <- getNumCapabilities
  sem <- newQSem caps
  mapConcurrently (\x -> bracket_ (waitQSem sem) (signalQSem sem) (f x)) t

lookupOrFail :: (Show k, Hashable k) => M.HashMap k v -> k -> IO v
lookupOrFail hashMap key = maybe (fail $ "Key not found: " <> show key) pure $ M.lookup key hashMap

eitherToIO :: Either String a -> IO a
eitherToIO = either (error . show) pure

oddFilter :: Foldable t => (a -> Bool) -> t a -> Bool
oddFilter predicate =
  let foldFunction !acc !x = if predicate x then not acc else acc
  in  foldl' foldFunction False

evenFilter :: Foldable t => (a -> Bool) -> t a -> Bool
evenFilter predicate =
  let foldFunction !acc !x = if predicate x then not acc else acc
  in  foldl' foldFunction True