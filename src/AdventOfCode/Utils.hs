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

parseInt :: String -> Either String Int
parseInt possibleInt = maybe (Left $ "Invalid int: " <> possibleInt) Right $ readMaybe possibleInt

parseInteger :: String -> Either String Integer
parseInteger possibleInteger = maybe (Left $ "Invalid integer: " <> possibleInteger) Right $ readMaybe possibleInteger

presentResult :: Show a => Int -> Int -> IO a -> IO ()
presentResult day part resultExpression = do
  before <- getCurrentTime
  result <- resultExpression
  after <- getCurrentTime
  let timeDiff = diffUTCTime after before
  let timeDiffMillis = realToFrac timeDiff * 1000 :: Double
  let formattedDay = printf "%02d" day :: String
  let formattedTime = " (" <> (printf "%f" timeDiffMillis :: String) <> "ms)"
  putStrLn $ "Day " <> formattedDay <> " - " <> show part <> ": " <> show result <> formattedTime

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