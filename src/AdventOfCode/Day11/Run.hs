{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module AdventOfCode.Day11.Run where

import AdventOfCode.Day11.Input
import AdventOfCode.Utils
import Data.List.Extra
import Control.Monad
import qualified Data.HashSet as S
import Control.Monad.ST
import qualified Data.HashTable.ST.Basic as H
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.Vector.Instances ()
import Algebra.Graph.AdjacencyIntMap
import Data.Maybe
import qualified Data.IntSet as IS

type Devices = M.HashMap String [String]

type StringMap = M.HashMap String Int

parseDevice :: String -> Either String (String, [String])
parseDevice line = case splitOn ": " line of
  [deviceName, deviceOutputs] -> do
    let parsedOutputs = words deviceOutputs
    return (deviceName, parsedOutputs)
  _ -> Left "Invalid device."

parseDevices :: String -> Either String Devices
parseDevices inputString = do
  devices <- mapM parseDevice $ lines inputString
  return $ M.fromList devices

graphStringsToInts :: Devices -> StringMap
graphStringsToInts devices =
  let allNames = S.fromList $ sort (M.keys devices <> concat (M.elems devices))
  in  M.fromList $ zip (S.toList allNames) [0..]

edgesFromDevices :: Devices -> StringMap -> [(Int, Int)]
edgesFromDevices devices stringMap = do
  deviceName <- M.keys devices
  deviceNameNumber <- maybeToList $ M.lookup deviceName stringMap
  deviceOutputs <- maybeToList $ M.lookup deviceName devices
  deviceOutput <- deviceOutputs
  deviceOutputNumber <- maybeToList $ M.lookup deviceOutput stringMap
  pure (deviceNameNumber, deviceOutputNumber)

type PartTable s stateValues = H.HashTable s (Int, Int, stateValues) Int

class FindPaths baseValues stateValues where
  isCurrent :: baseValues -> stateValues -> Int -> Int -> Bool
  updateStateValues :: baseValues -> stateValues -> Int -> Int -> stateValues

instance FindPaths (Int, Int) (Bool, Bool) where
  isCurrent _ (visited1, visited2) current target = current == target && visited1 && visited2
  updateStateValues (midpoint1, midpoint2) (visited1, visited2) current _ = (visited1 || current == midpoint1, visited2 || current == midpoint2)

instance FindPaths () () where
  isCurrent _ _ current target = current == target
  updateStateValues _ _ _ _ = ()

findAllPaths :: (FindPaths baseValues stateValues, Hashable stateValues) => baseValues -> PartTable s stateValues -> AdjacencyIntMap -> Int -> Int -> stateValues -> ST s Int
findAllPaths baseValues partsTable graph current target stateValues
    | isCurrent baseValues stateValues current target = pure 1
    | otherwise =
        let newStateValues = updateStateValues baseValues stateValues current target
            neighbours = IS.toList $ postIntSet current graph
            neighbourFold workingPaths neighbour = (workingPaths +) <$> findAllPaths baseValues partsTable graph neighbour target newStateValues
            foldValue = foldM neighbourFold 0 neighbours
        in  lookupFromTableOrDefault partsTable (current, target, newStateValues) foldValue

type PartSpecifics baseValues stateValues = Devices -> StringMap -> IO (Int, Int, baseValues, stateValues)

partGraph :: (FindPaths baseValues stateValues, Hashable stateValues) => PartSpecifics baseValues stateValues -> IO Int
partGraph partSpecifics = do
  devices <- either (error . show) pure $ parseDevices input
  let stringMap = graphStringsToInts devices
  let deviceEdges = edgesFromDevices devices stringMap
  let graph = edges deviceEdges
  (start, end, baseValues, stateValues) <- partSpecifics devices stringMap
  let pathCount = runST $ do
        targetsTable <- H.new
        findAllPaths baseValues targetsTable graph start end stateValues
  pure pathCount

part1Specifics :: PartSpecifics () ()
part1Specifics _ stringMap = do
  youNumber <- lookupOrFail stringMap "you"
  outNumber <- lookupOrFail stringMap "out"
  pure (youNumber, outNumber, (), ())

part2Specifics :: PartSpecifics (Int, Int) (Bool, Bool)
part2Specifics _ stringMap = do
  svrNumber <- lookupOrFail stringMap "svr"
  fftNumber <- lookupOrFail stringMap "fft"
  dacNumber <- lookupOrFail stringMap "dac"
  outNumber <- lookupOrFail stringMap "out"
  pure (svrNumber, outNumber, (fftNumber, dacNumber), (False, False))

solvePart1 :: IO Int
solvePart1 = partGraph part1Specifics

solvePart2 :: IO Int
solvePart2 = partGraph part2Specifics

solve :: IO ()
solve = do
  presentResult 11 1 solvePart1
  presentResult 11 2 solvePart2