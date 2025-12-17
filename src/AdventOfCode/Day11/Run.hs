{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Day11.Run where

import AdventOfCode.Day11.Input
import AdventOfCode.Utils
import Data.List.Extra
import Data.Foldable
import Control.Monad
import qualified Data.HashSet as S
import Data.Monoid
import Control.Monad.ST
import qualified Data.HashTable.ST.Basic as H
import qualified Data.HashMap.Strict as M
import Data.Hashable
import GHC.Generics
import Text.Megaparsec
import Data.Either.Extra (mapLeft)
import Data.Void
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import qualified Data.Vector as V
import Data.Vector.Instances ()
import Control.Lens hiding ((.>))
import Data.Semigroup
import Control.Parallel.Strategies
import Data.SBV
import Safe
import Control.Monad.Extra
import Debug.Trace
import Algebra.Graph.AdjacencyIntMap
import Data.Maybe
import qualified Data.IntSet as IS

testInput :: String
testInput = """
aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
"""

testInput2 :: String
testInput2 = """
svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
"""

type Devices = M.HashMap String [String]

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

graphStringsToInts :: Devices -> M.HashMap String Int
graphStringsToInts devices =
  let allNames = S.fromList $ sort (M.keys devices <> concat (M.elems devices))
  in  M.fromList $ zip (S.toList allNames) [0..]

edgesFromDevices :: Devices -> M.HashMap String Int -> [(Int, Int)]
edgesFromDevices devices stringMap = do
  deviceName <- M.keys devices
  deviceNameNumber <- maybeToList $ M.lookup deviceName stringMap
  deviceOutputs <- maybeToList $ M.lookup deviceName devices
  deviceOutput <- deviceOutputs
  deviceOutputNumber <- maybeToList $ M.lookup deviceOutput stringMap
  pure (deviceNameNumber, deviceOutputNumber)

type Part1Table s = H.HashTable s (Int, Int) Int

findAllPathsPart1 :: Part1Table s -> AdjacencyIntMap -> Int -> Int -> ST s Int
findAllPathsPart1 partsTable graph current target
    | current == target = pure 1
    | otherwise =
        let neighbours = IS.toList $ postIntSet current graph
            neighbourFold workingPaths neighbour = (workingPaths +) <$> findAllPathsPart1 partsTable graph neighbour target
            foldValue = foldM neighbourFold 0 neighbours
        in  lookupFromTableOrDefault partsTable (current, target) foldValue

part1Graph :: Devices -> IO Int
part1Graph devices = do
  let stringMap = graphStringsToInts devices
  let deviceEdges = edgesFromDevices devices stringMap
  let graph = edges deviceEdges
  youNumber <- lookupOrFail stringMap "you"
  outNumber <- lookupOrFail stringMap "out"
  let pathCount = runST $ do
        targetsTable <- H.new
        findAllPathsPart1 targetsTable graph youNumber outNumber
  pure pathCount

type Part2Table s = H.HashTable s (Int, Int, Bool, Bool) Int

findAllPathsPart2 :: Int -> Int -> Part2Table s -> AdjacencyIntMap -> Int -> Int -> Bool -> Bool -> ST s Int
findAllPathsPart2 midpoint1 midpoint2 partsTable graph current target visited1 visited2
    | current == target && visited1 && visited2 = pure 1
    | otherwise =
        let newVisited1 = visited1 || current == midpoint1
            newVisited2 = visited2 || current == midpoint2
            neighbours = IS.toList $ postIntSet current graph
            neighbourFold workingPaths neighbour = (workingPaths +) <$> findAllPathsPart2 midpoint1 midpoint2 partsTable graph neighbour target newVisited1 newVisited2
            foldValue = foldM neighbourFold 0 neighbours
        in  lookupFromTableOrDefault partsTable (current, target, visited1, visited2) foldValue

part2Graph :: Devices -> IO Int
part2Graph devices = do
  let stringMap = graphStringsToInts devices
  let deviceEdges = edgesFromDevices devices stringMap
  let graph = edges deviceEdges
  svrNumber <- lookupOrFail stringMap "svr"
  fftNumber <- lookupOrFail stringMap "fft"
  dacNumber <- lookupOrFail stringMap "dac"
  outNumber <- lookupOrFail stringMap "out"
  let pathCount = runST $ do
        targetsTable <- H.new
        findAllPathsPart2 fftNumber dacNumber targetsTable graph svrNumber outNumber False False
  pure pathCount

solvePart1 :: IO Int
solvePart1 = do
  devices <- either (error . show) pure $ parseDevices input
  part1Graph devices

solvePart2 :: IO Int
solvePart2 = do
  devices <- either (error . show) pure $ parseDevices input
  part2Graph devices

solve :: IO ()
solve = do
  presentResult 11 1 solvePart1
  presentResult 11 2 solvePart2

