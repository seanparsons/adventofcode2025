{-# LANGUAGE MultilineStrings #-}

module AdventOfCode.Day07.Run where

import AdventOfCode.Day07.Input
import qualified Data.HashMap.Strict as M
import Data.Monoid
import Control.Monad.ST
import qualified Data.HashTable.ST.Basic as H

data BeamSquare = BeamStart | BeamContinue | BeamSplit
  deriving (Show, Eq)

parseBeamSquare :: ((Int, Int), Char) -> Either String ((Int, Int), BeamSquare)
parseBeamSquare ((x, y), 'S') = Right ((x, y), BeamStart)
parseBeamSquare ((x, y), '.') = Right ((x, y), BeamContinue)
parseBeamSquare ((x, y), '^') = Right ((x, y), BeamSplit)
parseBeamSquare entry = Left $ "Invalid beam square: " <> show entry

type BeamGrid = M.HashMap (Int, Int) BeamSquare

buildBeamGrid :: String -> Either String BeamGrid
buildBeamGrid inputString = do
  let inputLines = lines inputString
  let entries = do
        (y, line) <- zip [0..] inputLines
        (x, char) <- zip [0..] line
        pure ((x, y), char)
  beamEntries <- mapM parseBeamSquare entries
  pure $ M.fromList beamEntries

findBeamStart :: BeamGrid -> Maybe (Int, Int)
findBeamStart beamGrid = getFirst $ M.foldMapWithKey (\(x, y) square -> First (if square == BeamStart then Just (x, y) else Nothing)) beamGrid

data STState = STState

type BeamSplitTable s = H.HashTable s (Int, Int) Int

data ProblemPart = Part1 | Part2
  deriving (Show, Eq)

countSplits :: ProblemPart -> BeamGrid -> (Int, Int) -> BeamSplitTable s -> ST s Int
countSplits problemPart grid position@(x, y) beamSplitTable = do
  let splitsLookup = case M.lookup position grid of
        Just BeamSplit -> do 
          count1 <- countSplits problemPart grid (x - 1, y) beamSplitTable
          count2 <- countSplits problemPart grid (x + 1, y) beamSplitTable
          pure ((if problemPart == Part1 then 1 else 0) + count1 + count2)
        Just BeamContinue -> countSplits problemPart grid (x, y + 1) beamSplitTable
        Just BeamStart -> countSplits problemPart grid (x, y + 1) beamSplitTable
        Nothing -> pure (if problemPart == Part1 then 0 else 1)
  fromSplitTable <- H.lookup beamSplitTable position
  case fromSplitTable of
    Just count -> pure (if problemPart == Part1 then 0 else count)
    Nothing -> do
      count <- splitsLookup
      H.insert beamSplitTable position count
      pure count

testInput :: String
testInput = """
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
"""

solve :: IO ()
solve = do
  beamGrid <- either (error . show) pure $ buildBeamGrid input
  beamStart <- maybe (error "No beam start found.") pure $ findBeamStart beamGrid
  let part1Splits = runST $ do
        beamSplitTable <- H.newSized $ M.size beamGrid
        countSplits Part1 beamGrid beamStart beamSplitTable
  putStrLn ("Day 07 - 1: " <> show part1Splits)
  let part2Splits = runST $ do
        beamSplitTable <- H.newSized $ M.size beamGrid
        countSplits Part2 beamGrid beamStart beamSplitTable
  putStrLn ("Day 07 - 2: " <> show part2Splits)
