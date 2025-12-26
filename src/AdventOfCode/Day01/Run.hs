module AdventOfCode.Day01.Run where

import AdventOfCode.Day01.Input
import AdventOfCode.Utils

data Rotation = RotateLeft Int | RotateRight Int
  deriving (Show, Eq)

data RotationState = RotationState Int Int
  deriving (Show, Eq)


parseLine :: String -> Either String Rotation
parseLine ('L' : n) = RotateLeft <$> parseInt n
parseLine ('R' : n) = RotateRight <$> parseInt n
parseLine content = Left ("Invalid" <> content)

rotationsFromInput :: Either String [Rotation]
rotationsFromInput = mapM parseLine $ lines input

applyRotation :: RotationState -> Rotation -> RotationState
applyRotation (RotationState x zerosSoFar) (RotateLeft n) =
  let baseX = x - n
      fixX xToFix zeros = if xToFix < 0 then fixX (xToFix + 100) (zeros + 1) else (xToFix, zeros)
      (newX, newZeros) = if baseX < 0 then fixX baseX 0 else (baseX, 0)
      startingZeroShift = if x == 0 then -1 else 0
  in  RotationState newX (zerosSoFar + newZeros + (if newX == 0 then 1 else 0) + startingZeroShift)
applyRotation (RotationState x zerosSoFar) (RotateRight n) =
  let baseX = x + n
      fixX xToFix zeros = if xToFix > 99 then fixX (xToFix - 100) (zeros + 1) else (xToFix, zeros)
      (newX, newZeros) = if baseX > 99 then fixX baseX 0 else (baseX, 0)
  in  RotationState newX (zerosSoFar + newZeros)

solvePart1 :: IO Int
solvePart1 = do
  rotations <- either (error . show) pure rotationsFromInput
  let states = scanl applyRotation (RotationState 50 0) rotations
  let firstPartZeros = length $ filter (\(RotationState stateValue _) -> stateValue == 0) states
  pure firstPartZeros

solvePart2 :: IO Int
solvePart2 = do
  rotations <- either (error . show) pure rotationsFromInput
  let finalState = foldl' applyRotation (RotationState 50 0) rotations
  let (RotationState _ secondPartZeros) = finalState
  pure secondPartZeros

solve :: [AOCUncomputedResult]
solve =
  [ AOCUncomputedResult 1 1 solvePart1
  , AOCUncomputedResult 1 2 solvePart2
  ]