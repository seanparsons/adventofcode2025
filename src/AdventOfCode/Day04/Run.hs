module AdventOfCode.Day04.Run where

import AdventOfCode.Day04.Input
import Data.Monoid
import qualified Data.HashSet as S
import Data.List
import AdventOfCode.Utils

testInputLines :: [String]
testInputLines =
  [ "..@@.@@@@."
  , "@@@.@.@.@@"
  , "@@@@@.@.@@"
  , "@.@@@@..@."
  , "@@.@@@@.@@"
  , ".@@@@@@@.@"
  , ".@.@.@.@@@"
  , "@.@@@.@@@@"
  , ".@@@@@@@@."
  , "@.@.@@@.@."
  ]

inputLines :: [String]
inputLines = lines input

allPositions :: [(Int, Int)]
allPositions = do
  (y, line) <- zip [0..] inputLines
  (x, _) <- zip [0..] line
  pure (x, y)

rollPositions :: [(Int, Int)]
rollPositions = do
  (y, line) <- zip [0..] inputLines
  (x, char) <- zip [0..] line
  if char == '@' then [(x, y)] else []

accessibleShifts :: [(Int, Int)]
accessibleShifts = fmap (\(x, y) -> (x, y))
  [ (-1, -1), (0, -1), (1, -1)
  , (-1, 0), (1, 0)
  , (-1, 1), (0, 1), (1, 1)
  ]

shiftPosition :: (Int, Int) -> (Int, Int) -> (Int, Int)
shiftPosition (x, y) (dx, dy) = (x + dx, y + dy)

type PaperRolls = S.HashSet (Int, Int)

adjacentRolls :: PaperRolls -> (Int, Int) -> [(Int, Int)]
adjacentRolls positionsSet position =
  let adjacentPositions = fmap (shiftPosition position) accessibleShifts
  in  filter (`S.member` positionsSet) adjacentPositions

isAccessible :: PaperRolls -> (Int, Int) -> Bool
isAccessible positionsSet position =
  let adjacent = adjacentRolls positionsSet position
  in  compareLength adjacent 4 == LT

removeAccessibleRolls :: PaperRolls -> PaperRolls
removeAccessibleRolls positionsSet = S.filter (not . isAccessible positionsSet) positionsSet

removeUntilNoChange :: PaperRolls -> PaperRolls
removeUntilNoChange positionsSet =
  let newPositionsSet = removeAccessibleRolls positionsSet
  in  if newPositionsSet == positionsSet then positionsSet else removeUntilNoChange newPositionsSet

solvePart1 :: IO Int
solvePart1 = do
  let positionsSet = S.fromList rollPositions
  let accessiblePositions = getSum $ foldMap (\position -> Sum $ if isAccessible positionsSet position then 1 else 0) rollPositions :: Int
  pure accessiblePositions

solvePart2 :: IO Int
solvePart2 = do
  let positionsSet = S.fromList rollPositions
  let allAccessibleRemoved = removeUntilNoChange positionsSet
  let changeInRolls = S.size positionsSet - S.size allAccessibleRemoved
  pure changeInRolls

solve :: [AOCUncomputedResult]
solve =
  [ AOCUncomputedResult 4 1 solvePart1
  , AOCUncomputedResult 4 2 solvePart2
  ]