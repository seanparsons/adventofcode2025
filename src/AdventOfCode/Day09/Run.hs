{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AdventOfCode.Day09.Run where

import AdventOfCode.Day09.Input
import AdventOfCode.Utils
import Data.List.Extra
import Data.Foldable
import Data.Char
import Control.Monad
import Debug.Trace
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Monoid
import Control.Monad.ST
import qualified Data.HashTable.ST.Basic as H
import Data.Hashable
import GHC.Generics
import Data.Ord
import Safe.Foldable

testInput :: String
testInput = """
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
"""

testInput2 :: String
testInput2 = """
7,1
11,1
11,7
9,7
9,5
6,5
6,7
2,7
2,3
7,3
"""

type Point = (Int, Int)

type Rectangle = (Point, Point)

type Edge = (Point, Point)

parseLine :: String -> Either String Point
parseLine line = case splitOn "," line of
  [x, y] -> (,) <$> parseInt x <*> parseInt y
  _ -> Left $ "Invalid line: " <> line

parseLines :: String -> Either String [Point]
parseLines linesString = mapM parseLine $ lines linesString

rectangleArea :: Rectangle -> Int
rectangleArea ((x1, y1), (x2, y2)) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

compareRectangles :: Rectangle -> Rectangle -> Ordering
compareRectangles firstRectangle secondRectangle = compare (rectangleArea firstRectangle) (rectangleArea secondRectangle)

rectanglesFromPoints :: [Point] -> [Rectangle]
rectanglesFromPoints points = S.toList $ S.fromList $ do
  point1 <- points
  point2 <- points
  if point1 == point2 then [] else [(min point1 point2, max point1 point2)]

maximumRectangle :: [Point] -> Maybe Rectangle
maximumRectangle points =
  let rectangles = rectanglesFromPoints points
  in  maximumByMay compareRectangles rectangles

solvePart1 :: IO Int
solvePart1 = do
  points <- either (error . show) pure $ parseLines input
  pure $ maybe 0 rectangleArea $ maximumRectangle points

edgesFromPoints :: [Point] -> [Edge]
edgesFromPoints points =
  let pointsLength = length points
      edgeFromPair (first : second : rest) = (first, second) : edgeFromPair (second : rest)
      edgeFromPair _ = []
      loopedAround = take (pointsLength + 1) $ cycle points
  in  edgeFromPair loopedAround

cornerPointsFromRectangle :: Rectangle -> [Point]
cornerPointsFromRectangle ((x1, y1), (x2, y2)) = [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]

edgesFromRectangle :: Rectangle -> [Edge]
edgesFromRectangle rectangle = edgesFromPoints $ cornerPointsFromRectangle rectangle

between :: Int -> Int -> Int -> Bool
between value first second | first > second = second <= value && value <= first
                           | otherwise = first <= value && value <= second

betweenExclusive :: Int -> Int -> Int -> Bool
betweenExclusive value first second | first > second = second < value && value < first
                                    | otherwise = first < value && value < second

isRectangleInside :: Rectangle -> [Edge] -> Bool
isRectangleInside rectangle edges = False

part2Fold :: [Edge] -> Maybe Rectangle -> Rectangle -> Maybe Rectangle
part2Fold edges Nothing rectangle | isRectangleInside rectangle edges = Just rectangle
                                  | otherwise = Nothing
part2Fold edges previousRect@(Just currentRectangle) rectangle =
  let rectangleIsLarger = rectangleArea rectangle > rectangleArea currentRectangle
      rectangleIsInside = isRectangleInside rectangle edges
  in  traceWith (\r -> "part2Fold: " <> show (rectangle, currentRectangle, rectangleArea rectangle, rectangleArea currentRectangle, rectangleIsLarger, rectangleIsInside, r)) $ if rectangleIsLarger && rectangleIsInside then Just rectangle else previousRect

solvePart2 :: IO Int
solvePart2 = do
  points <- either (error . show) pure $ parseLines testInput2
  let rectangles = rectanglesFromPoints points
  let pointEdges = edgesFromPoints points
  let maxRectangle = foldl' (part2Fold pointEdges) Nothing rectangles
  pure $ maybe 0 rectangleArea maxRectangle

solve :: IO ()
solve = do
  presentResult 9 1 solvePart1
  presentResult 9 2 solvePart2