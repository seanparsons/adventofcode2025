{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

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
import qualified Data.HashTable.Class ()
import Data.Hashable
import GHC.Generics
import Data.Ord
import Safe.Foldable
import Control.Monad.Extra
import qualified Data.Massiv.Core as MA
import qualified Data.Massiv.Array.Mutable as MA
import qualified Data.Massiv.Array as A
import Data.Massiv.Array (Ix2(..))

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
  if point1 == point2 then [] else [(point1, point2)]

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

{-# INLINE between #-}
between :: Int -> Int -> Int -> Bool
between value first second | first > second = second <= value && value <= first
                           | otherwise = first <= value && value <= second

{-# INLINE betweenExclusive #-}
betweenExclusive :: Int -> Int -> Int -> Bool
betweenExclusive value first second | first > second = second < value && value < first
                                    | otherwise = first < value && value < second

type VerticalEdgesFromPointCache s = H.HashTable s Point Int

{-# INLINE pointOnVerticalEdge #-}
pointOnVerticalEdge :: Point -> Edge -> Bool
pointOnVerticalEdge (px, py) ((x1, y1), (x2, y2)) = px == x1 && px == x2 && between py y1 y2

{-# INLINE pointOnHorizontalEdge #-}
pointOnHorizontalEdge :: Point -> Edge -> Bool
pointOnHorizontalEdge (px, py) ((x1, y1), (x2, y2)) = py == y1 && py == y2 && between px x1 x2

checkVerticalEdgeApplicable :: Int -> Int -> Int -> Edge -> Bool
checkVerticalEdgeApplicable fromX toX y ((x1, y1), (_, y2)) = between x1 fromX toX && between y y1 y2

isPointInside :: Int -> Point -> [Edge] -> [Edge] -> ST s Bool
isPointInside maxX point@(x, y) edges verticalEdges = do
  let pointOnEdge p edge = pointOnVerticalEdge p edge || pointOnHorizontalEdge p edge
  let pointOnEdges = any (pointOnEdge point) edges
  let oddPointsOnVerticalEdges = oddFilter (checkVerticalEdgeApplicable x maxX y) verticalEdges
  pure $ pointOnEdges || oddPointsOnVerticalEdges

isVerticalEdge :: Edge -> Bool
isVerticalEdge ((x1, _), (x2, _)) = x1 == x2

type PointInsideCache s = H.HashTable s Point Bool

isPointInsideMemoized :: PointInsideCache s -> Int -> [Edge] -> [Edge] -> Point -> ST s Bool
isPointInsideMemoized pointInsideCache maxX edges verticalEdges point =
  isPointInside maxX point edges verticalEdges

{-# INLINE rectanglePoints #-}
rectanglePoints :: Rectangle -> [Point]
rectanglePoints ((x1, y1), (x2, y2)) = do
  x <- [min x1 x2..max x1 x2]
  y <- [min y1 y2..max y1 y2]
  pure (x, y)

maxXFromPoints :: [Point] -> Int
maxXFromPoints points = maximum $ fmap fst points

type Points s = MA.MArray (MA.PrimState (ST s)) A.U MA.Ix2 Bool

addPointsList :: [Point] -> ST s (Points s)
addPointsList points = do
  let minX = minimum $ fmap fst points
  let minY = minimum $ fmap snd points
  let maxX = maximum $ fmap fst points
  let maxY = maximum $ fmap snd points
  pointsTable <- MA.newMArray (MA.Sz2 (maxX - minX) (maxY - minY)) False
  forM_ points $ \(!x, !y) -> do
    MA.write_ pointsTable (MA.Ix2 (x - minX) (y - minY)) True
  pure pointsTable

fillPointsFromEdges :: [Edge] -> ST s (Points s)
fillPointsFromEdges edges = addPointsList $ do
  !edge <- edges
  let ((!p1x, !p1y), (!p2x, !p2y)) = edge
  !x <- [min p1x p2x..max p1x p2x]
  !y <- [min p1y p2y..max p1y p2y]
  pure (x, y)

findRightAngleInnerPoint :: [Edge] -> Maybe Point
findRightAngleInnerPoint (firstEdge : secondEdge : restEdges) =
  case getRightAngleInnerPoint firstEdge secondEdge of
    Just innerPoint -> Just innerPoint
    Nothing -> findRightAngleInnerPoint (secondEdge : restEdges)
findRightAngleInnerPoint _ = Nothing

getRightAngleInnerPoint :: Edge -> Edge -> Maybe Point
getRightAngleInnerPoint ((e1x1, e1y1), e1End@(e1x2, e1y2)) (e2Start@(e2x1, e2y1), (e2x2, e2y2)) =
  let e1VerticalAndUp = e1x1 == e1x2 && e1y1 < e1y2
      e2HorizontalAndRight = e2y1 == e2y2 && e2x1 < e2x2
      endToStart = e1End == e2Start
      isRightAngle = e1VerticalAndUp && e2HorizontalAndRight && endToStart
  in  if isRightAngle then Just (e1x2 + 1, e1y2 + 1) else Nothing

isPointInsidePoints :: Points s -> Point -> ST s Bool
isPointInsidePoints points (x, y) = do
  lookupResult <- MA.read points (MA.Ix2 x y)
  case lookupResult of
    Just True -> pure True
    _ -> pure False

floodFill :: Points s -> Point -> ST s ()
floodFill !points (!x, !y) = do
  let sz = MA.sizeOfMArray points
  -- We need a queue for BFS. A standard list is fine for simple cases.
  let processQueue [] = return ()
      processQueue (currentIx : rest) = do
        -- Check if we have already visited this node
        isVisited <- MA.readM points currentIx
        
        if isVisited
        then processQueue rest
        else do
            -- MARK AS VISITED (Mutation happening here!)
            MA.writeM points currentIx True
            
            -- Add neighbors to queue
            let (r :. c) = currentIx
                neighbours = [ (r - 1) :. c
                            , (r + 1) :. c
                            , r :. (c - 1)
                            , r :. (c + 1)
                            ]
            processQueue (rest ++ neighbours)

  -- Start the search
  processQueue [MA.Ix2 x y]

isRectangleInside :: Points s -> Rectangle -> ST s Bool
isRectangleInside points rectangle =
  let pointsOfRectangle = rectanglePoints rectangle
  in  allM (isPointInsidePoints points) pointsOfRectangle

solvePart2 :: IO Int
solvePart2 = do
  points <- either (error . show) pure $ parseLines input
  let rectangles = rectanglesFromPoints points
  putStrLn "rectangles"
  let pointEdges = edgesFromPoints points
  putStrLn "pointEdges"
  innerPoint <- maybe (error "No inner point found.") pure $ findRightAngleInnerPoint pointEdges
  let minX = minimum $ fmap fst points
  let minY = minimum $ fmap snd points
  let adjustedInnerPoint = (fst innerPoint - minX, snd innerPoint - minY)
  putStrLn "innerPoint"
  let !filteredRectangles = runST $ do
        pointsTable <- fillPointsFromEdges pointEdges
        floodFill pointsTable adjustedInnerPoint
        filterM (isRectangleInside pointsTable) $ traceWith (\r -> "isRectangleInside: " <> show (length r)) rectangles
  putStrLn "filteredRectangles"
  let maxRectangle = maximumByMay compareRectangles filteredRectangles
  pure $ maybe 0 rectangleArea maxRectangle

solve :: IO ()
solve = do
  presentResult 9 1 solvePart1
  presentResult 9 2 solvePart2