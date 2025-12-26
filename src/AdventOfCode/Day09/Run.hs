{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module AdventOfCode.Day09.Run (solve) where

import AdventOfCode.Day09.Input
import AdventOfCode.Utils
import Data.List.Extra
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.HashTable.Class ()
import Safe.Foldable
import Data.Interval
import qualified Data.IntervalMap.Strict as IVM
import qualified Data.IntervalSet as IS
import qualified Data.Interval as DI

type Point = (Int, Int)

type Rectangle = (Point, Point)

data Edge = VerticalEdge Int Int Int
          | HorizontalEdge Int Int Int
          deriving (Show, Eq, Ord)

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
-- | x coords to the y coordinates they cointain
regions :: [Point] -> IVM.IntervalMap Int (IS.IntervalSet Int)
regions pts = IVM.fromList $ zip (drop 1 xRanges) yRanges
  where
    -- the (sorted) x's of all points, with the y's they are at.
    xs :: [(Int, S.HashSet Int)]
    xs =
      sortOn fst $
        M.toList $
          M.fromListWith
            (<>)
            [ (x, S.singleton y)
            | (x, y) <- pts
            ]
    xRanges :: [Interval Int]
    yRanges :: [IS.IntervalSet Int]
    (xRanges, yRanges) = unzip $ scanl' go (NegInf <..< Finite 0, IS.empty) xs
      where
        go (i0, curr) (x, ys) = (DI.upperBound i0 <=..< Finite x, curr')
          where
            curr' = (curr `IS.union` ivs) `IS.difference` (curr `IS.intersection` ivs)
            ivs =
              IS.fromList
                [ Finite a <=..< Finite b
                | [a, b] <- chunksOf 2 (sort $ S.toList ys)
                ]

part2 :: [Point] -> Int
part2 pts = maximum
    [ rectangleArea (p, q)
    | p@(px, py) <- pts
    , q@(qx, qy) <- pts
    , let xRange = Finite (min px qx) <=..< Finite (max px qx)
          yRange = Finite (min py qy) <=..< Finite (max py qy)
          region = IVM.singleton xRange (IS.singleton yRange)
          outOfBounds = IVM.intersectionWith IS.difference region allowedRegion
    , all IS.null outOfBounds
    ]
  where
    allowedRegion = regions pts

solvePart2 :: IO Int
solvePart2 = do
  points <- either (error . show) pure $ parseLines input
  pure $ part2 points

solve :: [AOCUncomputedResult]
solve =
  [ AOCUncomputedResult 9 1 solvePart1
  , AOCUncomputedResult 9 2 solvePart2
  ]