{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module AdventOfCode.Day12.Run where

import AdventOfCode.Day12.Input
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
import Debug.Trace
import GHC.Generics
import Data.Monoid

testInput :: String
testInput = """
0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2
"""

type ShapePoints = S.HashSet (Int, Int)

newtype PresentShape = PresentShape
                     { presentShapePoints :: ShapePoints
                     }
                     deriving (Show, Eq, Ord, Generic)

instance Hashable PresentShape where

shapeSize :: PresentShape -> Int
shapeSize (PresentShape points) = S.size points

maxDimension :: Int
maxDimension = 2

flipShapeVertically :: PresentShape -> PresentShape
flipShapeVertically (PresentShape points) = PresentShape $ S.map (\(x, y) -> (x, maxDimension - y)) points

flipShapeHorizontally :: PresentShape -> PresentShape
flipShapeHorizontally (PresentShape points) = PresentShape $ S.map (\(x, y) -> (maxDimension - x, y)) points

rotateShapeClockwise :: PresentShape -> PresentShape
rotateShapeClockwise (PresentShape points) = PresentShape $ S.map (\(x, y) -> (maxDimension - y, x)) points

getRotations :: PresentShape -> [PresentShape]
getRotations shape =
  let oneRotation = rotateShapeClockwise shape
      twoRotations = rotateShapeClockwise oneRotation
      threeRotations = rotateShapeClockwise twoRotations
  in  [shape, oneRotation, twoRotations, threeRotations]

shiftShape :: PresentShape -> (Int, Int) -> PresentShape
shiftShape (PresentShape points) (xShift, yShift) = PresentShape $ S.map (\(x, y) -> (x + xShift, y + yShift)) points

getAllVariants :: Region ->ShapePoints -> ShapePoints -> PresentShape -> S.HashSet PresentShape
getAllVariants Region{..} regionPoints currentPoints shape =
  let rotationVariants = do
        vertical <- [shape, flipShapeVertically shape]
        horizontal <- [vertical, flipShapeHorizontally vertical]
        getRotations horizontal
      remainingPoints = S.toList $ S.difference regionPoints currentPoints
      filteredRemainingPoints = filter (\(x, y) -> x < regionWidth - maxDimension && y < regionHeight - maxDimension) remainingPoints
  in  S.fromList $ do
        rotationVariant <- rotationVariants
        remainingPoint <- filteredRemainingPoints
        pure $ shiftShape rotationVariant remainingPoint

data Region = Region
            { regionWidth :: Int
            , regionHeight :: Int
            , regionQuantities :: M.HashMap Int Int
            }
            deriving (Show, Eq, Ord)

data Input = Input
           { inputPresentShapes :: M.HashMap Int PresentShape
           , inputRegions :: [Region]
           }
           deriving (Show, Eq, Ord)

regionQuantitiesSize :: Input -> Region -> Int
regionQuantitiesSize inputForTest region =
  let  foldEntries working shapeIndex quantity = working + shapeSize (getShapeFromInput inputForTest shapeIndex) * quantity
  in   M.foldlWithKey' foldEntries 0 (regionQuantities region)

parseRegion :: String -> Either String Region
parseRegion regionString = case splitOn ": " regionString of
  [dimensions, quantities] -> case (splitOn "x" dimensions, words quantities) of
    ([width, height], quantitiesList) -> do
      widthInt <- parseInt width
      heightInt <- parseInt height
      quantitiesInt <- mapM parseInt quantitiesList
      return $ Region widthInt heightInt (M.fromList $ zip [0..] quantitiesInt)
    _ -> Left "Invalid region string."
  _ -> Left "Invalid region string."

parseShapeLine :: (String, Int) -> Either String (S.HashSet (Int, Int))
parseShapeLine (shapeLine, y) =
  let xPositions = map snd $ filter (\(c, _) -> c == '#') $ zip shapeLine [0..]
  in  Right $ S.fromList $ zip xPositions (repeat y)

parseShapeAndIndex :: [String] -> Either String (Int, PresentShape)
parseShapeAndIndex (indexLine : shapeLines) = case splitOn ":" indexLine of
  [indexValue, _] -> do
    indexInt <- parseInt indexValue
    let zippedShapeLines = zip shapeLines [0..]
    shapePoints <- mapM parseShapeLine zippedShapeLines
    pure (indexInt, PresentShape (mconcat shapePoints))
  _ -> Left "Invalid shape and index string."
parseShapeAndIndex _ = Left "Invalid shape and index string."

parseRegions :: [String] -> Either String [Region]
parseRegions = mapM parseRegion

parseInput :: String -> Either String Input
parseInput inputString = do
  let inputLines = lines inputString
  let inputChunks = splitOn [""] inputLines
  let reversedChunks = reverse inputChunks
  case reversedChunks of
    (regionLines : rest) -> do
      let rereversedRest = reverse rest
      shapesAndIndices <- mapM parseShapeAndIndex rereversedRest
      let shapesMap = M.fromList shapesAndIndices
      regions <- parseRegions regionLines
      pure $ Input shapesMap regions
    _ -> Left "Invalid input."

getInput :: String -> IO Input
getInput = either (error . show) pure . parseInput

getShapeFromInput :: Input -> Int -> PresentShape
getShapeFromInput inputForTest shapeIndex = fromMaybe (error $ "Shape not found: " <> show shapeIndex) $ M.lookup shapeIndex $ inputPresentShapes inputForTest

getShapesToFit :: Input -> Region -> [PresentShape]
getShapesToFit inputForTest region =
  let shapeKeys = concatMap (\(shapeKey, quantity) -> replicate quantity shapeKey) $ M.toList $ regionQuantities region
      lookupShape = getShapeFromInput inputForTest
  in  fmap lookupShape shapeKeys

shapePointsDoNotOverlap :: ShapePoints -> ShapePoints -> Bool
shapePointsDoNotOverlap points1 points2 = S.null $ S.intersection points1 points2

tryShapesInRegion :: Region -> ShapePoints -> ShapePoints -> [PresentShape] -> Bool
tryShapesInRegion _ _ _ [] = True
tryShapesInRegion region regionPoints currentPoints (firstShape : restShapes) = or $ do
  shapeVariant <- filter (\PresentShape{..} -> shapePointsDoNotOverlap currentPoints presentShapePoints) $ S.toList $ getAllVariants region regionPoints currentPoints firstShape
  pure $ tryShapesInRegion region regionPoints (currentPoints <> presentShapePoints shapeVariant) restShapes

getRegionPoints :: Region -> ShapePoints
getRegionPoints Region{..} = S.fromList $ do
  x <- [0..regionWidth - 1]
  y <- [0..regionHeight - 1]
  pure (x, y)

isRegionPossiblyLargeEnough :: Input -> Region -> Bool
isRegionPossiblyLargeEnough inputForTest region@Region{..} =
  let regionSizeRequired = regionQuantitiesSize inputForTest region
      maxRegionSize = regionWidth * regionHeight
  in  regionSizeRequired <= maxRegionSize

doesRegionHaveEnoughSpaceToFitShapesLazily :: Region -> Bool
doesRegionHaveEnoughSpaceToFitShapesLazily Region{..} = do
  let total3x3ShapesRequired = sum regionQuantities
  let total3x3ShapesAvailable = (regionWidth `div` 3) * (regionHeight `div` 3)
  total3x3ShapesRequired <= total3x3ShapesAvailable

canRegionFitShapes :: Input -> Region -> Bool
canRegionFitShapes inputForTest region =
  let checkPossiblyLargeEnough = isRegionPossiblyLargeEnough inputForTest region
      checkLazily = doesRegionHaveEnoughSpaceToFitShapesLazily region
      checkShapesCanFit =
        let shapesToFit = getShapesToFit inputForTest region
            regionPoints = getRegionPoints region
        in  tryShapesInRegion region regionPoints mempty shapesToFit
      checkLazilyFallback = checkPossiblyLargeEnough && checkShapesCanFit
  in  checkLazily || checkLazilyFallback

solvePart1 :: IO Int
solvePart1 = do
  inputValues <- getInput input
  let regionsThatFit = filter (canRegionFitShapes inputValues) $ inputRegions inputValues
  pure $ length regionsThatFit

solve :: [AOCUncomputedResult]
solve =
  [ AOCUncomputedResult 12 1 solvePart1
  ]
