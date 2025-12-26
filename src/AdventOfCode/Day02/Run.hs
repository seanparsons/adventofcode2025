module AdventOfCode.Day02.Run where

import AdventOfCode.Day02.Input
import AdventOfCode.Utils
import Data.List.Extra

data Range = Range Int Int
  deriving (Show, Eq)

parseRange :: String -> Either String Range
parseRange rangeString = case splitOn "-" rangeString of
  [start, end] -> Range <$> parseInt start <*> parseInt end
  _ -> Left "Invalid range"

parseRanges :: Either String [Range]
parseRanges = mapM parseRange $ splitOn "," input

rangeNumbers :: Range -> [Int]
rangeNumbers (Range start end) = [start..end]

isNumberInvalidPart1 :: Int -> Bool
isNumberInvalidPart1 n =
  let asString = show n
      (firstHalf, secondHalf) = splitAt (length asString `div` 2) asString
  in  firstHalf == secondHalf

checkStringLengthRepeated :: String -> Int -> Bool
checkStringLengthRepeated s lengthToTest =
  let segment = take lengthToTest s
      repeated = concat $ replicate (length s `div` lengthToTest) segment
  in  s == repeated

isNumberInvalidPart2 :: Int -> Bool
isNumberInvalidPart2 n =
  let asString = show n
      stringLength = length asString
      lengthsToTest = filter (\l -> mod stringLength l == 0) [1..stringLength `div` 2]
  in  any (checkStringLengthRepeated asString) lengthsToTest

solvePart1 :: IO Int
solvePart1 = do
  ranges <- either (error . show) pure parseRanges
  let invalidNumbersPart1 = filter isNumberInvalidPart1 $ concatMap rangeNumbers ranges
  pure $ sum invalidNumbersPart1

solvePart2 :: IO Int
solvePart2 = do
  ranges <- either (error . show) pure parseRanges
  let invalidNumbersPart2 = filter isNumberInvalidPart2 $ concatMap rangeNumbers ranges
  pure $ sum invalidNumbersPart2

solve :: [AOCUncomputedResult]
solve =
  [ AOCUncomputedResult 2 1 solvePart1
  , AOCUncomputedResult 2 2 solvePart2
  ]