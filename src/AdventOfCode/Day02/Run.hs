module AdventOfCode.Day02.Run where

import AdventOfCode.Day02.Input
import AdventOfCode.Utils
import Data.List.Extra

data Range = Range Integer Integer
  deriving (Show, Eq)

parseRange :: String -> Either String Range
parseRange rangeString = case splitOn "-" rangeString of
  [start, end] -> Range <$> parseInteger start <*> parseInteger end
  _ -> Left "Invalid range"

parseRanges :: Either String [Range]
parseRanges = mapM parseRange $ splitOn "," input

rangeNumbers :: Range -> [Integer]
rangeNumbers (Range start end) = [start..end]

isNumberInvalidPart1 :: Integer -> Bool
isNumberInvalidPart1 n =
  let asString = show n
      (firstHalf, secondHalf) = splitAt (length asString `div` 2) asString
  in  firstHalf == secondHalf

checkStringLengthRepeated :: String -> Int -> Bool
checkStringLengthRepeated s lengthToTest =
  let segment = take lengthToTest s
      repeated = concat $ replicate (length s `div` lengthToTest) segment
  in  s == repeated

isNumberInvalidPart2 :: Integer -> Bool
isNumberInvalidPart2 n =
  let asString = show n
      stringLength = length asString
      lengthsToTest = filter (\l -> mod stringLength l == 0) [1..stringLength `div` 2]
  in  any (checkStringLengthRepeated asString) lengthsToTest

testPart2 :: Integer -> IO ()
testPart2 n = do
  print ("test: " <> show n, isNumberInvalidPart2 n)

solve :: IO ()
solve = do
  ranges <- either (error . show) pure parseRanges
  let invalidNumbersPart1 = filter isNumberInvalidPart1 $ concatMap rangeNumbers ranges
  putStrLn $ "Day 02 - 1: " <> show (sum invalidNumbersPart1)
  let invalidNumbersPart2 = filter isNumberInvalidPart2 $ concatMap rangeNumbers ranges
  putStrLn $ "Day 02 - 2: " <> show (sum invalidNumbersPart2)