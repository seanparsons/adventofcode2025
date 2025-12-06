module AdventOfCode.Day06.Run where

import AdventOfCode.Day06.Input
import AdventOfCode.Utils
import Data.List.Extra
import Data.Foldable
import Data.Char
import Control.Monad
import Debug.Trace

data Operation = OpMul | OpAdd
  deriving (Show, Eq)

type NumberRow = [Int]

type OperationRow = [Operation]

data Calculation = Calculation [Int] Operation
  deriving (Show, Eq)

type Input = [Calculation]

parseNumberRowPart1 :: String -> Either String NumberRow
parseNumberRowPart1 numberRowString =
  let numberStrings = words numberRowString
  in  mapM parseInt numberStrings

parseOperation :: String -> Either String Operation
parseOperation "*" = Right OpMul
parseOperation "+" = Right OpAdd
parseOperation operation = Left $ "Invalid operation: " <> operation

parseOperationRow :: String -> Either String OperationRow
parseOperationRow operationRowString =
  let operationStrings = words operationRowString
  in  mapM parseOperation operationStrings

parseInputPart1 :: String -> Either String Input
parseInputPart1 inputString = parseInputRowsPart1 $ reverse $ lines inputString

parseInputRowsPart1 :: [String] -> Either String Input
parseInputRowsPart1 (lastRow : restRows) = do
  operationRow <- parseOperationRow lastRow
  numberRows <- mapM parseNumberRowPart1 $ reverse restRows
  let numberColumns = transpose numberRows
  pure $ zipWith Calculation numberColumns operationRow
parseInputRowsPart1 [] = Left "No input rows."

calculateCalculation :: Calculation -> Int
calculateCalculation (Calculation numbers OpMul) = product numbers
calculateCalculation (Calculation numbers OpAdd) = sum numbers

calculateInput :: Input -> Int
calculateInput inputEntries = sum $ fmap calculateCalculation inputEntries

testInput :: [String]
testInput =
  [ "123 328  51 64 "
  , " 45 64  387 23 "
  , "  6 98  215 314"
  , "*   +   *   +  "
  ]

data WorkingCalculation = WorkingCalculation [Int] (Maybe Operation)
  deriving (Show, Eq)

combineMaybeOperation :: Maybe Operation -> Maybe Operation -> Maybe Operation
combineMaybeOperation Nothing Nothing = Nothing
combineMaybeOperation (Just operation1) Nothing = Just operation1
combineMaybeOperation Nothing (Just operation2) = Just operation2
-- This case is a bit of a kludge, but it just means two operators in a column is invalid.
combineMaybeOperation (Just _) (Just _) = Nothing

instance Semigroup WorkingCalculation where
  WorkingCalculation numbers1 operation1 <> WorkingCalculation numbers2 operation2 = WorkingCalculation (numbers1 ++ numbers2) (combineMaybeOperation operation1 operation2)

instance Monoid WorkingCalculation where
  mempty = WorkingCalculation [] Nothing

parseReversedRowPart2 :: String -> Either String WorkingCalculation
parseReversedRowPart2 ('*' : rest) = do
  number <- parseInt $ reverse rest
  pure $ WorkingCalculation [number] (Just OpMul)
parseReversedRowPart2 ('+' : rest) = do
  number <- parseInt $ reverse rest
  pure $ WorkingCalculation [number] (Just OpAdd)
parseReversedRowPart2 rest = do
  number <- parseInt $ reverse rest
  pure $ WorkingCalculation [number] Nothing

isEmptyString :: String -> Bool
isEmptyString "" = True
isEmptyString stringValue = all isSpace stringValue

parseRowPart2 :: String -> Either String WorkingCalculation
-- Filter out whitespace as that causes grief, then reverse it so that any operator is the first character.
parseRowPart2 rowString = parseReversedRowPart2 $ reverse $ filter (not . isSpace) rowString

parseFold :: ([WorkingCalculation], WorkingCalculation) -> String -> Either String ([WorkingCalculation], WorkingCalculation)
parseFold (calculations, currentCalculation) value =
      -- If the row is just whitespace, that signifies the gap between calculations.
  let ifEmpty = Right (calculations <> [currentCalculation], mempty)
      -- If the row is not whitespace, then parse it and add it to the current calculation.
      notEmpty = do
        parsedCalculation <- parseRowPart2 value
        pure (calculations, currentCalculation <> parsedCalculation)
  in  if isEmptyString value then ifEmpty else notEmpty

workingToCalculation :: WorkingCalculation -> Either String Calculation
-- The only thing this really does is check for the operation.
workingToCalculation (WorkingCalculation numbers (Just operation)) = pure $ Calculation numbers operation
workingToCalculation (WorkingCalculation _ Nothing) = Left "Invalid working calculation."

parseRowsPart2 :: [String] -> Either String [Calculation]
parseRowsPart2 inputRows = do
  -- Transpose the input so that rows become columns and vice versa.
  -- Then reverse what were the columns, so that we're reading from right to left.
  let transposedInput = reverse $ transpose inputRows
  -- Work our way through the columns, building up the calculations as we go.
  (calculations, currentCalculation) <- foldM parseFold ([], mempty) transposedInput
  -- Add the final calculation to the list of calculations.
  let workingCalculations = calculations <> [currentCalculation]
  -- Attempt to turn the working calculations into real calculations.
  traverse workingToCalculation workingCalculations

solve :: IO ()
solve = do
  testInputPart1 <- either (error . show) pure (parseInputPart1 input)
  putStrLn ("Day 06 - 1: " <> show (calculateInput testInputPart1))
  --testInputPart2 <- either (error . show) pure (parseRowsPart2 testInput)
  testInputPart2 <- either (error . show) pure (parseRowsPart2 $ lines input)
  putStrLn ("Day 06 - 2: " <> show (calculateInput testInputPart2))
  