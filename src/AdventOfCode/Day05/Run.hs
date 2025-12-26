module AdventOfCode.Day05.Run where

import AdventOfCode.Day05.Input
import qualified Data.HashSet as S
import Data.List.Extra
import AdventOfCode.Utils

type Range = (Int, Int)

data Database = Database
              { freshRanges :: [Range]
              , ingredients :: S.HashSet Int
              }
              deriving (Show, Eq)

instance Semigroup Database where
  Database freshRanges1 ingredients1 <> Database freshRanges2 ingredients2 = Database (freshRanges1 ++ freshRanges2) (ingredients1 <> ingredients2)

instance Monoid Database where
  mempty = emptyDatabase

databaseFromFresh :: Range -> Database
databaseFromFresh range = Database [range] S.empty

databaseFromIngredient :: Int -> Database
databaseFromIngredient ingredient = Database [] (S.singleton ingredient)

emptyDatabase :: Database
emptyDatabase = Database [] S.empty

parseRange :: String -> Either String Range
parseRange rangeString = case splitOn "-" rangeString of
  [start, end] -> do
    startInt <- parseInt start
    endInt <- parseInt end
    return (startInt, endInt)
  _ -> Left "Invalid range"

parseLine :: String -> Either String Database
parseLine "" = Right emptyDatabase
parseLine line =
  let containsHyphen = isInfixOf "-" line
      fromFresh = (databaseFromFresh <$> parseRange line)
      fromIngredient = (databaseFromIngredient <$> parseInt line)
  in  if containsHyphen then fromFresh else fromIngredient

parseLines :: [String] -> Either String Database
parseLines = fmap mconcat . mapM parseLine

testInputLines :: [String]
testInputLines =
  [ "3-5"
  , "10-14"
  , "16-20"
  , "12-18"
  , ""
  , "1"
  , "5"
  , "8"
  , "11"
  , "17"
  , "32"
  ]

simplifyPair :: Range -> Range -> [Range]
simplifyPair first@(start1, end1) second@(start2, end2) | start2 >= start1 && start2 <= end1 = [(start1, max end1 end2)]
                                                        | end1 + 1 == start2 = [(start1, max end1 end2)]
                                                        | otherwise = [first, second]

simplifySortedRanges :: [Range] -> [Range]
simplifySortedRanges [] = []
simplifySortedRanges [singleRange] = [singleRange]
simplifySortedRanges (first : second : rest) =
  case simplifyPair first second of
    [simplified] -> simplifySortedRanges (simplified : rest)
    [firstSimplified, secondSimplified] -> firstSimplified : simplifySortedRanges (secondSimplified : rest)
    _ -> first : second : simplifySortedRanges rest

simplifyRanges :: [Range] -> [Range]
simplifyRanges ranges =
  let sortedRanges = sort ranges
  in  simplifySortedRanges sortedRanges

simplifyDatabase :: Database -> Database
simplifyDatabase (Database dbFreshRanges dbIngredients) =
  Database (simplifyRanges dbFreshRanges) dbIngredients

freshIngredients :: Database -> S.HashSet Int
freshIngredients (Database dbFreshRanges dbIngredients) =
  S.filter (\i -> any (\(start, end) -> i >= start && i <= end) dbFreshRanges) dbIngredients

freshIngredientsFromRanges :: Database -> Int
freshIngredientsFromRanges (Database dbFreshRanges _) =
  sum $ fmap (\(start, end) -> end - start + 1) dbFreshRanges

solvePart1 :: IO Int
solvePart1 = do
  testDB <- either (error . show) pure (parseLines $ lines input)
  let simplifiedTestDB = simplifyDatabase testDB
  pure $ S.size $ freshIngredients simplifiedTestDB

solvePart2 :: IO Int
solvePart2 = do
  testDB <- either (error . show) pure (parseLines $ lines input)
  let simplifiedTestDB = simplifyDatabase testDB
  pure $ freshIngredientsFromRanges simplifiedTestDB

solve :: [AOCUncomputedResult]
solve =
  [ AOCUncomputedResult 5 1 solvePart1
  , AOCUncomputedResult 5 2 solvePart2
  ]
