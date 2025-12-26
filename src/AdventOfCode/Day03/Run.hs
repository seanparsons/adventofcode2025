module AdventOfCode.Day03.Run where

import Debug.Trace
import AdventOfCode.Day03.Input
import AdventOfCode.Utils
import Data.Monoid
import Data.Semigroup
import Data.Maybe
import Safe
import Control.Monad

data Bank = Bank [Int]
  deriving (Show, Eq)

parseBank :: String -> Either String Bank
parseBank bankString = fmap Bank $ mapM parseInt $ map (: []) bankString

banksFromInput :: Either String [Bank]
banksFromInput = mapM parseBank $ lines input

maxJoltageFromBatteriesPart1 :: [Int] -> Int
maxJoltageFromBatteriesPart1 [] = 0
maxJoltageFromBatteriesPart1 [_] = 0
maxJoltageFromBatteriesPart1 (first : rest) =
  let firstTimesTen = first * 10
      second = maximum rest
   in max (firstTimesTen + second) (maxJoltageFromBatteriesPart1 rest)

maxJoltagePart1 :: Bank -> Int
maxJoltagePart1 (Bank banks) = maxJoltageFromBatteriesPart1 banks

maxWithPos :: [Int] -> (Int, Int)
maxWithPos bank =
  foldl'
    (\(max, pos) (curr, pos') -> if curr > max then (curr, pos') else (max, pos))
    (0, 0)
    (zip bank [0 ..])

pickLargest :: Int -> [Int] -> [Int]
pickLargest 0 _ = []
pickLargest n bank' =
  let takeUntil = length bank' - n + 1
      (maxVal, pos) = maxWithPos (take takeUntil bank')
      restBank = drop (pos + 1) bank'
    in maxVal : pickLargest (n - 1) restBank

maxJoltagePart2 :: Int -> Bank -> Int
maxJoltagePart2 n (Bank bank) = foldl' (\acc d -> acc * 10 + d) 0 (pickLargest n bank)

solvePart1 :: IO Int
solvePart1 = do
  banks <- either (error . show) pure banksFromInput
  pure $ sum $ fmap maxJoltagePart1 banks

solvePart2 :: IO Int
solvePart2 = do
  banks <- either (error . show) pure banksFromInput
  pure $ getSum $ foldMap Sum $ fmap (maxJoltagePart2 12) banks

solve :: [AOCUncomputedResult]
solve =
  [ AOCUncomputedResult 3 1 solvePart1
  , AOCUncomputedResult 3 2 solvePart2
  ]