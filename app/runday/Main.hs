module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

import qualified AdventOfCode.Day01.Run as Day01
import qualified AdventOfCode.Day02.Run as Day02
import qualified AdventOfCode.Day03.Run as Day03
import qualified AdventOfCode.Day04.Run as Day04
import qualified AdventOfCode.Day05.Run as Day05
import qualified AdventOfCode.Day06.Run as Day06
import qualified AdventOfCode.Day07.Run as Day07
import qualified AdventOfCode.Day08.Run as Day08
import qualified AdventOfCode.Day09.Run as Day09
import qualified AdventOfCode.Day10.Run as Day10
import qualified AdventOfCode.Day11.Run as Day11
import qualified AdventOfCode.Day12.Run as Day12

runDay :: Int -> IO ()
runDay 1  = Day01.solve
runDay 2  = Day02.solve
runDay 3  = Day03.solve
runDay 4  = Day04.solve
runDay 5  = Day05.solve
runDay 6  = Day06.solve
runDay 7  = Day07.solve
runDay 8  = Day08.solve
runDay 9  = Day09.solve
runDay 10 = Day10.solve
runDay 11 = Day11.solve
runDay 12 = Day12.solve
runDay n  = putStrLn $ "Error: Day " ++ show n ++ " does not exist. Valid days are 1-12."

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dayStr] -> case readMaybe dayStr of
      Just day | day >= 1 && day <= 12 -> do
        putStrLn $ "=== Running Day " ++ show day ++ " ==="
        putStrLn ""
        runDay day
      _ -> do
        putStrLn "Error: Invalid day number. Please provide a day between 1 and 12."
        exitFailure
    _ -> do
      putStrLn "Usage: runday <day-number>"
      putStrLn "Example: runday 1"
      exitFailure
