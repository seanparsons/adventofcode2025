module Main where

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

main :: IO ()
main = do
  putStrLn "=== Advent of Code 2025 ==="
  putStrLn ""
  Day01.solve
  Day02.solve
  Day03.solve
  Day04.solve
  Day05.solve
  Day06.solve
  Day07.solve
  Day08.solve
  Day09.solve
  Day10.solve
  Day11.solve
  Day12.solve
  putStrLn ""
  putStrLn "=== All days completed ==="
