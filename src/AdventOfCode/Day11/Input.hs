{-# LANGUAGE TemplateHaskell #-}

module AdventOfCode.Day11.Input where

import Data.FileEmbed (embedStringFile)

input :: String
input = $(embedStringFile "src/AdventOfCode/Day11/input.txt")

