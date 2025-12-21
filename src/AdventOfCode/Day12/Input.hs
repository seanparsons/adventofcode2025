{-# LANGUAGE TemplateHaskell #-}

module AdventOfCode.Day12.Input where

import Data.FileEmbed (embedStringFile)

input :: String
input = $(embedStringFile "src/AdventOfCode/Day12/input.txt")

