{-# LANGUAGE TemplateHaskell #-}

module AdventOfCode.Day07.Input where

import Data.FileEmbed (embedStringFile)

input :: String
input = $(embedStringFile "src/AdventOfCode/Day07/input.txt")

