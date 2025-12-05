{-# LANGUAGE TemplateHaskell #-}

module AdventOfCode.Day05.Input where

import Data.FileEmbed (embedStringFile)

input :: String
input = $(embedStringFile "src/AdventOfCode/Day05/input.txt")

