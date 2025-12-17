{-# LANGUAGE TemplateHaskell #-}

module AdventOfCode.Day09.Input where

import Data.FileEmbed (embedStringFile)

input :: String
input = $(embedStringFile "src/AdventOfCode/Day09/input.txt")

