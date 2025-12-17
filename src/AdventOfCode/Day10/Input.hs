{-# LANGUAGE TemplateHaskell #-}

module AdventOfCode.Day10.Input where

import Data.FileEmbed (embedStringFile)

input :: String
input = $(embedStringFile "src/AdventOfCode/Day10/input.txt")

