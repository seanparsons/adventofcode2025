{-# LANGUAGE TemplateHaskell #-}

module AdventOfCode.Day04.Input where

import Data.FileEmbed (embedStringFile)

input :: String
input = $(embedStringFile "src/AdventOfCode/Day04/input.txt")

