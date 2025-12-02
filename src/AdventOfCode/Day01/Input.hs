{-# LANGUAGE TemplateHaskell #-}

module AdventOfCode.Day01.Input where

import Data.FileEmbed (embedStringFile)

input :: String
input = $(embedStringFile "src/AdventOfCode/Day01/input.txt")

