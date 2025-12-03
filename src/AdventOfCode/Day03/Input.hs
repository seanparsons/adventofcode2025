{-# LANGUAGE TemplateHaskell #-}

module AdventOfCode.Day03.Input where

import Data.FileEmbed (embedStringFile)

input :: String
input = $(embedStringFile "src/AdventOfCode/Day03/input.txt")

