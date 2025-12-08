{-# LANGUAGE TemplateHaskell #-}

module AdventOfCode.Day08.Input where

import Data.FileEmbed (embedStringFile)

input :: String
input = $(embedStringFile "src/AdventOfCode/Day08/input.txt")

