{-# LANGUAGE TemplateHaskell #-}

module AdventOfCode.Day06.Input where

import Data.FileEmbed (embedStringFile)

input :: String
input = $(embedStringFile "src/AdventOfCode/Day06/input.txt")

