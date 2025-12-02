{-# LANGUAGE TemplateHaskell #-}

module AdventOfCode.Day02.Input where

import Data.FileEmbed (embedStringFile)

input :: String
input = $(embedStringFile "src/AdventOfCode/Day02/input.txt")

