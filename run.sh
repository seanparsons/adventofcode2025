#!/usr/bin/env bash
set -e

# Generate .cabal file from package.yaml using hpack
echo "Generating .cabal file from package.yaml..."
hpack

# Run the Advent of Code 2025 project using Nix flakes
cabal run aoc2025
