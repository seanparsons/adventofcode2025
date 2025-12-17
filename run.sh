#!/usr/bin/env bash
set -e
clear

# Generate .cabal file from package.yaml using hpack
echo "Generating .cabal file from package.yaml..."
hpack

# Build and run with CPU profiling enabled
echo "Building..."
cabal build aoc2025

# Run the Advent of Code 2025 project with multiple cores
echo "Running..."
cabal run aoc2025 -- +RTS -N -RTS