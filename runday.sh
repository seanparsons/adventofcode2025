#!/usr/bin/env bash
set -e

# Generate .cabal file from package.yaml using hpack
echo "Generating .cabal file from package.yaml..."
hpack

# Build and run with CPU profiling enabled
echo "Building with profiling enabled..."
cabal build --enable-profiling --flags=prof runday

# Run the Advent of Code 2025 project with profiling and multiple cores
echo "Running with CPU profiling..."
cabal run --enable-profiling --flags=prof runday -- +RTS -N -p -RTS 9

echo ""
echo "Profiling complete! Check aoc2025.prof for results."
