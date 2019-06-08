# How to benchmark

## Requirements

* [Google benchmark](https://github.com/google/benchmark)
* [CMake](https://cmake.org/)
* [Stack](https://docs.haskellstack.org) for building LambdaGen

## Usage

1. Update the location of the Google benchmark library in `CMakeLists.txt`.

2. Run `Benchmark.hs` (e.g. `stack runhaskell app/Benchmark.hs`), which exports the random contractions to json, then generates and builds the C++ benchmarking code.

3. Run the `bench` executable (e.g. `build\Release\bench.exe --benchmark_repetitions=5 --benchmark_display_aggregates_only=true`). For other options, like setting time unit and exporting to CSV, check the Google benchmark docs.
