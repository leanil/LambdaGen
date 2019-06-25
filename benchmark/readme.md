# How to benchmark

## Requirements

* [Google benchmark](https://github.com/google/benchmark)
* [CMake](https://cmake.org/)
* [Stack](https://docs.haskellstack.org) for building LambdaGen

## Usage

1. Set the location of the Google benchmark library (`-DGOOGLE_BENCH_LOC` or directly in `CMakeLists.txt`).

2. Generate benchmarks (`stack bench`), which exports the random contractions to json, then generates, builds and runs the C++ benchmarking code.

3. The collected measurements are stored in JSON files in the data folder.
