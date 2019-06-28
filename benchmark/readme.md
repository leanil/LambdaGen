# How to benchmark

## Requirements

* [CMake](https://cmake.org/)
* [Stack](https://docs.haskellstack.org) for building LambdaGen

## Usage

1. Configure the random expression generation in `Benchmark.hs`.

2. Run benchmarks (`stack bench`), which

    * exports the random generated contractions to json

    * generates, builds and runs the C++ benchmarking code

    * composes the expression trees with the measured execution times

3. The collected measurements are stored in JSON files in the data folder.
