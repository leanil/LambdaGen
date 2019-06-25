cmake_minimum_required(VERSION 3.5)

project(LambdaGen_benchmark LANGUAGES CXX)

set(GOOGLE_BENCH_LOC D:/Programs/google_bench/Release CACHE PATH "Google benchmark library (release build)")
set(GOOGLE_BENCH_DEBUG_LOC D:/Programs/google_bench/Debug CACHE PATH "Google benchmark library (debug build)")

FILE(GLOB CONTRACTIONS contraction/*)

add_executable(bench main.cpp
                     ../tensor/List.h
                     ../tensor/View.h
                     ../tensor/util.hpp
                     ${CONTRACTIONS})

target_include_directories(bench PRIVATE 
                           ../tensor
                           contraction
                           ${GOOGLE_BENCH_LOC}/include)
                        
target_link_libraries(bench debug ${GOOGLE_BENCH_DEBUG_LOC}/lib/benchmark.lib)
target_link_libraries(bench optimized ${GOOGLE_BENCH_LOC}/lib/benchmark.lib)
target_link_libraries(bench shlwapi.lib)

set_target_properties(bench PROPERTIES
                      CXX_STANDARD 14
                      CXX_STANDARD_REQUIRED ON)

if(MSVC)
    set(RUN_BENCH ${CMAKE_BINARY_DIR}/Release/bench.exe)
else()
    set(RUN_BENCH ${CMAKE_BINARY_DIR}/bench)
endif()

if(WIN32)
    set(RUN_PYTHON py)
else()
    set(RUN_PYTHON python3)
endif()

message(${RUN_BENCH})

add_custom_command(TARGET bench POST_BUILD
                   COMMAND ${RUN_BENCH} --benchmark_repetitions=5 --benchmark_out=times.json
                                        --benchmark_out_format=json --benchmark_report_aggregates_only=true
                   COMMAND ${RUN_PYTHON} compose_data.py
                   WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/data
                   COMMENT "Running benchmarks and composing data file...")
                      