cmake_minimum_required(VERSION 3.5)

project(LambdaGen_benchmark LANGUAGES CXX)

FILE(GLOB CONTRACTIONS contraction/*)

add_executable(bench main.cpp
                     ../tensor/List.h
                     ../tensor/View.h
                     ../tensor/util.hpp
                     ${CONTRACTIONS})

target_include_directories(bench PRIVATE 
                           ../tensor
                           contraction
                           D:/Programs/google_bench/Debug/include)
                        
target_link_libraries(bench debug D:/Programs/google_bench/Debug/lib/benchmark.lib)
target_link_libraries(bench optimized D:/Programs/google_bench/Release/lib/benchmark.lib)
target_link_libraries(bench shlwapi.lib)

set_target_properties(bench PROPERTIES
                      CXX_STANDARD 14
                      CXX_STANDARD_REQUIRED ON)
                      