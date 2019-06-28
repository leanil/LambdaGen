cmake_minimum_required(VERSION 3.12)

project(LambdaGen_benchmark LANGUAGES CXX)

set(GOOGLE_BENCH_LOC D:/Programs/google_bench/Release CACHE PATH "Google benchmark library (release build)")
set(GOOGLE_BENCH_DEBUG_LOC D:/Programs/google_bench/Debug CACHE PATH "Google benchmark library (debug build)")
    
set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)

function(add_benchmark num)

    set(BENCH bench${num})
    set(MAIN src/main${num}.cpp)
    list(TRANSFORM ARGN PREPEND src/)
    list(TRANSFORM ARGN APPEND .h OUTPUT_VARIABLE CONT_H)
    list(TRANSFORM ARGN APPEND .cpp OUTPUT_VARIABLE CONT_CPP)

    add_executable(${BENCH}
                   ${MAIN}
                   ../tensor/List.h
                   ../tensor/View.h
                   ../tensor/util.hpp
                   ${CONT_H}
                   ${CONT_CPP})

    target_include_directories(${BENCH} PRIVATE 
                               ../tensor
                               src
                               ${GOOGLE_BENCH_LOC}/include)
                            
    target_link_libraries(${BENCH} debug ${GOOGLE_BENCH_DEBUG_LOC}/lib/benchmark.lib)
    target_link_libraries(${BENCH} optimized ${GOOGLE_BENCH_LOC}/lib/libbenchmark.a Threads::Threads)
    target_link_libraries(${BENCH} shlwapi.lib)
    
    set_target_properties(${BENCH} PROPERTIES
                          CXX_STANDARD 14
                          CXX_STANDARD_REQUIRED ON)

    set(OUT_JSON ${CMAKE_SOURCE_DIR}/data/times${num}.json)
    set(TIME_FILES ${TIME_FILES} ${OUT_JSON} PARENT_SCOPE)
    add_custom_command(OUTPUT ${OUT_JSON}
                       COMMAND ${BENCH} --benchmark_repetitions=5 --benchmark_out=${OUT_JSON}
                                        --benchmark_out_format=json --benchmark_report_aggregates_only=true
                       DEPENDS ${BENCH}
                       COMMENT "Running benchmark ${num}")
endfunction()

include(add_executables.txt)

if(WIN32)
    set(RUN_PYTHON py)
else()
    set(RUN_PYTHON python3)
endif()

add_custom_target(data ${RUN_PYTHON} compose_data.py ${TIME_FILES}
                  DEPENDS ${TIME_FILES}
                  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/data)
                      
