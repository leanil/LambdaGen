cmake_minimum_required(VERSION 3.12)

project(LambdaGen_benchmark LANGUAGES CXX)
    
set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)
find_package(Python3 REQUIRED COMPONENTS Interpreter)

get_filename_component(GOOGLE_BENCH_DIR ../dependencies/google_benchmark ABSOLUTE)
option(BENCHMARK_ENABLE_GTEST_TESTS "Enable building the unit tests which depend on gtest" OFF)
add_subdirectory(${GOOGLE_BENCH_DIR}/src ${GOOGLE_BENCH_DIR}/build)

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

    target_include_directories(${BENCH} PRIVATE ../tensor)
                            
    target_link_libraries(${BENCH} benchmark Threads::Threads)
    if(WIN32)
        target_link_libraries(${BENCH} shlwapi.lib)
    endif()
    
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

add_custom_target(data ${Python3_EXECUTABLE} compose_data.py ${TIME_FILES}
                  DEPENDS ${TIME_FILES}
                  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/data)
                      
