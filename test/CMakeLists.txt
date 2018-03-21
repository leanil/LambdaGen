cmake_minimum_required(VERSION 3.9)

project(LambdaGen LANGUAGES CXX)

FILE(GLOB KERNELS kernel/evaluator*.hpp)
            
add_executable(LambdaGen main.cpp
                         inc/helper.h
                         inc/my_sycl.h
                         inc/View.h
                         inc/tester.hpp
                         ${KERNELS})
                    
target_include_directories(LambdaGen PRIVATE 
                           inc
                           kernel
                           D:/Programs/triSYCL/include/
                           D:/Programs/boost_1_61_0/)

set_target_properties(LambdaGen PROPERTIES
                      C_STANDARD 14
                      C_STANDARD_REQUIRED ON)
                      
enable_testing()
list(LENGTH KERNELS TESTS)
foreach(I RANGE 1 ${TESTS})
  add_test(test${I} LambdaGen ${I})
endforeach(I)