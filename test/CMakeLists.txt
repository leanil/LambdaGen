cmake_minimum_required(VERSION 3.5)

project(LambdaGen LANGUAGES CXX)

FILE(GLOB KERNELS kernel/evaluator*.cpp)
FILE(GLOB CONTRACTIONS contraction/cont*.cpp)

add_executable(LambdaGen main.cpp
                         inc/helper.h
                         inc/List.h
                         inc/my_sycl.h
                         inc/View.h
                         inc/tester.hpp
                         ${KERNELS}
                         ${CONTRACTIONS})
                    
target_include_directories(LambdaGen PRIVATE 
                           inc
                           kernel
                           contraction)

set_target_properties(LambdaGen PROPERTIES
                      CXX_STANDARD 14
                      CXX_STANDARD_REQUIRED ON)
                      
enable_testing()
list(LENGTH KERNELS KERNEL_CNT)
list(LENGTH CONTRACTIONS CONT_CNT)
math(EXPR TEST_CNT ${KERNEL_CNT}+${CONT_CNT})
foreach(I RANGE 1 ${TEST_CNT})
  add_test(test${I} LambdaGen ${I})
endforeach(I)