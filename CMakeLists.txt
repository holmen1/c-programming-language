cmake_minimum_required(VERSION 3.30)
project(c_programming_language C)

set(CMAKE_C_STANDARD 90)

include_directories(4_Functions_and_Program_Structure/calculator)

add_executable(c_programming_language
    4_Functions_and_Program_Structure/calculator/test_calculator.c
    4_Functions_and_Program_Structure/calculator/getch.c
    4_Functions_and_Program_Structure/calculator/getop.c
    4_Functions_and_Program_Structure/calculator/stack.c
    4_Functions_and_Program_Structure/calculator/main.c)

target_link_libraries(c_programming_language m)
