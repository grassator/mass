cmake_minimum_required(VERSION 3.25)
project(mass C)

set(CMAKE_C_STANDARD 23)

include_directories(.)

add_executable(mass
        mass.c)

add_executable(source_spec
        source_spec.c)
