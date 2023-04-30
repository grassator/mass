cmake_minimum_required(VERSION 3.25)
project(mass C)

set(CMAKE_C_STANDARD 23)

include_directories(.)

add_executable(meta
        meta.c)

add_custom_command(
        OUTPUT generated_exports.c generated_types.h
        COMMAND meta
        DEPENDS meta.c
)

add_custom_target(generate_code DEPENDS generated_exports.c generated_types.h)
add_dependencies(generate_code meta)

add_executable(mass
        mass.c)
add_dependencies(mass generate_code)

add_executable(source_spec
        source_spec.c)
add_dependencies(source_spec generate_code)

