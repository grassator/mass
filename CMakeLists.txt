cmake_minimum_required(VERSION 3.25)
project(mass C)

set(CMAKE_C_STANDARD 23)

include_directories(.)

if (MSVC)
    add_compile_options(
        /Wall /wd4456 /wd4711 /wd4068 /wd4710 /wd4204 /wd4200 /wd4221 /wd4057 /wd4996 /wd6334
        /wd4255 /wd4505 /wd4201 /wd4668 /wd4820 /wd5045 /wd4100 /wd4214 /wd5105 /wd4191 /wd4061
        /D UNICODE /D _UNICODE)
else()
    add_compile_options(-Wno-incompatible-pointer-types)
endif()

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

