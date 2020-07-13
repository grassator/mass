@echo off

cls
call build
build\mass.exe
build\function_spec.exe

build\test.exe
echo ExitCode: %errorlevel%

build\hello_world.exe