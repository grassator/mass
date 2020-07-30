@echo off

cls
call build
build\spec.exe
if %errorlevel% neq 0 (exit /b %errorlevel%)
build\function_spec.exe
if %errorlevel% neq 0 (exit /b %errorlevel%)
build\source_spec.exe
if %errorlevel% neq 0 (exit /b %errorlevel%)

build\test.exe
echo ExitCode (Macro): %errorlevel%
build\test_parsed.exe
echo ExitCode (Parsed): %errorlevel%

build\hello_world.exe