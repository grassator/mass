@echo off

cls
call build
if %errorlevel% neq 0 (exit /b %errorlevel%)
build\source_spec.exe
if %errorlevel% neq 0 (exit /b %errorlevel%)

build\test_parsed.exe
if %errorlevel% neq 42 (exit /b %errorlevel%)

build\hello_world.exe
if %errorlevel% neq 0 (exit /b %errorlevel%)