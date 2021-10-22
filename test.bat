@echo off

set "CC=%1"
if "%CC%"=="" set "CC=cl"

cls
call build.bat %CC%
if %errorlevel% neq 0 (exit /b 1)

for %%f in (build\*_spec.exe) do (
  %%f
  if %errorlevel% neq 0 (exit /b 1)
)

build\test_parsed.exe
if %errorlevel% neq 42 (exit /b 1)

build\hello_world.exe
if %errorlevel% neq 0 (exit /b 1)