@echo off

cls
call build.bat

for %%f in (build\*_spec.exe) do (
  %%f
  if errorlevel 1 (exit /b 1)
)

build\test_parsed.exe
if %errorlevel% neq 42 (exit /b 1)

build\hello_world.exe
if %errorlevel% neq 0 (exit /b 1)