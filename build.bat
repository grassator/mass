@echo off

set "CC=%1"
if "%CC%"=="" set "CC=cl"

set VS2022="C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat"
set VS2019="C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars64.bat"

if not defined DevEnvDir (
  if exist %VS2022% (call %VS2022%) else if exist %VS2019% (call %VS2019%)
)

rmdir /S /Q build
mkdir build

pushd build

set FLAGS=/Od /std:c11 /nologo /WX /F 16777216 /FC /Zo /Zi^
  /Wall /wd4456 /wd4711 /wd4068 /wd4710 /wd4204 /wd4200 /wd4221 /wd4057 /wd4996 /wd6334^
  /wd4255 /wd4505 /wd4201 /wd4668 /wd4820 /wd5045 /wd4100 /wd4214 /wd5105 /wd4191^
  /D UNICODE /D _UNICODE

if "%CC%"=="clang-cl" (
  set FLAGS=-gcodeview /Od /Zo /Zi -D UNICODE -D _UNICODE^
    -Wno-incompatible-function-pointer-types ^
    -Wno-initializer-overrides ^
    -Wno-deprecated-declarations
)

%CC% %FLAGS% ..\meta.c
if %errorlevel% neq 0 (goto Fail)

.\meta
if %errorlevel% neq 0 (goto Fail)

%CC% %FLAGS% ..\source_spec.c
if %errorlevel% neq 0 (goto Fail)

%CC% %FLAGS% ..\mass.c
if %errorlevel% neq 0 (goto Fail)

:Success
popd
exit /b 0

:Fail
popd
exit /b 1