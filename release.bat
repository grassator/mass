@echo off

set "CC=%1"
if "%CC%"=="" set "CC=cl"

if not defined DevEnvDir (
    call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars64.bat"
)

rmdir /S /Q build
mkdir build

pushd build

set FLAGS=/std:c17 /nologo /WX /FC /Zo /Zi /GL ^
  /Wall /wd4456 /wd4711 /wd4068 /wd4710 /wd4204 /wd4200 /wd4221 /wd4057 /wd4996 /wd6334^
  /wd4255 /wd4505 /wd4201 /wd4668 /wd4820 /wd5045 /wd4100 /wd4214 /wd5105 /wd4191^
  /D UNICODE /D _UNICODE

if "%CC%"=="clang-cl" (
  set FLAGS=-D UNICODE -D _UNICODE^
    -Wno-incompatible-function-pointer-types ^
    -Wno-initializer-overrides ^
    -Wno-deprecated-declarations
)

%CC% /Od %FLAGS% ..\meta.c
if %errorlevel% neq 0 (goto Fail)

.\meta
if %errorlevel% neq 0 (goto Fail)

%CC% /O2 /Oy /Ob2 %FLAGS% /wd4189 /D NDEBUG ..\mass.c /link /STACK:16777216
if %errorlevel% neq 0 (goto Fail)

:Success
popd
exit /b 0

:Fail
popd
exit /b 1