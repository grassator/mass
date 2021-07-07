# Mass (Meta Assembly) Language

A lot of the code for this repository is developed live on my
"[Compiler Programming in C](https://www.youtube.com/playlist?list=PLvdK1vRmp8wMzH4w_8sQ30NKU3Bt4Cc-M)"
series of videos on YouTube.

## Build

### Windows

The code has been tested with MSVC 14.0+ compiler with. To build the code
open up MSVC Command Prompt and run `build.bat` in the repository folder.
Tests can be executed by running `test.bat`.

### Mac / Linux

> While the compiler itself can be built and run on non-Windows systems,
there is currently **no support for generating Linux or Mac binaries**, nor
support JIT-compilations on these platforms. Cross-compiled to Windows
executables that the compiler outputs can be run using Wine.

You can build the code by running `./build.sh` and tests are run with
`./test.sh`. Both gcc and clang compilers are supported and you can
set which one to use by providing a `CC` environment variable:

```
CC=clang ./build.sh
```

### Tokenizer

Tokenizer is generated with [re2c](https://re2c.org/) so if you need
to make changes you need to have it available on Linux or in WSL.

## License

Copyright (c) 2020 Dmitriy Kubyshkin (unless noted otherwise in the code).

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

