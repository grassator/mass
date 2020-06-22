#include "windows.h"

void __stdcall entry(void) {
  ExitProcess(42);
}
