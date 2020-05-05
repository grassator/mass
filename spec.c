#include "bdd-for-c.h"
#include "windows.h"
#include <stdint.h>
#include <stdbool.h>

typedef int64_t i64;
typedef uint8_t u8;

#define x64_ret 0xc3
#define x64_REX_W 0x48

typedef i64 (*the_answer)();

// 48 c7 c0 2a 00 00 00
spec("mass") {
  it("should create function that will return 42") {
    u8 *memory = VirtualAlloc(
      0, 1024, MEM_COMMIT | MEM_RESERVE, PAGE_EXECUTE_READWRITE
    );
    memory[0] = 0x48;
    memory[1] = 0xc7;
    memory[2] = 0xc0;
    memory[3] = 0x2a;
    memory[4] = 0x00;
    memory[5] = 0x00;
    memory[6] = 0x00;
    memory[7] = x64_ret;

    i64 result = ((the_answer)memory)();
    check(result == 42);
  }
}
