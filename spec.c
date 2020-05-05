#include "bdd-for-c.h"
#include "windows.h"
#include <stdint.h>
#include <stdbool.h>

typedef int64_t s64;
typedef int32_t s32;
typedef uint8_t u8;

#define x64_ret 0xc3

typedef s32 (*constant_s32)();

// Little Endian
// s32 = 4 bytes
// 42
// 0x2a 0x00 0x00 0x00

constant_s32
make_constant_s32(
  s32 value
) {
  u8 *memory = VirtualAlloc(
    0, 1024, MEM_COMMIT | MEM_RESERVE, PAGE_EXECUTE_READWRITE
  );
  memory[0] = 0x48;
  memory[1] = 0xc7;
  memory[2] = 0xc0;
  s32 *immediate_value = (s32 *)(&memory[3]);
  *immediate_value = value;
  memory[7] = x64_ret;
  return (constant_s32)memory;
}

typedef s64 (*identity_s64)();

identity_s64
make_identity_s64() {
  u8 *memory = VirtualAlloc(
    0, 1024, MEM_COMMIT | MEM_RESERVE, PAGE_EXECUTE_READWRITE
  );
  memory[0] = 0x48;
  memory[1] = 0x89;
  memory[2] = 0xc8;
  memory[3] = x64_ret;
  return (identity_s64)memory;
}

spec("mass") {
  it("should create function that will return 42") {
    constant_s32 the_answer = make_constant_s32(42);
    s32 result = the_answer();
    check(result == 42);
    check(the_answer() == 42);
  }
  it("should create function that will return 21") {
    constant_s32 not_the_answer = make_constant_s32(21);
    s32 result = not_the_answer();
    check(result == 21);
  }
  it("should create function that returns s64 value that was passed") {
    identity_s64 id_s64 = make_identity_s64();
    s64 result = id_s64(42);
    check(result == 42);
  }
}
