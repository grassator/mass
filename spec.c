#include "bdd-for-c.h"
#include "windows.h"
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>

typedef int64_t s64;
typedef uint64_t u64;
typedef int32_t s32;
typedef uint8_t u8;
typedef int8_t s8;

#define x64_ret 0xc3

typedef s32 (*constant_s32)();

typedef struct {
  u8 *memory;
  u64 occupied;
  u64 capacity;
} Buffer;

Buffer
make_buffer(
  u64 capacity,
  s32 permission_flags
) {
  u8 *memory = VirtualAlloc(0, capacity, MEM_COMMIT | MEM_RESERVE, permission_flags);
  return (const Buffer) {
    .memory = memory,
    .occupied = 0,
    .capacity = capacity,
  };
}

void
buffer_append_u8(
  Buffer *buffer,
  u8 value
) {
  assert(buffer->occupied + sizeof(value) <= buffer->capacity);
  buffer->memory[buffer->occupied] = value;
  buffer->occupied += sizeof(value);
}

void
buffer_append_s32(
  Buffer *buffer,
  s32 value
) {
  assert(buffer->occupied + sizeof(value) <= buffer->capacity);
  u8 *first_non_occupied_address = buffer->memory + buffer->occupied;
  s32 *target = (s32 *)first_non_occupied_address;
  *target  = value;
  buffer->occupied += sizeof(value);
}

void
buffer_append_sub_rsp_imm_8(
  Buffer *buffer,
  s8 value
) {
  buffer_append_u8(buffer, 0x48);
  buffer_append_u8(buffer, 0x83);
  buffer_append_u8(buffer, 0xec);
  buffer_append_u8(buffer, value);
}

void
buffer_append_add_rsp_imm_8(
  Buffer *buffer,
  s8 value
) {
  buffer_append_u8(buffer, 0x48);
  buffer_append_u8(buffer, 0x83);
  buffer_append_u8(buffer, 0xc4);
  buffer_append_u8(buffer, value);
}

void
buffer_append_mov_to_stack_offset_imm_32(
  Buffer *buffer,
  s8 offset,
  s32 value
) {
  buffer_append_u8(buffer, 0xc7);
  buffer_append_u8(buffer, 0x44);
  buffer_append_u8(buffer, 0x24);
  buffer_append_u8(buffer, offset);
  buffer_append_s32(buffer, value);
}

void
buffer_append_add_to_ecx_value_at_stack_offset(
  Buffer *buffer,
  s8 value
) {
  buffer_append_u8(buffer, 0x03);
  buffer_append_u8(buffer, 0x4C);
  buffer_append_u8(buffer, 0x24);
  buffer_append_u8(buffer, value);
}

void
buffer_append_mov_to_rcx_from_rax(
  Buffer *buffer
) {
  buffer_append_u8(buffer, 0x48);
  buffer_append_u8(buffer, 0x89);
  buffer_append_u8(buffer, 0xc8);
}

constant_s32
make_constant_s32(
  s32 value
) {
  Buffer buffer = make_buffer(1024, PAGE_EXECUTE_READWRITE);
  buffer_append_u8(&buffer, 0x48);
  buffer_append_u8(&buffer, 0xc7);
  buffer_append_u8(&buffer, 0xc0);
  buffer_append_s32(&buffer, value);
  buffer_append_u8(&buffer, x64_ret);
  return (constant_s32)buffer.memory;
}

typedef s64 (*identity_s64)();

identity_s64
make_identity_s64() {
  Buffer buffer = make_buffer(1024, PAGE_EXECUTE_READWRITE);
  buffer_append_mov_to_rcx_from_rax(&buffer);
  buffer_append_u8(&buffer, x64_ret);
  return (identity_s64)buffer.memory;
}

typedef s64 (*increment_s64)();
increment_s64
make_increment_s64() {
  Buffer buffer = make_buffer(1024, PAGE_EXECUTE_READWRITE);
  buffer_append_sub_rsp_imm_8(&buffer, 24);
  buffer_append_mov_to_stack_offset_imm_32(&buffer, 0, 1);
  buffer_append_add_to_ecx_value_at_stack_offset(&buffer, 0);
  buffer_append_mov_to_rcx_from_rax(&buffer);
  buffer_append_add_rsp_imm_8(&buffer, 24);
  buffer_append_u8(&buffer, x64_ret);
  return (increment_s64)buffer.memory;
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

  it("should create function increments s64 value passed to it") {
    identity_s64 inc_s64 = make_increment_s64();
    s64 result = inc_s64(42);
    check(result == 43);
  }
}










