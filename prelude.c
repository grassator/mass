#include "types.h"

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
    .capacity = capacity,
    .occupied = 0,
  };
}

#define static_array_size(Array) (sizeof(Array) / sizeof(Array[0]))

#define define_buffer_append(Type) \
inline void \
buffer_append_##Type( \
  Buffer *buffer, \
  Type value \
) { \
  assert(buffer->occupied + sizeof(Type) <= buffer->capacity); \
  Type *target = (Type *)(buffer->memory + buffer->occupied); \
  *target = value; \
  buffer->occupied += sizeof(Type); \
}

define_buffer_append(s8)
define_buffer_append(s16)
define_buffer_append(s32)
define_buffer_append(s64)

define_buffer_append(u8)
define_buffer_append(u16)
define_buffer_append(u32)
define_buffer_append(u64)
#undef define_buffer_append
