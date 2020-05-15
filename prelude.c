#include <stdint.h>
#include <stdbool.h>
#include <assert.h>

typedef int8_t  s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

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


typedef void (*fn_type_void_to_void)(void);

typedef fn_type_void_to_void fn_type_opaque;

typedef s32 (*fn_type_void_to_s32)(void);
typedef s64 (*fn_type_void_to_s64)(void);

typedef s32 (*fn_type_s32_to_s32)(s32);
typedef s64 (*fn_type_s64_to_s64)(s64);

typedef s64 (*fn_type_s64_s64_s64_to_s64)(s64, s64, s64);


typedef s32 (*fn_type__void_to_s32__to_s32)(fn_type_void_to_s32);
