#include "prelude.h"
#include <math.h>

s32
align(
  s32 number,
  s32 alignment
) {
  return (s32)(ceil((double)number / alignment) * alignment);
}

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

void
free_buffer(
  Buffer *buffer
) {
  VirtualFree(buffer->memory, 0, MEM_RELEASE);
}

#define static_array_size(_array_) (sizeof(_array_) / sizeof((_array_)[0]))

void
buffer_reset(
  Buffer *buffer
) {
  buffer->occupied = 0;
}

void *
buffer_allocate_size(
  Buffer *buffer,
  u64 byte_size
) {
  assert(buffer->occupied + byte_size <= buffer->capacity);
  void *target = buffer->memory + buffer->occupied;
  buffer->occupied += byte_size;
  return target;
}

#define buffer_allocate(_buffer_, _type_) \
  (_type_ *)buffer_allocate_size((_buffer_), sizeof(_type_))

#define define_buffer_append(_type_) \
inline _type_ * \
buffer_append_##_type_( \
  Buffer *buffer, \
  _type_ value \
) { \
  _type_ *target = buffer_allocate(buffer, _type_); \
  *target = value; \
  return target; \
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

Buffer temp_buffer = {0};

Dynamic_Array_Internal *
dynamic_array_realloc_internal(
  Dynamic_Array_Internal *internal,
  size_t item_size,
  size_t item_count
) {
  size_t new_allocation_size = sizeof(Dynamic_Array_Internal) + item_count * item_size;
  Dynamic_Array_Internal *result = realloc(internal, new_allocation_size);
  if (result) {
    result->next_free = result->items;
    result->after_last = result->items + item_count * item_size;
  } else {
    free(internal);
  }
  return result;
}

void
dynamic_array_increase_capacity(
  Dynamic_Array_Internal **internal,
  size_t item_size
) {
  size_t current_capacity = ((*internal)->after_last - (*internal)->items) / item_size;
  size_t new_capacity = current_capacity + current_capacity / 2;
  size_t next_free_offset = (*internal)->next_free - (*internal)->items;
  *internal = dynamic_array_realloc_internal(*internal, item_size, new_capacity);
  if (*internal) {
    (*internal)->next_free += next_free_offset;
  }
}
