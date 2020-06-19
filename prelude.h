#ifndef TYPES_H
#define TYPES_H

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

typedef void (*fn_type_void_to_void)(void);
typedef fn_type_void_to_void fn_type_opaque;

typedef s32 (*fn_type_void_to_s32)(void);
typedef s64 (*fn_type_void_to_s64)(void);
typedef s32 (*fn_type_voidp_to_s32)(void*);
typedef s64 (*fn_type_voidp_s64_to_s64)(void*, s64);
typedef s8  (*fn_type_s32_s8_to_s8)(s32, s8);
typedef void (*fn_type_s32p_to_void)(s32*);
typedef s8 (*fn_type_s32_to_s8)(s32);
typedef s32 (*fn_type_s32_to_s32)(s32);
typedef s32 (*fn_type_s32_s32_to_s32)(s32, s32);
typedef s64 (*fn_type_s64_to_s64)(s64);
typedef s64 (*fn_type_s64_s64_to_s64)(s64, s64);
typedef s64 (*fn_type_s64_s64_s64_to_s64)(s64, s64, s64);
typedef s64 (*fn_type_s64_s64_s64_s64_s64_to_s64)(s64, s64, s64, s64, s64);
typedef s64 (*fn_type_s64_s64_s64_s64_s64_s64_to_s64)(s64, s64, s64, s64, s64, s64);
typedef s32 (*fn_type__void_to_s32__to_s32)(fn_type_void_to_s32);

s32
align(
  s32 number,
  s32 alignment
);

typedef struct {
  u8 *memory;
  u64 occupied;
  u64 capacity;
} Buffer;

void
buffer_reset(
  Buffer *buffer
);

void *
buffer_allocate_size(
  Buffer *buffer,
  u64 byte_size
);

void
buffer_reset(
  Buffer *buffer
);

#define buffer_allocate(_buffer_, _type_) \
  (_type_ *)buffer_allocate_size((_buffer_), sizeof(_type_))

#define define_buffer_append(_type_) \
inline _type_ * \
buffer_append_##_type_( \
  Buffer *buffer, \
  _type_ value \
);

define_buffer_append(s8)
define_buffer_append(s16)
define_buffer_append(s32)
define_buffer_append(s64)

define_buffer_append(u8)
define_buffer_append(u16)
define_buffer_append(u32)
define_buffer_append(u64)
#undef define_buffer_append

#define temp_allocate_array(_type_, _count_) \
  buffer_allocate_size(&temp_buffer, sizeof(_type_) * _count_)

#define temp_allocate(_type_) \
  (_type_ *)temp_allocate_array(_type_, 1)


#endif TYPES_H
