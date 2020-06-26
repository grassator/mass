#ifndef PRELUDE_H
#define PRELUDE_H

#include <stdint.h>
#include <stdbool.h>
#include <assert.h>

#define CONCAT_HELPER(a, b) a ## b
#define CONCAT(a, b) CONCAT_HELPER(a, b)

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

Buffer
make_buffer(
  u64 capacity,
  s32 permission_flags
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


//////////////////////////////////////////////////////////////////////////////
// Array
//////////////////////////////////////////////////////////////////////////////

typedef struct {
  s8 *next_free;
  s8 *after_last;
  s8 items[];
} Dynamic_Array_Internal;

#define array_type(_type_)\
  union {\
    Dynamic_Array_Internal *internal;\
    struct {\
      _type_ *next_free;\
      _type_ *after_last;\
      _type_ items[];\
    } *array;\
  }

typedef array_type(s8)  Array_s8;
typedef array_type(s16) Array_s16;
typedef array_type(s32) Array_s32;
typedef array_type(s64) Array_s64;

typedef array_type(u8)  Array_u8;
typedef array_type(u16) Array_u16;
typedef array_type(u32) Array_u32;
typedef array_type(u64) Array_u64;

Dynamic_Array_Internal *
dynamic_array_realloc_internal(
  Dynamic_Array_Internal *internal,
  size_t item_size,
  size_t item_count
);

void
dynamic_array_increase_capacity(
  Dynamic_Array_Internal **internal,
  size_t item_size
);

inline void
dynamic_array_ensure_capacity(
  Dynamic_Array_Internal **internal,
  size_t item_size,
  size_t extra_item_count
) {
  while (((*internal)->next_free + extra_item_count * item_size) > (*internal)->after_last) {
    dynamic_array_increase_capacity(internal, item_size);
  }
}

#define array_alloc(_array_type_, _count_)\
  ((_array_type_) {\
    .internal = dynamic_array_realloc_internal(\
      0, sizeof(((_array_type_ *) 0)->array->items[0]), (_count_)\
    ),\
  })

#define array_free(_array_)\
  free((_array_).array)

#define array_push(_array_, ...)\
  do {\
    dynamic_array_ensure_capacity(&((_array_).internal), sizeof((_array_).array->items[0]), 1);\
    (*(_array_).array->next_free++ = (##__VA_ARGS__));\
  } while(0)

#define array_capacity(_array_)\
  (((_array_).internal->after_last - (_array_).internal->items) / sizeof((_array_).array->items[0]))

#define array_count(_array_)\
  (((_array_).array->next_free - (_array_).array->items))

#define array_get(_array_, _index_)\
  (&(_array_).array->items[_index_])

#define array_begin(_array_)\
  ((_array_).array->items)

#define array_end(_array_)\
  ((_array_).array->next_free)

#define at(_index_) array->items[_index_]



#endif PRELUDE_H
