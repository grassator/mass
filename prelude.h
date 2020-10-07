#ifndef PRELUDE_H
#define PRELUDE_H

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#ifndef countof
#define countof(...)\
  (sizeof(__VA_ARGS__) / sizeof((__VA_ARGS__)[0]))
#endif

#define static_assert(_condition_, _message_)\
  struct _message_ { int _message_ : !!(_condition_); }

#define static_assert_type_alignment(_type_, _alignment_)\
  static_assert(\
    sizeof(_type_) % (_alignment_) == 0,\
    _type_##__expected_to_be_aligned_to__##_alignment_\
  );

#define panic(_message_) assert(!(_message_))

typedef int8_t  s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

#define u64_max_value UINT64_MAX
#define u64_min_value 0

#define u32_max_value UINT32_MAX
#define u32_min_value 0

#define u16_max_value UINT16_MAX
#define u16_min_value 0

#define u8_max_value UINT8_MAX
#define u8_min_value 0

#define s64_max_value INT64_MAX
#define s64_min_value INT64_MIN

#define s32_max_value INT32_MAX
#define s32_min_value INT32_MIN

#define s16_max_value INT16_MAX
#define s16_min_value INT16_MIN

#define s8_max_value INT8_MAX
#define s8_min_value INT8_MIN

typedef float f32;
typedef double f64;

static_assert(sizeof(void *) == sizeof(u64), prelude_only_supports_64_bit_architectures);

#define PRELUDE_ENUMERATE_SIGNED_INTEGER_TYPES\
  PRELUDE_PROCESS_TYPE(s8)\
  PRELUDE_PROCESS_TYPE(s16)\
  PRELUDE_PROCESS_TYPE(s32)\
  PRELUDE_PROCESS_TYPE(s64)

#define PRELUDE_ENUMERATE_UNSIGNED_INTEGER_TYPES\
  PRELUDE_PROCESS_TYPE(u8)\
  PRELUDE_PROCESS_TYPE(u16)\
  PRELUDE_PROCESS_TYPE(u32)\
  PRELUDE_PROCESS_TYPE(u64)

#define PRELUDE_ENUMERATE_FLOAT_TYPES\
  PRELUDE_PROCESS_TYPE(f32)\
  PRELUDE_PROCESS_TYPE(f64)

#define PRELUDE_ENUMERATE_INTEGER_TYPES\
  PRELUDE_ENUMERATE_SIGNED_INTEGER_TYPES\
  PRELUDE_ENUMERATE_UNSIGNED_INTEGER_TYPES

#define PRELUDE_ENUMERATE_NUMERIC_TYPES\
  PRELUDE_ENUMERATE_FLOAT_TYPES\
  PRELUDE_ENUMERATE_SIGNED_INTEGER_TYPES\
  PRELUDE_ENUMERATE_UNSIGNED_INTEGER_TYPES

//////////////////////////////////////////////////////////////////////////////
// Integer Casts
//////////////////////////////////////////////////////////////////////////////

#define DEFINE_UNSIGNED_CAST(_source_type_, _target_type_)\
  inline bool _source_type_##_fits_into_##_target_type_(_source_type_ value) {\
    return value <= _target_type_##_max_value;\
  }\
  inline _target_type_ _source_type_##_to_##_target_type_(_source_type_ value) {\
    assert(value <= _target_type_##_max_value);\
    return (_target_type_)value;\
  }

#define PRELUDE_PROCESS_TYPE(_type_)\
  DEFINE_UNSIGNED_CAST(u8, _type_)\
  DEFINE_UNSIGNED_CAST(u16, _type_)\
  DEFINE_UNSIGNED_CAST(u32, _type_)\
  DEFINE_UNSIGNED_CAST(u64, _type_)
PRELUDE_ENUMERATE_INTEGER_TYPES
#undef PRELUDE_PROCESS_TYPE
#undef DEFINE_UNSIGNED_CAST

#define DEFINE_SIGNED_CAST(_source_type_, _target_type_)\
  inline bool _source_type_##_fits_into_##_target_type_(_source_type_ value) {\
    return value >= _target_type_##_min_value && value <= _target_type_##_max_value;\
  }\
  inline _target_type_ _source_type_##_to_##_target_type_(_source_type_ value) {\
    assert(value >= _target_type_##_min_value);\
    assert(value <= _target_type_##_max_value);\
    return (_target_type_)value;\
  }

#define PRELUDE_PROCESS_TYPE(_type_)\
  DEFINE_SIGNED_CAST(s8, _type_)\
  DEFINE_SIGNED_CAST(s16, _type_)\
  DEFINE_SIGNED_CAST(s32, _type_)\
  DEFINE_SIGNED_CAST(s64, _type_)
PRELUDE_ENUMERATE_INTEGER_TYPES
#undef PRELUDE_PROCESS_TYPE
#undef DEFINE_UNSIGNED_CAST

//////////////////////////////////////////////////////////////////////////////
// Math
//////////////////////////////////////////////////////////////////////////////

#define PRELUDE_PROCESS_TYPE(_type_)\
  inline _type_ _type_##_min(_type_ x, _type_ y) { return x < y ? x : y; }\
  inline _type_ _type_##_max(_type_ x, _type_ y) { return x > y ? x : y; }
PRELUDE_ENUMERATE_NUMERIC_TYPES
#undef PRELUDE_PROCESS_TYPE

#define PRELUDE_PROCESS_TYPE(_type_)\
  inline _type_ _type_##_abs(_type_ x) { return x < 0 ? (-x) : x; }
PRELUDE_ENUMERATE_SIGNED_INTEGER_TYPES
#undef PRELUDE_PROCESS_TYPE

#define PRELUDE_PROCESS_TYPE(_type_)\
  inline _type_ _type_##_align(_type_ x, _type_ align) { return ((x + align - 1) / align) * align; }
PRELUDE_ENUMERATE_INTEGER_TYPES
#undef PRELUDE_PROCESS_TYPE

////////////////////////////////////////////////////////////////////////////////
// Color
////////////////////////////////////////////////////////////////////////////////

typedef union {
  struct {
    u8 r;
    u8 g;
    u8 b;
    u8 a;
  };
  u32 raw;
} Color_Rgba_8;

#define color_rgba(_r_, _g_, _b_, _a_)\
  (Color_Rgba_8){.r = (_r_), .g = (_g_), .b = (_b_), .a = (_a_)}

#define color_rgb(_r_, _g_, _b_)\
  color_rgba(_r_, _g_, _b_, 0xff)

//////////////////////////////////////////////////////////////////////////////
// Memory
//////////////////////////////////////////////////////////////////////////////
#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

s32
memory_page_size() {
  static s32 size = -1;
  if (size == -1) {
    #ifdef _WIN32
    SYSTEM_INFO system_info;
    GetSystemInfo(&system_info);
    size = system_info.dwPageSize;
    #else
    size = sysconf(_SC_PAGESIZE);
    #endif
  }
  return size;
}

//////////////////////////////////////////////////////////////////////////////
// Allocator
//////////////////////////////////////////////////////////////////////////////
typedef struct {
  void *raw;
} Allocator_Handle;

typedef struct {
  void *(*allocate)(Allocator_Handle handle, u64 size_in_bytes, u64 alignment);
  void  (*deallocate)(Allocator_Handle handle, void *address);
  void *(*reallocate)(
    Allocator_Handle handle,
    void *address,
    u64 old_size_in_bytes,
    u64 new_size_in_bytes,
    u64 alignment
  );
  Allocator_Handle handle;
} Allocator;

inline void *
allocator_default_allocate(
  Allocator_Handle handle,
  u64 size_in_bytes,
  u64 alignment
) {
  // TODO use aligned malloc if available
  return malloc(size_in_bytes);
}
inline void
allocator_default_deallocate(
  Allocator_Handle handle,
  void *address
) {
  free(address);
}

inline void *
allocator_default_reallocate(
  Allocator_Handle handle,
  void *address,
  u64 old_size_in_bytes,
  u64 new_size_in_bytes,
  u64 alignment
) {
  // TODO use aligned realloc if available
  return realloc(address, new_size_in_bytes);
}

const Allocator *allocator_default = &(Allocator){
  allocator_default_allocate,
  allocator_default_deallocate,
  allocator_default_reallocate
};

#ifdef _WIN32
inline void *
allocator_system_allocate(
  Allocator_Handle handle,
  u64 size_in_bytes,
  u64 alignment
) {
  // More than page of alignment is probably not required anyway so can ignore
  return VirtualAlloc(0, size_in_bytes, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
}
inline void
allocator_system_deallocate(
  Allocator_Handle handle,
  void *address
) {
  VirtualFree(address, 0, MEM_RELEASE);
}

inline void *
allocator_system_reallocate(
  Allocator_Handle handle,
  void *address,
  u64 old_size_in_bytes,
  u64 new_size_in_bytes,
  u64 alignment
) {
  // More than page of alignment is probably not required anyway so can ignore
  void *new_address = allocator_system_allocate(handle, new_size_in_bytes, alignment);
  memcpy(new_address, address, old_size_in_bytes);
  allocator_system_deallocate(handle, address);
  return new_address;
}

const Allocator *allocator_system = &(Allocator){
  allocator_system_allocate,
  allocator_system_deallocate,
  allocator_system_reallocate,
};
#endif

inline void *
allocator_allocate_bytes_internal(
  const Allocator *allocator,
  u64 size_in_bytes,
  u64 alignment
) {
  return allocator->allocate(allocator->handle, size_in_bytes, alignment);
}

#ifdef PRELUDE_TRACK_ALLOCATION
  inline void *
  allocator_debug_allocate_bytes(
    const Allocator *allocator,
    u64 size_in_bytes,
    u64 alignment,
    const char *function,
    const char *file,
    u64 line
  );

  #define allocator_allocate_bytes(...)\
    allocator_debug_allocate_bytes(__VA_ARGS__, __func__, __FILE__, __LINE__)
#else
  #define allocator_allocate_bytes(...)\
    allocator_allocate_bytes_internal(__VA_ARGS__)
#endif

#define allocator_allocate(_allocator_, _type_)\
  (_type_ *)allocator_allocate_bytes((_allocator_), sizeof(_type_), _Alignof(_type_))

#define allocator_allocate_array(_allocator_, _type_, _count_)\
  (_type_ *)allocator_allocate_bytes((_allocator_), sizeof(_type_) * (_count_), _Alignof(_type_))

inline void
allocator_deallocate(
  const Allocator *allocator,
  void *address
) {
  allocator->deallocate(allocator->handle, address);
}

inline void *
allocator_reallocate(
  const Allocator *allocator,
  void *address,
  u64 old_size_in_bytes,
  u64 new_size_in_bytes,
  u64 alignment
) {
  return allocator->reallocate(
    allocator->handle,
    address,
    old_size_in_bytes,
    new_size_in_bytes,
    alignment
  );
}

#define allocator_make(_allocator_, _type_, ...)\
  (_type_ *)memcpy(\
    allocator_allocate((_allocator_), _type_),\
    &(_type_){__VA_ARGS__},\
    sizeof(_type_)\
  )

//////////////////////////////////////////////////////////////////////////////
// Range
//////////////////////////////////////////////////////////////////////////////

#define range_length(_r_)\
  ((_r_).to - (_r_).from)

#define range_equal(_a_, _b_)\
  ((_a_).from == (_b_).from && (_a_).to == (_b_).to)

#define range_intersects(_a_, _b_)\
  ((_a_).from < (_b_).to && (_b_).from < (_a_).to)

#define range_contains(_range_, _value_)\
  ((_value_) >= (_range_).from && (_value_) < (_range_).to)

/// `from` is inclusive, `to` is exclusive
#define PRELUDE_PROCESS_TYPE(_type_)\
  typedef struct { _type_ from; _type_ to; } Range_##_type_;\
  \
  Range_##_type_ range_##_type_##_intersection(Range_##_type_ *a, Range_##_type_ *b) {\
    Range_##_type_ result = {0};\
    if (range_intersects(*a, *b)) { \
      result.from = _type_##_max(a->from, b->from);\
      result.to = _type_##_min(a->to, b->to);\
    } \
    return result;\
  }
PRELUDE_ENUMERATE_INTEGER_TYPES
#undef PRELUDE_PROCESS_TYPE

//////////////////////////////////////////////////////////////////////////////
// Bucket_Array
//////////////////////////////////////////////////////////////////////////////

#define BUCKET_ARRAY_INITIAL_TAIL_CAPACITY 10

#define bucket_array_bucket_struct(_type_, _count_) \
  struct { \
    u64 length; \
    _type_ (* items)[_count_]; \
  }

// Must match bucket_array_bucket_struct definition above
typedef struct {
  u64 length;
  s8 *memory;
} Bucket_Array_Bucket_Internal;

#define bucket_array_struct_internal(_bucket_type_) \
  struct { \
    const Allocator *allocator; \
    u64 bucket_tail_length; \
    u64 bucket_tail_capacity; \
    _bucket_type_ bucket_tail[]; \
  }

#define bucket_array_struct(_type_, _count_) \
  bucket_array_struct_internal(\
    bucket_array_bucket_struct(_type_, _count_)\
  )

typedef bucket_array_struct_internal(Bucket_Array_Bucket_Internal) Bucket_Array_Internal;

#define bucket_array_type(_type_, _count_)\
  union {\
    Bucket_Array_Internal *internal;\
    bucket_array_struct(_type_, _count_) *typed;\
  }

#define bucket_array_type_page_size(_type_)\
  bucket_array_type(_type_, (4096 / sizeof(_type_)))

#define PRELUDE_PROCESS_TYPE(_type_)\
  typedef bucket_array_type_page_size(_type_)  Bucket_Array_##_type_;
PRELUDE_ENUMERATE_NUMERIC_TYPES
#undef PRELUDE_PROCESS_TYPE

struct Bucket_Array_Make_Options {
  void *_count_reset;
  const Allocator *allocator;
};

inline Bucket_Array_Internal *
bucket_array_alloc_internal(
  struct Bucket_Array_Make_Options *options,
  u64 single_bucket_byte_size,
  u64 item_byte_size,
  u64 initial_bucket_tail_capacity
) {
  const Allocator *allocator = options->allocator;
  u64 allocation_size =
    sizeof(Bucket_Array_Internal) +
    sizeof(Bucket_Array_Bucket_Internal) * initial_bucket_tail_capacity;
  Bucket_Array_Internal *result = allocator_allocate_bytes(
    allocator,
    allocation_size,
    u64_max(_Alignof(Bucket_Array_Bucket_Internal), _Alignof(Bucket_Array_Internal))
  );
  *result = (Bucket_Array_Internal) {
    .allocator = allocator,
    .bucket_tail_length = 1,
    .bucket_tail_capacity = initial_bucket_tail_capacity,
  };

  result->bucket_tail[0] = (Bucket_Array_Bucket_Internal) {
    .length = 0,
    .memory = allocator_allocate_bytes(allocator, single_bucket_byte_size, item_byte_size),
  };

  return result;
}

inline void
bucket_array_ensure_capacity_for_push(
  Bucket_Array_Internal **internal_pointer,
  u64 single_bucket_byte_size,
  u64 item_byte_size,
  u64 bucket_item_capacity
) {
  Bucket_Array_Internal *internal = *internal_pointer;
  u64 last_bucket_index = (internal->bucket_tail_length - 1);
  if (internal->bucket_tail[last_bucket_index].length < bucket_item_capacity) return;
  if (internal->bucket_tail_length == internal->bucket_tail_capacity) {
    u64 old_allocation_size = sizeof(Bucket_Array_Internal) +
      sizeof(Bucket_Array_Bucket_Internal) * internal->bucket_tail_capacity;
    internal->bucket_tail_capacity += internal->bucket_tail_capacity / 2;
    u64 new_allocation_size = sizeof(Bucket_Array_Internal) +
      sizeof(Bucket_Array_Bucket_Internal) * internal->bucket_tail_capacity;
    internal = allocator_reallocate(
      internal->allocator,
      internal,
      old_allocation_size,
      new_allocation_size,
      sizeof(Bucket_Array_Bucket_Internal)
    );
    *internal_pointer = internal;
  }
  internal->bucket_tail[internal->bucket_tail_length] = (Bucket_Array_Bucket_Internal) {
    .length = 0,
    .memory = allocator_allocate_bytes(
      internal->allocator, single_bucket_byte_size, item_byte_size
    ),
  };
  ++internal->bucket_tail_length;
}

inline u64
bucket_array_length_internal(
  Bucket_Array_Internal *internal,
  u64 bucket_item_capacity
) {
  u64 last_bucket_index = (internal->bucket_tail_length - 1);
  u64 full_bucket_count = last_bucket_index;
  u64 full_buckets_item_count = full_bucket_count * bucket_item_capacity;
  u64 last_bucket_item_count = internal->bucket_tail[last_bucket_index].length;
  return full_buckets_item_count + last_bucket_item_count;
}

void
bucket_array_destroy_internal(
  Bucket_Array_Internal *internal
) {
  for (u64 i = 0; i < internal->bucket_tail_length; ++i) {
    allocator_deallocate(internal->allocator, internal->bucket_tail[i].memory);
  }
  allocator_deallocate(internal->allocator, internal);
}

#define bucket_array_bucket_capacity(_array_)\
  countof(*(_array_).typed->bucket_tail[0].items)

#define bucket_array_length(_array_)\
  bucket_array_length_internal(\
    (_array_).internal,\
    bucket_array_bucket_capacity(_array_)\
  )

#define bucket_array_make(_array_type_, ...)\
  ((_array_type_) {\
    .internal = bucket_array_alloc_internal(\
      &((struct Bucket_Array_Make_Options){\
        .allocator = allocator_default,\
        ._count_reset = 0,\
        __VA_ARGS__\
      }),\
      sizeof(*((_array_type_ *) 0)->typed->bucket_tail[0].items),\
      sizeof((*((_array_type_ *) 0)->typed->bucket_tail[0].items)[0]),\
      BUCKET_ARRAY_INITIAL_TAIL_CAPACITY\
    )\
  })

#define bucket_array_get(_array_, _index_)\
  (&(*(_array_).typed->bucket_tail[\
    _index_ / bucket_array_bucket_capacity(_array_)\
  ].items)[_index_ % bucket_array_bucket_capacity(_array_)])

#define bucket_array_last_bucket(_array_)\
  ((_array_).typed->bucket_tail[(_array_).typed->bucket_tail_length - 1])

#define bucket_array_push(_array_, ...)\
  (\
    bucket_array_ensure_capacity_for_push(\
      &(_array_).internal,\
      sizeof(*(_array_).typed->bucket_tail[0].items),\
      sizeof((*(_array_).typed->bucket_tail[0].items)[0]),\
      bucket_array_bucket_capacity(_array_)\
    ),\
    ((*bucket_array_last_bucket(_array_).items)[\
      bucket_array_last_bucket(_array_).length\
    ] = (__VA_ARGS__)),\
    &((*bucket_array_last_bucket(_array_).items)[\
      bucket_array_last_bucket(_array_).length++\
    ])\
  )

#define bucket_array_destroy(_array_)\
  bucket_array_destroy_internal((_array_).internal)

//////////////////////////////////////////////////////////////////////////////
// Array
//////////////////////////////////////////////////////////////////////////////

#define dyn_array_struct(_type_) \
  struct { \
    const Allocator *allocator;\
    u64 length; \
    u64 capacity; \
    _type_ items[]; \
  }

typedef dyn_array_struct(s8) Dyn_Array_Internal;

#define dyn_array_type(_type_)\
  union {\
    Dyn_Array_Internal *internal;\
    dyn_array_struct(_type_) *data;\
  }

#define PRELUDE_PROCESS_TYPE(_type_)\
  typedef dyn_array_type(_type_)  Array_##_type_;
PRELUDE_ENUMERATE_NUMERIC_TYPES
#undef PRELUDE_PROCESS_TYPE

inline Dyn_Array_Internal *
dyn_array_realloc_internal(
  Dyn_Array_Internal *internal,
  u64 length,
  u64 item_byte_size,
  u64 capacity
) {
  // TODO use the size of the struct with real type for alignment as it might need to be
  // overaligned (more than pointer size)
  u64 old_allocation_size = sizeof(Dyn_Array_Internal) + item_byte_size * internal->capacity;
  u64 new_allocation_size = sizeof(Dyn_Array_Internal) + item_byte_size * capacity;
  Dyn_Array_Internal *result = allocator_reallocate(
    internal->allocator,
    internal,
    old_allocation_size,
    new_allocation_size,
    item_byte_size
  );
  if (result) {
    result->capacity = capacity;
  } else {
    allocator_deallocate(internal->allocator, internal);
  }
  return result;
}

struct Dyn_Array_Make_Options {
  void *_count_reset;
  u64 capacity;
  const Allocator *allocator;
};

inline Dyn_Array_Internal *
dyn_array_alloc_internal(
  struct Dyn_Array_Make_Options *options,
  u64 item_byte_size
) {
  u64 new_allocation_size = sizeof(Dyn_Array_Internal) + item_byte_size * options->capacity;
  Dyn_Array_Internal *result = allocator_allocate_bytes(
    options->allocator, new_allocation_size, item_byte_size
  );
  *result = (Dyn_Array_Internal) {
    .allocator = options->allocator,
    .capacity = options->capacity,
  };
  return result;
}

#define dyn_array_is_initialized(_array_)\
  (!!(_array_).data)

#define dyn_array_capacity(_array_)\
  ((_array_).data->capacity)

#define dyn_array_length(_array_)\
  ((_array_).data->length)

#define dyn_array_raw(_array_)\
  ((_array_).data->items)

inline u64
dyn_array_bounds_check(
  u64 index,
  u64 length
) {
  assert(index < length);
  return index;
}

#define dyn_array_get(_array_, _index_)\
  (&(_array_).data->items[dyn_array_bounds_check(_index_, (_array_).data->length)])

#define dyn_array_peek(_array_, _index_)\
  (((_index_) < (_array_).data->length) ? (&(_array_).data->items[_index_]) : 0)

#define dyn_array_get_unsafe(_array_, _index_)\
  (&(_array_).data->items[_index_])

#define dyn_array_clear(_array_)\
  ((_array_).data->length = 0)

inline void
dyn_array_ensure_capacity(
  Dyn_Array_Internal **internal,
  u64 item_byte_size,
  u64 extra
) {
  if ((*internal)->length + extra <= (*internal)->capacity) return;
  u64 new_capacity = (*internal)->capacity + u64_max((*internal)->capacity >> 1, extra);
  *internal = dyn_array_realloc_internal(
    *internal, (*internal)->length, item_byte_size, new_capacity
  );
}

#define dyn_array_item_size(_array_type_)\
  sizeof(((_array_type_ *) 0)->data->items[0])

#define dyn_array_make(_array_type_, ...)\
  ((_array_type_) {\
    .internal = dyn_array_alloc_internal(\
      &(struct Dyn_Array_Make_Options) {\
        .allocator = allocator_default,\
        .capacity = 16,\
        ._count_reset = 0,\
        __VA_ARGS__\
      },\
      dyn_array_item_size(_array_type_)\
    )\
  })

inline Dyn_Array_Internal *
dyn_array_sub_internal(
  Dyn_Array_Internal *source,
  u64 item_byte_size,
  Range_u64 range
) {
  assert(source->length == 0 || range.from < source->length);
  assert(range.to <= source->length);
  assert(range.from <= range.to);
  Dyn_Array_Internal *result = dyn_array_alloc_internal(
    &(struct Dyn_Array_Make_Options) {
      .allocator = source->allocator,
      .capacity = u64_max(range_length(range), 2),
    },
    item_byte_size
  );
  result->length = range_length(range);
  s8 *source_memory = (s8 *)source->items + item_byte_size * range.from;
  memcpy(result->items, source_memory, item_byte_size * result->length);
  return result;
}

inline Dyn_Array_Internal *
dyn_array_copy_internal(
  Dyn_Array_Internal *source,
  u64 item_byte_size
) {
  return dyn_array_sub_internal(source, item_byte_size, (Range_u64){0, source->length});
}

#define dyn_array_copy(_array_type_, _array_)\
  ((_array_type_) {\
    .internal = dyn_array_copy_internal(\
      (_array_).internal,\
      dyn_array_item_size(_array_type_)\
    )\
  })

#define dyn_array_sub(_array_type_, _array_, ...)\
  ((_array_type_) {\
    .internal = dyn_array_sub_internal(\
      (_array_).internal,\
      dyn_array_item_size(_array_type_),\
      (__VA_ARGS__)\
    )\
  })

#define dyn_array_destroy(_array_)\
  allocator_deallocate((_array_).data->allocator, (_array_).data)

inline void *
dyn_array_insert_internal(
  Dyn_Array_Internal **internal,
  u64 index,
  u64 length,
  u64 item_byte_size
) {
  dyn_array_ensure_capacity(internal, item_byte_size, length);
  u64 original_length = (*internal)->length;
  assert(index <= (*internal)->length);
  (*internal)->length += length;
  s8 *source = (*internal)->items + item_byte_size * index;
  // No need to memmove for a inserting at the end (same as push)
  if (index < original_length) {
    u64 copy_size = (original_length - index) * item_byte_size;
    memmove(source + item_byte_size * length, source, copy_size);
  }
  return source;
}

#define dyn_array_insert(_array_, _index_, ...)\
  (\
    dyn_array_insert_internal(&((_array_).internal), (_index_), 1, sizeof((_array_).data->items[0])),\
    ((_array_).data->items[_index_] = (__VA_ARGS__)),\
    &(_array_).data->items[_index_]\
  )

#define dyn_array_splice(_array_, _index_, _length_, _to_splice_)\
  do { \
    u64 _splice_index = _index_; \
    u64 _insert_length = dyn_array_length(_to_splice_); \
    s64 _diff = (s64)_insert_length - (_length_); \
    if (_diff < 0) { \
      dyn_array_delete_range(\
        _array_, (Range_u64){_splice_index, _splice_index - _diff}\
      ); \
    } else if (_diff > 0) { \
      dyn_array_insert_internal(\
        &((_array_).internal), _splice_index, _diff, sizeof((_array_).data->items[0])\
      ); \
    } \
    /* do not use memmove to ensure type safety */ \
    for (u64 j = 0; j < _insert_length; ++j) { \
      (_array_).data->items[_splice_index + j] = (_to_splice_).data->items[j]; \
    } \
  } while(0);

#define dyn_array_push(_array_, ...)\
  (\
    dyn_array_ensure_capacity(&((_array_).internal), sizeof((_array_).data->items[0]), 1),\
    ((_array_).data->items[(_array_).data->length] = (__VA_ARGS__)),\
    &(_array_).data->items[(_array_).data->length++]\
  )

#define dyn_array_pop(_array_, ...)\
  (dyn_array_length(_array_) > 0 ? &(_array_).data->items[((_array_).data->length--) - 1] : 0)

inline void
dyn_array_delete_range_internal(
  Dyn_Array_Internal *internal,
  u64 item_byte_size,
  Range_u64 range
) {
  assert(range.from <= range.to);
  assert(range.to <= internal->length);
  u64 range_length = range_length(range);
  if (range_length == 0) return;
  s8 *from_pointer = internal->items + item_byte_size * range.from;
  s8 *to_pointer = internal->items + item_byte_size * range.to;

  u64 move_size = (item_byte_size * internal->length) - item_byte_size * range.to;
  memmove(from_pointer, to_pointer, move_size);
  internal->length -= range_length;
}

inline void
dyn_array_delete_internal(
  Dyn_Array_Internal *internal,
  u64 item_byte_size,
  u64 index
) {
  dyn_array_delete_range_internal(internal, item_byte_size, (Range_u64){index, index + 1});
}

#define dyn_array_delete(_array_, _index_)\
  dyn_array_delete_internal(\
    (_array_).internal,\
    sizeof((_array_).data->items[0]),\
    (_index_)\
  )

#define dyn_array_delete_range(_array_, ...)\
  dyn_array_delete_range_internal(\
    (_array_).internal,\
    sizeof((_array_).data->items[0]),\
    (__VA_ARGS__)\
  )

#define dyn_array_last(_array_)\
  (dyn_array_length(_array_) > 0 ? &(_array_).data->items[(_array_).data->length - 1] : 0)

#define dyn_array_split(_result_, _array_, ...)\
  do {\
    \
  } while(0)

//////////////////////////////////////////////////////////////////////////////
// Slice
//////////////////////////////////////////////////////////////////////////////

typedef struct {
  const s8 *bytes;
  u64 length;
} Slice;

typedef dyn_array_type(Slice) Array_Slice;

inline void
slice_print(
  Slice slice
) {
  printf("%.*s", u64_to_s32(slice.length), slice.bytes);
}

/// Input slice is expected to just contain the number without any surrounding whitespace.
s64
slice_parse_s64(
  Slice slice,
  bool *ok
) {
  if (!ok) ok = &(bool){0};
  *ok = true;
  s64 integer = 0;
  s64 multiplier = 1;
  for (s64 index = slice.length - 1; index >= 0; --index) {
    s8 digit = slice.bytes[index];
    if (digit >= '0' && digit <= '9') {
      integer += (digit - '0') * multiplier;
      multiplier *= 10;
    } else if (digit == '-') {
      if (index != 0) {
        *ok = false;
        return 0;
      }
      integer = -integer;
    } else {
      *ok = false;
      return 0;
    }
  }
  return integer;
}

inline Slice
slice_from_c_string(
  const s8 *c_string
) {
  if (!c_string) return (Slice){0};
  return (Slice){
    .bytes = c_string,
    .length = strlen(c_string),
  };
}

#define slice_literal_fields(...)\
  { .bytes = (__VA_ARGS__), .length = sizeof(__VA_ARGS__) - 1, }

#define slice_literal(...)\
  ((Slice)slice_literal_fields(__VA_ARGS__))

inline Slice
slice_trim_starting_whitespace(
  Slice slice
) {
  if (!slice.length) return slice;
  u64 start_index = 0;
  for (;start_index < slice.length; start_index++) {
    if (!isspace(slice.bytes[start_index])) break;
  }
  return (Slice){
    .bytes = slice.bytes + start_index,
    .length = slice.length - start_index,
  };
}

inline Slice
slice_trim_ending_whitespace(
  Slice slice
) {
  if (!slice.length) return slice;
  u64 end_index = slice.length;
  for (;end_index != 0; end_index--) {
    if (!isspace(slice.bytes[end_index - 1])) break;
  }
  return (Slice){
    .bytes = slice.bytes,
    .length = end_index,
  };
}

Slice
slice_trim_whitespace(
  Slice slice
) {
  return slice_trim_ending_whitespace(slice_trim_starting_whitespace(slice));
}

inline bool
slice_starts_with(
  Slice haystack,
  Slice needle
) {
  if (haystack.bytes == needle.bytes) return true;
  if (needle.length > haystack.length) return false;
  return memcmp(
    haystack.bytes,
    needle.bytes,
    needle.length
  ) == 0;
}

inline bool
slice_ends_with(
  Slice haystack,
  Slice needle
) {
  if (haystack.bytes == needle.bytes) return true;
  if (needle.length > haystack.length) return false;
  return memcmp(
    haystack.bytes + (haystack.length - needle.length),
    needle.bytes,
    needle.length
  ) == 0;
}

char *
slice_to_c_string(
  const Allocator *allocator,
  Slice slice
) {
  char *result = allocator_allocate_bytes(allocator, slice.length + 1, _Alignof(char));
  memcpy(result, slice.bytes, slice.length);
  result[slice.length] = 0;
  return result;
}

#define INDEX_OF_NOT_FOUND 0xFFFFFFFFFFFFFFFFLLU

u64
slice_index_of_in_range(
  Slice haystack,
  Slice needle,
  Range_u64 range
) {
  u64 trail_hash_mask = 0xFFFFFFFFFFFFFFFFLLU;
  u64 needle_trail_hash = 0;
  u64 haystack_trail_hash = 0;
  u64 from = u64_min(range.from, haystack.length);
  u64 to = u64_min(range.to, haystack.length);
  for (
    u64 haystack_index = from, needle_index = 0;
    haystack_index < to;
  ) {
    haystack_trail_hash = (haystack_trail_hash << 8) | haystack.bytes[haystack_index++];

    // if we are not at the end of needle
    if (needle_index < needle.length) {
      needle_trail_hash = (needle_trail_hash << 8) | needle.bytes[needle_index++];
      // if we just processed the last character
      if (needle_index == needle.length) {
        // if needle is shorter than hash in bytes need to adjust the mask
        // so that we can continuously shift the hash and then mask off
        // unnecessary bytes from previous shifts
        const s64 needle_length_diff = sizeof(needle_trail_hash) - needle.length;
        // TODO Check if it would be beneficial to take 6 or 4 bits and increase
        //      the trail instead cutting off at 8 bytes. @LossyHash
        if (needle_length_diff > 0) trail_hash_mask >>= needle_length_diff * 8;
      } else {
        continue;
      }
    }

    // Check for hash match
    if (((needle_trail_hash ^ haystack_trail_hash) & trail_hash_mask) != 0) continue;

    u64 offset = haystack_index - needle.length;

    // Since we are using full bytes for trail hash if it matches
    // and needle is shorter than hash in bytes, can return early. @LossyHash
    if (needle.length <= sizeof(needle_trail_hash)) return offset;

    const char *match = haystack.bytes + offset;

    if (memcmp(match, needle.bytes, needle.length) != 0) continue;
    return offset;
  }

  return INDEX_OF_NOT_FOUND;
}

inline u64
slice_index_of(
  Slice haystack,
  Slice needle
) {
  return slice_index_of_in_range(haystack, needle, (Range_u64){0, haystack.length});
}

inline u64
slice_index_of_char_in_range(
  Slice haystack,
  char needle,
  Range_u64 range
) {
  Slice needle_slice = {.bytes = &needle, .length = 1};
  return slice_index_of_in_range(haystack, needle_slice, range);
}

inline u64
slice_index_of_char(
  Slice haystack,
  char needle
) {
  Slice needle_slice = {.bytes = &needle, .length = 1};
  return slice_index_of(haystack, needle_slice);
}

inline bool
slice_equal(
  const Slice a,
  const Slice b
) {
  if (a.length != b.length) return false;
  return memcmp(a.bytes, b.bytes, a.length) == 0;
}

const s8 ascii_case_insensitive_map[64] = {
  0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, // 0x41 == 'A'
  0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f,
  0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
  0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
  0x60, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, // 0x41 == 'A' (transformed from 'a')
  0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f,
  0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
  0x58, 0x59, 0x5a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f
};

inline bool
slice_ascii_case_insensitive_equal_bytes_internal(
  const s8 *a,
  const s8 *b,
  u64 length
) {
  for (u64 i = 0; i < length; ++i) {
    s8 a_char = a[i];
    s8 b_char = b[i];
    s8 is_strict_same = a_char == b_char;
    // Both 'a' and 'A' are above 0x40 which means that if signed compare
    // with that number is false we are definitely not dealing with letters
    // Signed compare means that codes above 127 are negative numbers and
    // are also excluded. This observation allows to shrink lookup table
    // to just 64 bytes that should fit into 1 cache line on most x64 CPUs
    s8 a_normalized = ascii_case_insensitive_map[(a_char - 0x40) & 0x3F];
    s8 b_normalized = ascii_case_insensitive_map[(b_char - 0x40) & 0x3F];
    s8 is_case_insensitive_same =
      (a_char > 0x40) & (b_char > 0x40) & (a_normalized == b_normalized);
    if (!(is_strict_same | is_case_insensitive_same)) return false;
  }
  return true;
}

bool
slice_ascii_case_insensitive_equal(
  const Slice a,
  const Slice b
) {
  if (a.length != b.length) return false;
  u64 *a_blocks = (u64 *)a.bytes;
  u64 *b_blocks = (u64 *)b.bytes;
  u64 block_length = a.length / sizeof(u64);
  // Going in 8 byte blocks for happy case
  for (u64 i = 0; i < block_length; ++i) {
    if (a_blocks[i] != b_blocks[i]) {
      if (!slice_ascii_case_insensitive_equal_bytes_internal(
        (s8 *)&a_blocks[i],
        (s8 *)&b_blocks[i],
        sizeof(u64)
      )) {
        return false;
      }
    }
  }
  u64 remaining_length = a.length % sizeof(u64);
  u64 remaining_index = a.length - remaining_length;
  return slice_ascii_case_insensitive_equal_bytes_internal(
    &a.bytes[remaining_index],
    &b.bytes[remaining_index],
    remaining_length
  );
}

inline bool
slice_contains(
  const Slice haystack,
  const Slice needle
) {
  return slice_index_of(haystack, needle) != INDEX_OF_NOT_FOUND;
}

inline Slice
slice_sub(
  const Slice slice,
  u64 from,
  u64 to
) {
  if (to > slice.length) to = slice.length;
  if (from > to) from = to;
  return (Slice){
    .bytes = slice.bytes + from,
    .length = to - from,
  };
}

inline Slice
slice_sub_range(
  const Slice slice,
  Range_u64 range
) {
  return slice_sub(slice, range.from, range.to);
}

inline Slice
slice_sub_length(
  const Slice slice,
  u64 start,
  u64 length
) {
  u64 end = start + length;
  return slice_sub(slice, start, end);
}

Array_Slice
slice_split_by_slice(
  const Allocator *allocator,
  const Slice slice,
  const Slice separator
) {
  if (!slice.length) {
    return dyn_array_make(Array_Slice, .allocator = allocator, .capacity = 0);
  }
  // In case of zero-length separator return individual bytes as slices
  if (separator.length == 0) {
    Array_Slice result = dyn_array_make(
      Array_Slice,
      .allocator = allocator,
      .capacity = slice.length
    );
    for (u64 i = 0; i < slice.length; ++i) {
      dyn_array_push(result, slice_sub_length(slice, i, 1));
    }
    return result;
  }
  Array_Slice result = dyn_array_make(Array_Slice, .allocator = allocator);
  Slice remaining = slice;
  for(;;) {
    u64 index = slice_index_of(remaining, separator);
    if(index == INDEX_OF_NOT_FOUND) {
      dyn_array_push(result, remaining);
      break;
    } else {
      dyn_array_push(result, (Slice){
        .bytes = remaining.bytes,
        .length = index,
      });
      remaining = slice_sub(remaining, index + separator.length, remaining.length);
    }
  }
  return result;
}

Array_Slice
slice_split_by_length(
  const Allocator *allocator,
  Slice slice,
  u64 length
) {
  if (!slice.length || !length) {
    return dyn_array_make(Array_Slice, .allocator = allocator, .capacity = 0);
  }
  Array_Slice result = dyn_array_make(Array_Slice, .allocator = allocator);
  u64 item_count = slice.length / length + 1;
  for (u64 i = 0; i < item_count; ++i) {
    dyn_array_push(result, slice_sub_length(slice, i * length, length));
  }
  return result;
}


typedef bool (*slice_split_callback)(s32 ch);
typedef bool (*slice_split_callback_indexed)(s32 ch, u64 index);

Array_Slice
slice_split_by_callback_internal(
  const Allocator *allocator,
  Slice slice,
  slice_split_callback_indexed callback,
  bool indexed
) {
  if (!slice.length || !callback) {
    return dyn_array_make(Array_Slice, .allocator = allocator);
  }
  Array_Slice result = dyn_array_make(Array_Slice, .allocator = allocator);
  bool previous_is_separator = false;
  u64 prev_index = 0;
  for (u64 index = 0; index < slice.length; ++index) {
    s8 ch = slice.bytes[index];
    bool is_separator = indexed
      ? callback(ch, index)
      : ((slice_split_callback)(callback))(ch);
    if (is_separator && !previous_is_separator) {
      previous_is_separator = true;
      dyn_array_push(result, slice_sub(slice, prev_index, index));
    } else if (!is_separator && previous_is_separator) {
      previous_is_separator = false;
      prev_index = index;
    }
  }
  dyn_array_push(result, slice_sub_length(slice, prev_index, slice.length));
  return result;
}

inline Array_Slice
slice_split_by_callback(
  const Allocator *allocator,
  Slice slice,
  slice_split_callback callback
) {
  return slice_split_by_callback_internal(
    allocator, slice, (slice_split_callback_indexed)callback, false
  );
}

inline Array_Slice
slice_split_by_callback_indexed(
  const Allocator *allocator,
  Slice slice,
  slice_split_callback_indexed callback
) {
  return slice_split_by_callback_internal(
    allocator, slice, callback, true
  );
}

Slice
slice_dirname(
  Slice path
) {
  u64 last_slash = 0;
  for (u64 i = 0; i < path.length; ++i) {
    if (path.bytes[i] == '\\' || path.bytes[i] == '/') last_slash = i;
  }
  return slice_sub(path, 0, last_slash);
}

Slice
slice_basename(
  Slice path
) {
  u64 after_last_slash = 0;
  for (u64 i = 0; i < path.length; ++i) {
    if (path.bytes[i] == '\\' || path.bytes[i] == '/') after_last_slash = i + 1;
  }
  return slice_sub(path, after_last_slash, path.length);
}

#ifdef _WIN32
static const s8 system_path_separator = '\\';
#else
static const s8 system_path_separator = '/';
#endif

Slice
slice_normalize_path(
  const Allocator *allocator,
  Slice path
) {
  u64 after_last_slash = 0;
  u64 available_segments = 0;
  s8 *buffer = allocator_allocate_bytes(allocator, path.length, sizeof(s8));
  s64 buffer_length = 0;

  static const Slice dot = slice_literal_fields(".");
  static const Slice dotdot = slice_literal_fields("..");
  for (u64 i = 0; i < path.length; ++i) {
    if (path.bytes[i] == '\\' || path.bytes[i] == '/') {
      Slice segment = slice_sub(path, after_last_slash, i);
      after_last_slash = i + 1;
      if (slice_equal(segment, dot)) continue;

      if (slice_equal(segment, dotdot)) {
        if (available_segments) {
          available_segments -= 1;

          // Find the start of last segment
          s64 last_slash = 0;
          for (last_slash = buffer_length - 2; last_slash >= 0; --last_slash) {
            if (buffer[last_slash] == system_path_separator) break;
          }
          buffer_length = last_slash + 1;
          continue;
        }
      } else {
        available_segments += 1;
      }

      memcpy(buffer + buffer_length, segment.bytes, segment.length);
      buffer_length += segment.length;
      buffer[buffer_length] = system_path_separator;
      buffer_length += 1;
    }
  }

  // Copy last segment
  Slice segment = slice_sub(path, after_last_slash, path.length);
  memcpy(buffer + buffer_length, segment.bytes, segment.length);
  buffer_length += segment.length;

  buffer = allocator_reallocate(allocator, buffer, path.length, buffer_length, sizeof(s8));
  return (Slice){buffer, buffer_length};
}

//////////////////////////////////////////////////////////////////////////////
// Hex
//////////////////////////////////////////////////////////////////////////////

s8
hex_parse_digit(
  s8 byte,
  bool *ok
) {
  *ok = true;
  if (byte >= '0' && byte <= '9') return byte - '0';
  if (byte >= 'a' && byte <= 'f') return byte - 'a' + 10;
  if (byte >= 'A' && byte <= 'F') return byte - 'A' + 10;
  *ok = false;
  return 0;
}

Color_Rgba_8
hex_parse_color_digits(
  Slice color,
  bool *ok
) {
  s8 digits[8] = {0, 0, 0, 0, 0, 0, 0xf, 0xf};
  if (!ok) ok = &(bool){0};
  *ok = true;

  if (color.length == 3 || color.length == 4) {
    digits[0] = digits[1] = hex_parse_digit(color.bytes[0], ok);
    if (*ok) digits[2] = digits[3] = hex_parse_digit(color.bytes[1], ok);
    if (*ok) digits[4] = digits[5] = hex_parse_digit(color.bytes[2], ok);
    if (*ok && color.length == 4) {
      digits[6] = digits[7] = hex_parse_digit(color.bytes[3], ok);
    }
  } else if (color.length == 6 || color.length == 8) {
    for (u64 i = 0; *ok && i < color.length; ++i) {
      digits[i] = hex_parse_digit(color.bytes[i], ok);
    }
  } else {
    *ok = false;
  }
  if (!*ok) return (Color_Rgba_8){0};
  return (Color_Rgba_8) {
    .r = (digits[0] << 4) | digits[1],
    .g = (digits[2] << 4) | digits[3],
    .b = (digits[4] << 4) | digits[5],
    .a = (digits[6] << 4) | digits[7],
  };
}

//////////////////////////////////////////////////////////////////////////////
// C Strings
//////////////////////////////////////////////////////////////////////////////

inline bool
c_string_starts_with(
  const char *haystack,
  const char *needle
) {
  if (haystack == needle) return true;
  if (!needle) return true;
  if (!haystack) return false;
  while (*needle) {
    if (*needle++ != *haystack++) return false;
  }
  return true;
}

inline bool
c_string_ends_with(
  const char *haystack,
  const char *needle
) {
  return slice_ends_with(slice_from_c_string(haystack), slice_from_c_string(needle));
}

u64
c_string_index_of(
  const char *haystack,
  const char *needle
) {
  if (!haystack || !needle) return INDEX_OF_NOT_FOUND;

  u64 trail_hash_mask = 0xFFFFFFFFFFFFFFFFLLU;

  const char *haystack_ch = haystack;
  const char *needle_ch = needle;
  u64 needle_trail_hash = 0;
  u64 haystack_trail_hash = 0;
  s64 needle_length = 0;
  while (*haystack_ch) {
    haystack_trail_hash = (haystack_trail_hash << 8) | (*haystack_ch++);

    // if we are not at the end of needle
    if (*needle_ch != 0) {
      needle_trail_hash = (needle_trail_hash << 8) | (*needle_ch++);
      // if we just processed the last character
      if (*needle_ch == 0) {
        // if needle is shorter than hash in bytes need to adjust the mask
        // so that we can continuously shift the hash and then mask off
        // unnecessary bytes from previous shifts
        needle_length = needle_ch - needle;
        const s64 needle_length_diff = sizeof(needle_trail_hash) - needle_length;
        // TODO Check if it would be beneficial to take 6 or 4 bits and increase
        //      the trail instead cutting off at 8 bytes. @LossyHash
        if (needle_length_diff > 0) trail_hash_mask >>= needle_length_diff * 8;
      } else {
        continue;
      }
    }

    // Check for hash match
    if (((needle_trail_hash ^ haystack_trail_hash) & trail_hash_mask) != 0) continue;

    u64 offset = (haystack_ch - haystack) - needle_length;

    // Since we are using full bytes for trail hash if it matches
    // and needle is shorter than hash in bytes, can return early. @LossyHash
    if (needle_length <= sizeof(needle_trail_hash)) return offset;

    const char *match = haystack + offset;

    if (memcmp(match, needle, needle_length) != 0) continue;
    return offset;
  }

  return INDEX_OF_NOT_FOUND;
}

inline bool
c_string_contains(
  const char *haystack,
  const char *needle
) {
  return c_string_index_of(haystack, needle) != INDEX_OF_NOT_FOUND;
}

inline u64
c_string_length(
  const char *string
) {
  if (!string) return 0;
  return (u64)strlen(string);
}

inline s32
c_string_compare(
  const char *lhs,
  const char *rhs
) {
  return strcmp(lhs, rhs);
}

inline bool
c_string_equal(
  const char *lhs,
  const char *rhs
) {
  return c_string_compare(lhs, rhs) == 0;
}

inline Array_Slice
c_string_split_by_c_string(
  const Allocator *allocator,
  const char *string,
  const char *separator
) {
  if (!string || !separator) {
    return dyn_array_make(Array_Slice, .allocator = allocator, .capacity = 0);
  }
  return slice_split_by_slice(
    allocator,
    slice_from_c_string(string),
    slice_from_c_string(separator)
  );
}

Array_Slice
c_string_split_by_length(
  const Allocator *allocator,
  const char *string,
  u64 length
) {
  if (!string || !length) {
    return dyn_array_make(Array_Slice, .allocator = allocator, .capacity = 0);
  }
  Array_Slice result = dyn_array_make(Array_Slice, .allocator = allocator);
  const char *ch = string;
  u64 index = 0;
  while (*ch) {
    if (index != 0 && index % length == 0) {
      dyn_array_push(result, (Slice){string, ch - string});
      string = ch;
    }
    ++ch;
    ++index;
  }
  dyn_array_push(result, slice_from_c_string(string));
  return result;
}

typedef bool (*c_string_split_callback)(s32 ch);
typedef bool (*c_string_split_callback_indexed)(s32 ch,u64 index);

Array_Slice
c_string_split_by_callback_internal(
  const Allocator *allocator,
  const char *string,
  c_string_split_callback_indexed callback,
  bool indexed
) {
  if (!string || !callback) {
    return dyn_array_make(Array_Slice, .allocator = allocator, .capacity = 0);
  }
  Array_Slice result = dyn_array_make(Array_Slice, .allocator = allocator);
  const char *ch = string;
  bool previous_is_separator = false;
  u64 index = 0;
  while (*ch) {
    bool is_separator = indexed
      ? callback(*ch, index)
      : ((c_string_split_callback)(callback))(*ch);
    if (is_separator && !previous_is_separator) {
      previous_is_separator = true;
      dyn_array_push(result, (Slice){string, ch - string});
    } else if (!is_separator && previous_is_separator) {
      previous_is_separator = false;
      string = ch;
    }
    ++ch;
    ++index;
  }
  dyn_array_push(result, slice_from_c_string(string));
  return result;
}

inline Array_Slice
c_string_split_by_callback(
  const Allocator *allocator,
  const char *string,
  c_string_split_callback callback
) {
  return c_string_split_by_callback_internal(
    allocator, string, (c_string_split_callback_indexed)callback, false
  );
}

inline Array_Slice
c_string_split_by_callback_indexed(
  const Allocator *allocator,
  const char *string,
  c_string_split_callback_indexed callback
) {
  return c_string_split_by_callback_internal(allocator, string, callback, true);
}

//////////////////////////////////////////////////////////////////////////////
// Unicode
//////////////////////////////////////////////////////////////////////////////


inline bool
code_point_is_ascii_letter(
  s32 ch
) {
  return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z');
}

inline bool
code_point_is_digit(
  s32 code_point
) {
  return (code_point >= '0' && code_point <= '9');
}

inline bool
code_point_is_hex_digit(
  s32 code_point
) {
  return code_point_is_digit(code_point) ||
    (code_point >= 'a' && code_point <= 'f') ||
    (code_point >= 'A' && code_point <= 'F');
}

inline bool
code_point_is_ascii_alphanumeric(
  s32 ch
) {
  return code_point_is_ascii_letter(ch) || code_point_is_digit(ch);
}

inline bool
code_point_is_ascii_space(
  s32 ch
) {
  switch(ch) {
    case ' ':
    case '\n':
    case '\r':
    case '\t':
    case '\v':
    case '\f':
      return true;
    default:
      return false;
  }
}

// The `utf8_decode` function has following copyright and license
//
// Copyright (c) 2008-2009 Bjoern Hoehrmann <bjoern@hoehrmann.de>
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without restriction,
// including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software,
// and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
// OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
// See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.
//

#define UTF8_ACCEPT 0
#define UTF8_REJECT 12

static const u8 utf8_dfa_decode_table[] = {
  // The first part of the table maps bytes to character classes that
  // to reduce the size of the transition table and create bitmasks.
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
   8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,

  // The second part is a transition table that maps a combination
  // of a state of the automaton and a character class to a state.
   0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
  12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
  12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
  12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
  12,36,12,12,12,12,12,12,12,12,12,12,
};

inline u32
utf8_decode(
  u32* state,
  u32* code_point,
  u8 byte
) {
  u32 type = utf8_dfa_decode_table[byte];

  *code_point = (*state != UTF8_ACCEPT) ?
    (byte & 0x3fu) | (*code_point << 6) :
    (0xff >> type) & (byte);

  *state = utf8_dfa_decode_table[256 + *state + type];
  return *state;
}

inline s32
utf8_next_or_fffd(
  Slice utf8,
  u64 *index
) {
  u32 state = 0;
  u32 code_point = 0;
  for (; *index < utf8.length; (*index) += 1) {
    if (utf8_decode(&state, &code_point, utf8.bytes[*index])) {
      if (state == UTF8_REJECT) {
        code_point = 0xfffd;
        state = UTF8_ACCEPT;
      } else {
        continue;
      }
    }
    (*index) += 1;
    return code_point;
  }
  return -1;
}

/// Returns count of code_points in the string.
/// -1 return value indicates invalid UTF-8 sequence.
inline s64
utf8_code_point_length(
  Slice slice
) {
  u32 code_point = 0;
  s64 count = 0;
  u32 state = 0;

  for (u64 index = 0; index < slice.length; ++index) {
    if (!utf8_decode(&state, &code_point, slice.bytes[index])) ++count;
  }

  if (state != UTF8_ACCEPT) return -1;
  return count;
}

inline bool
utf8_is_ascii_byte(
  s8 byte
) {
  return byte > 0;
}

inline u64
utf8_code_point_byte_length(
  u32 code_point
) {
  if (code_point <= 0x7f) return 1;
  if (code_point <= 0x7ff) return 2;
  if (code_point <= 0xd7ff) return 3;
  if (code_point <= 0xdfff || code_point > 0x10ffff) return 0;
  if (code_point <= 0xffff) return 3;
  return 4;
}

/// The worst case here is for ascii bytes which require 1 byte in UTF-8
/// but require 2 bytes for UTF-16
inline u64
utf8_to_utf16_estimate_max_byte_length(
  Slice slice
) {
  return slice.length * 2;
}

/// The worst case here is for ascii bytes which require 1 byte in UTF-8
/// but require 4 bytes for UTF-32
inline u64
utf8_to_utf32_estimate_max_byte_length(
  Slice slice
) {
  return slice.length * 4;
}

/// Returns number of *bytes* written
/// Replaces invalid utf8 sequences with U+FFFD code point
u64
utf8_to_utf16_raw(
  Slice utf8,
  wchar_t *target,
  u64 target_byte_size
) {
  u64 target_wide_length = target_byte_size / 2;
  u64 target_index = 0;

  u64 index = 0;
  for (;;) {
    s32 code_point = utf8_next_or_fffd(utf8, &index);
    if (code_point == -1) break;

    if (code_point <= 0xFFFF) {
      if (target_index >= target_wide_length) return target_wide_length * 2;
      target[target_index++] = (wchar_t)code_point;
    } else {
      if (target_index + 1 >= target_wide_length) return target_wide_length * 2;
      target[target_index++] = (wchar_t)(0xD7C0 + (code_point >> 10));
      target[target_index++] = (wchar_t)(0xDC00 + (code_point & 0x3FF));
    }
  }

  return target_index * 2;
}

static_assert(sizeof(wchar_t) == 2, wchar_t_must_be_16_bit_wide);

inline wchar_t *
utf8_to_utf16_null_terminated(
  const Allocator *allocator,
  Slice utf8
) {
  u64 max_byte_length = utf8_to_utf16_estimate_max_byte_length(utf8);
  // + sizeof(wchar_t) accounts for null termination
  u64 allocation_size = max_byte_length + sizeof(wchar_t);
  wchar_t *target = allocator_allocate_bytes(allocator, allocation_size, sizeof(wchar_t));
  u64 bytes_written = utf8_to_utf16_raw(utf8, target, max_byte_length);
  u64 end_index = bytes_written / sizeof(wchar_t);
  target[end_index] = 0;
  u64 actual_size = bytes_written + sizeof(wchar_t);

  if (actual_size != allocation_size) {
    target = allocator_reallocate(allocator, target, allocation_size, actual_size, sizeof(wchar_t));
  }

  return target;
}

/// Accepts source (UTF-16) string size in *bytes*
/// The worst case here is for some asian characters that are 2 bytes in UTF16
/// but require 3 bytes for UTF-8
inline u64
utf16_to_utf8_estimate_max_byte_length(
  u64 size_in_bytes
) {
  return size_in_bytes * 3;
}

/// Returns number of *bytes* written
/// Accepts source (UTF-16) string size in *bytes*
/// Invalid code points and orphaned surrogate pair sides
/// are replaced with U+FFFD code_point
u64
utf16_to_utf8_raw(
  const wchar_t *source,
  u64 source_byte_size,
  s8 *target,
  u64 target_byte_size
) {
  u64 source_wide_length = source_byte_size / 2;
  u64 target_index = 0;

  for (u64 source_index = 0; source_index < source_wide_length; ++source_index) {
    u32 code_point = source[source_index];
    // Surrogate pair
    if (code_point >= 0xd800 && code_point <= 0xdbff) {
      ++source_index;
      if (source_index < source_wide_length) {
        wchar_t pair = source[source_index];
        // Valid pair
        if (pair >= 0xdc00 && pair <= 0xdfff) {
          code_point = (code_point - 0xd800) * 0x400 + (pair - 0xdc00) + 0x10000;
        } else {
          code_point = 0xfffd;
        }
      } else {
        // Orphaned surrogate pair at the end of the string
        code_point = 0xfffd;
      }
    }

    if (code_point <= 0x7f) {
      if (target_index >= target_byte_size) return target_byte_size;
      target[target_index] = (s8) code_point;
      target_index += 1;
    } else if (code_point <= 0x7ff) {
      if (target_index + 1 >= target_byte_size) return target_byte_size;
      target[target_index + 0] = (s8) (((code_point >> 6) & 0x1f) | 0xc0);
      target[target_index + 1] = (s8) (((code_point >> 0) & 0x3f) | 0x80);
      target_index += 2;
    } else if (code_point <= 0xffff) {
      if (target_index + 2 >= target_byte_size) return target_byte_size;
      target[target_index + 0] = (s8) (((code_point >> 12) & 0x0f) | 0xd0);
      target[target_index + 1] = (s8) (((code_point >>  6) & 0x3f) | 0x80);
      target[target_index + 2] = (s8) (((code_point >>  0) & 0x3f) | 0x80);
      target_index += 3;
    } else {
      if (target_index + 3 >= target_byte_size) return target_byte_size;
      target[target_index + 0] = (s8) (((code_point >> 18) & 0x07) | 0xf0);
      target[target_index + 1] = (s8) (((code_point >> 12) & 0x3f) | 0x80);
      target[target_index + 2] = (s8) (((code_point >>  6) & 0x3f) | 0x80);
      target[target_index + 3] = (s8) (((code_point >>  0) & 0x3f) | 0x80);
      target_index += 4;
    }
  }

  return target_index;
}

Slice
utf16_to_utf8(
  const Allocator *allocator,
  const wchar_t *source,
  u64 source_byte_size
) {
  u64 max_byte_length = utf16_to_utf8_estimate_max_byte_length(source_byte_size);
  s8 *target = allocator_allocate_bytes(allocator, max_byte_length, sizeof(s8));
  u64 bytes_written = utf16_to_utf8_raw(source, source_byte_size, target, max_byte_length);

  target = allocator_reallocate(allocator, target, max_byte_length, bytes_written, sizeof(s8));

  return (Slice) {
    .bytes = target,
    .length = bytes_written
  };
}

Slice
utf16_null_terminated_to_utf8(
  const Allocator *allocator,
  const wchar_t *source
) {
  u64 source_byte_size = wcslen(source) * sizeof(wchar_t);
  return utf16_to_utf8(allocator, source, source_byte_size);
}

//////////////////////////////////////////////////////////////////////////////
// LPEG
// Loosely based on ideas presented in a paper:
//   A Text Pattern-Matching Tool based on Parsing Expression Grammars
//   by Roberto Ierusalimschy in 2008
//////////////////////////////////////////////////////////////////////////////

typedef enum {
  Lpeg_Mode_Choice = 0,
  Lpeg_Mode_Sequence = 1,
} Lpeg_Mode;

typedef struct {
  u64 index;
  const Lpeg_Mode mode;
  bool success;
  bool done;
} Lpeg_Stack_Entry;

typedef struct {
  u64 index;
  bool success;
} Lpeg_Test_Result;

// Use real CPU stack to save previous LPEG stack and restore when nested code is done.
// If the mode is sequence we stop at the first failed nested pattern
// and in case of choice we stop at first success.
#define LPEG_SAVE_INTERNAL\
  if(lpeg->mode != lpeg->success) continue; else\
  for(\
    Lpeg_Stack_Entry *lpeg_saved = lpeg;\
    lpeg_saved;\
    (lpeg = lpeg_saved),(lpeg_saved = 0)\
  )

// "Push" new entry on LPEG stack by shadowing existing `lpeg` variable
#define LPEG_PUSH_INTERNAL(_mode_, _initial_success_)\
  Lpeg_Stack_Entry \
     lpeg_new = {\
       .index = lpeg_saved->index,\
       .mode = (_mode_),\
       .success = (_initial_success_),\
       .done = 0,\
     },\
     *lpeg = &lpeg_new

// Matches zero or more of the nested patterns
#define LPEG_MANY\
  LPEG_SAVE_INTERNAL\
  for( \
    LPEG_PUSH_INTERNAL(Lpeg_Mode_Sequence, 1);\
    !lpeg->done;\
    (\
      /* check that we made progress or a pattern like (a*)* will not terminate */\
      (lpeg->success = lpeg->success && lpeg_saved->index != lpeg->index),\
      (lpeg->done = !lpeg->success),\
      (lpeg->success && ((lpeg_saved->index = lpeg->index), 0))\
    )\
  )

// In both choices and sequences we only go through the loop once
// because of unconditional `lpeg->done = 1` at the end of the loop.
#define LPEG_CHOICE_SEQUENCE_INTERNAL(_mode_, _initial_success_)\
  LPEG_SAVE_INTERNAL\
  for( \
    LPEG_PUSH_INTERNAL((_mode_), (_initial_success_));\
    !lpeg->done;\
    (\
      (lpeg->done = 1),\
      (lpeg_saved->success = lpeg->success),\
      (lpeg->success && ((lpeg_saved->index = lpeg->index), 0))\
    )\
  )

#define LPEG_CHOICE LPEG_CHOICE_SEQUENCE_INTERNAL(Lpeg_Mode_Choice, 0)
#define LPEG_SEQUENCE LPEG_CHOICE_SEQUENCE_INTERNAL(Lpeg_Mode_Sequence, 1)

#define LPEG_NOT\
  if(lpeg->mode != lpeg->success) continue;\
  else for(\
    bool lpeg_not_loop = 1, lpeg_success_value = 0;\
    lpeg_not_loop;\
    (lpeg_not_loop = 0)\
  )

#define LPEG_CHAR_INTERNAL(_success_)\
  if(lpeg->mode != lpeg->success) continue;\
  else if (\
    lpeg->success = (\
      lpeg_input[lpeg->index] && ((_success_) ? lpeg_success_value : !lpeg_success_value)\
    ),\
    lpeg->success\
  ) ++lpeg->index;

#define LPEG_END\
  if(lpeg->mode != lpeg->success) continue;\
  else if (\
    lpeg->success = ((lpeg_input[lpeg->index] == 0) ? lpeg_success_value : !lpeg_success_value),\
    lpeg->success\
  ) ++lpeg->index;

#define LPEG_ANY\
  LPEG_CHAR_INTERNAL((lpeg_input[lpeg->index] != 0))

#define LPEG_RANGE(_from_, _to_)\
  LPEG_CHAR_INTERNAL((lpeg_input[lpeg->index] >= (_from_) && lpeg_input[lpeg->index] <= (_to_)))

#define LPEG_CHAR(_char_)\
  LPEG_CHAR_INTERNAL((lpeg_input[lpeg->index] == (_char_)))

#define LPEG_DIGIT LPEG_RANGE('0', '9')

#define LPEG_ASCII_LETTER\
  LPEG_CHAR_INTERNAL(code_point_is_ascii_letter(lpeg_input[lpeg->index]))

#define LPEG_ALPHANUMERIC\
  LPEG_CHAR_INTERNAL(code_point_is_ascii_alphanumeric(lpeg_input[lpeg->index]))

#define LPEG_TEST(_input_string_, _result_ptr_)\
  for (\
    Lpeg_Stack_Entry *lpeg = &(Lpeg_Stack_Entry){\
      .index = 0,\
      .mode = Lpeg_Mode_Sequence,\
      .success = 1,\
      .done = 0,\
    }, *lpeg_macro_done = 0;\
    !lpeg_macro_done;\
    (++lpeg_macro_done),\
    ((_result_ptr_)->index = lpeg->index),\
    ((_result_ptr_)->success = lpeg->success)\
  )\
  for(\
    bool lpeg_success_value_loop = 1, lpeg_success_value = 1;\
    lpeg_success_value_loop;\
    (lpeg_success_value_loop = 0),(void)lpeg_success_value\
  )\
  for (const char *lpeg_input = (_input_string_); lpeg_input; lpeg_input = 0)

inline bool
lpeg_string_internal(
  Lpeg_Stack_Entry *lpeg,
  const char *haystack,
  const char *needle
) {
  u64 index = lpeg->index;
  for (;*needle; ++index) {
    if (*needle++ != *(haystack + index)) {
      lpeg->success = false;
      return lpeg->mode == Lpeg_Mode_Sequence;
    }
  }
  lpeg->success = true;
  lpeg->index = index;
  return lpeg->mode == Lpeg_Mode_Choice;
}

#define LPEG_STRING(_string_)\
  if(lpeg_string_internal(lpeg, lpeg_input, (_string_))) continue


//////////////////////////////////////////////////////////////////////////////
// Fixed Buffer
//////////////////////////////////////////////////////////////////////////////

typedef struct {
  const Allocator *allocator;
  u64 capacity;
  u64 occupied;
  void *last_allocation;
  s8 memory[];
} Fixed_Buffer;

// Since the buffer can be used as an allocator we need to make sure
// That the first address is aligned to 16 bytes to match malloc
static_assert_type_alignment(Fixed_Buffer, 16);

struct Fixed_Buffer_Make_Options {
  void *_count_reset;
  u64 capacity;
  const Allocator *allocator;
};

inline Fixed_Buffer *
fixed_buffer_make_internal(
  struct Fixed_Buffer_Make_Options *options
) {
  u64 allocation_size = sizeof(Fixed_Buffer) + options->capacity;
  Fixed_Buffer *buffer = allocator_allocate_bytes(
    options->allocator,
    allocation_size,
    _Alignof(Fixed_Buffer)
  );
  memset(buffer, 0, allocation_size);
  *buffer = (Fixed_Buffer) {
    .allocator = options->allocator,
    .capacity = options->capacity,
    .occupied = 0,
    .last_allocation = 0,
  };
  return buffer;
}

#define fixed_buffer_make(...)\
  fixed_buffer_make_internal(&(struct Fixed_Buffer_Make_Options) {\
    .allocator = allocator_default,\
    .capacity = 256 - sizeof(Fixed_Buffer),\
    ._count_reset = 0,\
    __VA_ARGS__\
  })

#define fixed_buffer_stack_make(_capacity_)\
  &(\
    (union { Fixed_Buffer buffer; s8 memory[sizeof(Fixed_Buffer) + (_capacity_)]; })\
    { .buffer = { .capacity = (_capacity_) }}\
  ).buffer

inline Fixed_Buffer *
fixed_buffer_zero(
  Fixed_Buffer *buffer
) {
  memset(buffer->memory, 0, buffer->capacity);
  buffer->occupied = 0;
  return buffer;
}

inline void
fixed_buffer_destroy(
  Fixed_Buffer *buffer
) {
  if (buffer->allocator) allocator_deallocate(buffer->allocator, buffer);
}

inline void *
fixed_buffer_allocate_bytes(
  Fixed_Buffer *buffer,
  u64 byte_size,
  u64 alignment
) {
  u64 aligned_occupied = buffer->occupied;
  if (alignment != 1) {
    aligned_occupied = u64_align(aligned_occupied, alignment);
    byte_size = u64_align(byte_size, alignment);
  }
  assert(buffer->capacity >= byte_size + aligned_occupied);
  buffer->occupied = aligned_occupied;
  void *result = buffer->memory + buffer->occupied;
  buffer->occupied += byte_size;
  buffer->last_allocation = result;
  return result;
}

inline Slice
fixed_buffer_as_slice(
  Fixed_Buffer *buffer
) {
  return (Slice){.bytes = buffer->memory, .length = buffer->occupied};
}

#define fixed_buffer_allocate_unaligned(_buffer_, _type_)\
  ((_type_ *)fixed_buffer_allocate_bytes((_buffer_), sizeof(_type_), 1))

#define fixed_buffer_allocate_array(_buffer_, _type_, _count_)\
  ((_type_ *)fixed_buffer_allocate_bytes((_buffer_), (_count_) * sizeof(_type_), _Alignof(_type_)))

#define fixed_buffer_allocate(_buffer_, _type_)\
  ((_type_ *)fixed_buffer_allocate_bytes((_buffer_), sizeof(_type_), _Alignof(_type_)))

#define PRELUDE_PROCESS_TYPE(_type_)\
  inline _type_ *fixed_buffer_append_##_type_(Fixed_Buffer *buffer, _type_ value) {\
    _type_ *result = fixed_buffer_allocate_bytes(buffer, sizeof(_type_), 1);\
    *result = value;\
    return result;\
  }
PRELUDE_ENUMERATE_NUMERIC_TYPES
#undef PRELUDE_PROCESS_TYPE

inline Slice
fixed_buffer_append_slice(
  Fixed_Buffer *buffer,
  Slice slice
) {
  if (!slice.length) return (Slice) {0};
  void *target = fixed_buffer_allocate_bytes(buffer, slice.length, _Alignof(s8));
  memcpy(target, slice.bytes, slice.length);
  return (Slice) { .bytes = target, .length = slice.length, };
}

inline Slice
fixed_buffer_slice(
  Fixed_Buffer *buffer,
  Range_u64 range
) {
  u64 from = u64_min(buffer->occupied, range.from);
  u64 to = u64_min(buffer->occupied, range.to);
  return (Slice){.bytes = buffer->memory + from, to - from};
}

inline void *
fixed_buffer_allocator_allocate(
  Allocator_Handle handle,
  u64 size_in_bytes,
  u64 alignment
) {
  return fixed_buffer_allocate_bytes(
    (Fixed_Buffer *){handle.raw},
    size_in_bytes,
    alignment
  );
}
inline void
fixed_buffer_allocator_deallocate(
  Allocator_Handle handle,
  void *address
) {
  // noop
}

inline void *
fixed_buffer_allocator_reallocate(
  Allocator_Handle handle,
  void *address,
  u64 old_size_in_bytes,
  u64 new_size_in_bytes,
  u64 alignment
) {
  Fixed_Buffer *buffer = (Fixed_Buffer *)handle.raw;
    if (address == buffer->last_allocation) {
    buffer->occupied = (s8 *)address - buffer->memory;
    return fixed_buffer_allocate_bytes(buffer, new_size_in_bytes, alignment);
  }

  void *result = fixed_buffer_allocator_allocate(handle, new_size_in_bytes, alignment);
  memcpy(result, address, old_size_in_bytes);
  fixed_buffer_allocator_deallocate(handle, address);
  return result;
}

inline Allocator *
fixed_buffer_allocator_init(
  Fixed_Buffer *buffer,
  Allocator *allocator
) {
  *allocator = (Allocator){
    .allocate = fixed_buffer_allocator_allocate,
    .reallocate = fixed_buffer_allocator_reallocate,
    .deallocate = fixed_buffer_allocator_deallocate,
    .handle = {buffer},
  };
  return allocator;
}

inline Allocator *
fixed_buffer_allocator_make(
  Fixed_Buffer *buffer
) {
  return fixed_buffer_allocator_init(
    buffer,
    fixed_buffer_allocate(buffer, Allocator)
  );
}

inline s8 *
fixed_buffer_first_free_byte_address(
  Fixed_Buffer *buffer
) {
  return buffer->memory + buffer->occupied;
}

inline u64
fixed_buffer_remaining_capacity(
  Fixed_Buffer *buffer
) {
  return buffer->capacity - buffer->occupied;
}

inline Slice
fixed_buffer_wrap_snprintf(
  Fixed_Buffer *buffer,
  s32 bytes_written
) {
  if (bytes_written < 0) {
    assert(!"Buffer is too small");
    return (Slice) {0};
  }
  void *bytes = fixed_buffer_first_free_byte_address(buffer);
  buffer->occupied += bytes_written;
  return (Slice) { .bytes = bytes, .length = bytes_written, };
}

#define fixed_buffer_vsprintf(_buffer_, _format_, _va_)\
  fixed_buffer_wrap_snprintf((_buffer_), vsnprintf(\
    fixed_buffer_first_free_byte_address((_buffer_)),\
    fixed_buffer_remaining_capacity(_buffer_),\
    (_format_),\
    (_va_)\
  ))

#define fixed_buffer_sprintf(_buffer_, _format_, ...)\
  fixed_buffer_wrap_snprintf((_buffer_), snprintf(\
    fixed_buffer_first_free_byte_address(_buffer_),\
    fixed_buffer_remaining_capacity(_buffer_),\
    (_format_), \
    __VA_ARGS__\
  ))

typedef enum  {
  File_Read_Error_None,
  File_Read_Error_Failed_To_Open,
  File_Read_Error_Failed_To_Read,
} File_Read_Error;

struct Fixed_Buffer_From_File_Options {
  void *_count_reset;
  const Allocator *allocator;
  File_Read_Error *error;
  bool null_terminate;
};

#ifdef _WIN32
#include <windows.h>
#else
static_assert(false, reading_whole_file_is_not_implemented_on_nix_systems);
#endif

static Fixed_Buffer *
fixed_buffer_from_file_internal(
  Slice file_path,
  struct Fixed_Buffer_From_File_Options *options
) {
  // Allows to not check for null when reporting
  static File_Read_Error dummy_error = File_Read_Error_None;
  if (!options->error) options->error = &dummy_error;

#ifdef _WIN32
  wchar_t *file_path_utf16 = utf8_to_utf16_null_terminated(allocator_default, file_path);

  HANDLE file_handle = CreateFileW(
    file_path_utf16,
    GENERIC_READ,
    FILE_SHARE_READ,
    0,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    0
  );
  allocator_deallocate(allocator_default, file_path_utf16);
  Fixed_Buffer *buffer = 0;
  if (file_handle == INVALID_HANDLE_VALUE) {
    *options->error = File_Read_Error_Failed_To_Open;
    goto handle_error;
  }

  s32 buffer_size = GetFileSize(file_handle, 0);
  if (options->null_terminate) buffer_size++;
  buffer = fixed_buffer_make(
    .allocator = allocator_system,
    .capacity = s32_to_u64(buffer_size),
  );
  s32 bytes_read = 0;
  BOOL is_success = ReadFile(file_handle, buffer->memory, buffer_size, &bytes_read, 0);
  if (!is_success || bytes_read != buffer_size)  {
    *options->error = File_Read_Error_Failed_To_Read;
    goto handle_error;
  }
  buffer->occupied = s32_to_u64(bytes_read);
  if (options->null_terminate) fixed_buffer_append_u8(buffer, 0);
  return buffer;

  handle_error: {
    if (file_handle) CloseHandle(file_handle);
    if (buffer) fixed_buffer_destroy(buffer);
    return 0;
  }
#else
  assert(!"Not implemented");
#endif
}

#define fixed_buffer_from_file(_path_, ...)\
  fixed_buffer_from_file_internal((_path_), &(struct Fixed_Buffer_From_File_Options) {\
    .allocator = allocator_default,\
    .error = 0,\
    ._count_reset = 0,\
    __VA_ARGS__\
  })

//////////////////////////////////////////////////////////////////////////////
// Bucket Buffer
//////////////////////////////////////////////////////////////////////////////

typedef struct {
  u64 capacity;
  u64 occupied;
  s8 *memory;
} Bucket_Buffer_Bucket;

typedef dyn_array_type(Bucket_Buffer_Bucket) Array_Bucket_Buffer_Bucket;

typedef struct {
  const Allocator *allocator;
  u64 default_bucket_size;
  Array_Bucket_Buffer_Bucket buckets;
} Bucket_Buffer_Internal;

typedef struct {
  Bucket_Buffer_Internal *internal;
} Bucket_Buffer;

inline Bucket_Buffer_Bucket *
bucket_buffer_push_bucket_internal(
  Bucket_Buffer_Internal *internal,
  u64 capacity
) {
  return dyn_array_push(internal->buckets, (Bucket_Buffer_Bucket) {
    .occupied = 0,
    .capacity = capacity,
    .memory = allocator_allocate_bytes(internal->allocator, capacity, _Alignof(void *)),
  });
}

struct Bucket_Buffer_Make_Options {
  void *_count_reset;
  u64 bucket_capacity;
  const Allocator *allocator;
};

inline const Bucket_Buffer
bucket_buffer_make_internal(
  struct Bucket_Buffer_Make_Options *options
) {
  Bucket_Buffer_Internal *internal = allocator_allocate(options->allocator, Bucket_Buffer_Internal);
  *internal = (Bucket_Buffer_Internal) {
    .allocator = options->allocator,
    .buckets = dyn_array_make(Array_Bucket_Buffer_Bucket, 16),
    .default_bucket_size = options->bucket_capacity,
  };
  bucket_buffer_push_bucket_internal(internal, internal->default_bucket_size);
  return (Bucket_Buffer){internal};
}

#define bucket_buffer_make(...)\
  bucket_buffer_make_internal(&(struct Bucket_Buffer_Make_Options) {\
    .allocator = allocator_default,\
    .bucket_capacity = 4096,\
    ._count_reset = 0,\
    __VA_ARGS__\
  })

inline void
bucket_buffer_destroy(
  const Bucket_Buffer handle
) {
  Bucket_Buffer_Internal *internal = handle.internal;
  for (u64 i = 0; i < dyn_array_length(internal->buckets); ++i) {
    allocator_deallocate(internal->allocator, dyn_array_get(internal->buckets, i)->memory);
  }
  allocator_deallocate(internal->allocator, internal);
}

inline void *
bucket_buffer_allocate_bytes(
  const Bucket_Buffer handle,
  u64 byte_size,
  u64 alignment
) {
  Bucket_Buffer_Bucket *bucket = dyn_array_last(handle.internal->buckets);
  u64 aligned_occupied = bucket->occupied;
  if (alignment != 1) {
    aligned_occupied = u64_align(aligned_occupied, alignment);
    byte_size = u64_align(byte_size, alignment);
  }

  if (bucket->capacity < aligned_occupied + byte_size) {
    u64 capacity = handle.internal->default_bucket_size;
    if (byte_size > capacity) capacity = byte_size;
    bucket = bucket_buffer_push_bucket_internal(handle.internal, capacity);
  } else {
    bucket->occupied = aligned_occupied;
  }
  void *result = bucket->memory + bucket->occupied;
  bucket->occupied += byte_size;
  return result;
}

#define bucket_buffer_allocate(_buffer_, _type_)\
  ((_type_ *)bucket_buffer_allocate_bytes((_buffer_), sizeof(_type_), _Alignof(_type_)))

#define bucket_buffer_allocate_array(_buffer_, _type_, _count_)\
  ((_type_ *)bucket_buffer_allocate_bytes((_buffer_), (_count_) * sizeof(_type_), _Alignof(_type_)))

#define bucket_buffer_allocate_unaligned(_buffer_, _type_)\
  ((_type_ *)bucket_buffer_allocate_bytes((_buffer_), sizeof(_type_), 1))

inline Slice
bucket_buffer_append_slice(
  const Bucket_Buffer handle,
  Slice slice
) {
  Slice result = {0};
  if (!slice.length) return result;;
  s8 *bytes = bucket_buffer_allocate_bytes(handle, slice.length, 1);
  memcpy(bytes, slice.bytes, slice.length);
  return (Slice){.bytes = bytes, .length = slice.length};
}

inline u64
bucket_buffer_total_occupied(
  const Bucket_Buffer handle
) {
  u64 occupied = 0;
  Bucket_Buffer_Internal *internal = handle.internal;
  for (u64 i = 0; i < dyn_array_length(internal->buckets); ++i) {
    occupied += dyn_array_get(internal->buckets, i)->occupied;
  }
  return occupied;
}

Fixed_Buffer *
bucket_buffer_to_fixed_buffer(
  const Allocator *allocator,
  const Bucket_Buffer handle
) {
  Bucket_Buffer_Internal *internal = handle.internal;
  u64 total_occupied = bucket_buffer_total_occupied(handle);
  Fixed_Buffer *result = fixed_buffer_make(.allocator = allocator, .capacity = total_occupied);
  for (u64 i = 0; i < dyn_array_length(internal->buckets); ++i) {
    Bucket_Buffer_Bucket *bucket = dyn_array_get(internal->buckets, i);
    s8 *target = fixed_buffer_allocate_bytes(result, bucket->occupied, 1);
    memcpy(target, bucket->memory, bucket->occupied);
  }
  return result;
}

inline void *
bucket_buffer_allocator_allocate(
  Allocator_Handle handle,
  u64 size_in_bytes,
  u64 alignment
) {
  return bucket_buffer_allocate_bytes(
    (Bucket_Buffer){.internal = handle.raw},
    size_in_bytes,
    alignment
  );
}

inline void
bucket_buffer_allocator_deallocate(
  Allocator_Handle handle,
  void *address
) {
  // noop
}

inline void *
bucket_buffer_allocator_reallocate(
  Allocator_Handle handle,
  void *address,
  u64 old_size_in_bytes,
  u64 new_size_in_bytes,
  u64 alignment
) {
  void *result = bucket_buffer_allocator_allocate(handle, new_size_in_bytes, alignment);
  memcpy(result, address, old_size_in_bytes);
  bucket_buffer_allocator_deallocate(handle, address);
  return result;
}

inline Allocator *
bucket_buffer_allocator_init(
  const Bucket_Buffer buffer,
  Allocator *allocator
) {
  *allocator = (Allocator){
    .allocate = bucket_buffer_allocator_allocate,
    .reallocate = bucket_buffer_allocator_reallocate,
    .deallocate = bucket_buffer_allocator_deallocate,
    .handle = {buffer.internal},
  };
  return allocator;
}

inline Allocator *
bucket_buffer_allocator_make(
  const Bucket_Buffer buffer
) {
  return bucket_buffer_allocator_init(
    buffer,
    bucket_buffer_allocate(buffer, Allocator)
  );
}


//////////////////////////////////////////////////////////////////////////////
// Hash
//////////////////////////////////////////////////////////////////////////////

inline s32
hash_u64(
  u64 value
) {
  const u64 prime = 6903248743164425207llu;
  return (s32)(value * prime);
}

inline s32
hash_pointer(
  void *address
) {
  return hash_u64((u64)address);
}

static const s32 hash_byte_start = 7;

inline s32
hash_byte(
  s32 previous,
  s8 byte
) {
  return previous * 31 + byte;
}

s32
hash_bytes(
  const void *address,
  u64 size
) {
  const s8 *bytes = address;
  const s8 *end = bytes + size;
  s32 hash = hash_byte_start;
  while (bytes != end) hash = hash_byte(hash, *bytes++);
  return hash;
}

inline s32
hash_slice(
  Slice slice
) {
  return hash_bytes(slice.bytes, slice.length);
}

s32
hash_c_string(
  const char *string
) {
  s32 hash = hash_byte_start;
  while (*string) hash = hash_byte(hash, *string++);
  return hash;
}

//////////////////////////////////////////////////////////////////////////////
// HashMap
//////////////////////////////////////////////////////////////////////////////

typedef struct {
  bool occupied;
  s32 hash;
} Hash_Map_Entry_Bookkeeping;

#define hash_map_type_internal(_hash_map_type_, _key_type_, _value_type_)\
  typedef struct _hash_map_type_ _hash_map_type_;\
  \
  typedef struct {\
    _hash_map_type_ *(*make)();\
    _value_type_ *(*get)(_hash_map_type_ *, _key_type_);\
    void (*set)(_hash_map_type_ *, _key_type_, _value_type_);\
    void (*delete)(_hash_map_type_ *, _key_type_);\
    _value_type_ *(*get_by_hash)(_hash_map_type_ *, s32, _key_type_);\
    void (*set_by_hash)(_hash_map_type_ *, s32, _key_type_, _value_type_);\
    void (*delete_by_hash)(_hash_map_type_ *, s32, _key_type_);\
  } _hash_map_type_##__Methods;\
  \
  typedef struct _hash_map_type_##__Entry {\
    /* Needs to be synced with Hash_Map_Entry_Bookkeeping and is provided for convinence */\
    union {\
      bool occupied;\
      Hash_Map_Entry_Bookkeeping bookkeeping;\
    };\
    _key_type_ key;\
    _value_type_ value;\
  } _hash_map_type_##__Entry;\
  \
  typedef struct _hash_map_type_ {\
    const _hash_map_type_##__Methods *methods;\
    const Allocator *allocator;\
    s32 hash_mask;\
    u32 capacity_power_of_2;\
    u64 capacity;\
    u64 occupied;\
    _hash_map_type_##__Entry *entries;\
  } _hash_map_type_;

hash_map_type_internal(Hash_Map_Internal, void *, void *)

static u64
hash_map_get_insert_index_internal(
  Hash_Map_Internal *map,
  u64 entry_byte_size,
  s32 hash
) {
  s32 hash_index = hash & map->hash_mask;
  for (u64 i = 0; i < map->capacity; ++i) {
    // Using hash_mask to wrap around in a cheap way
    u64 effective_index = (i + hash_index) & map->hash_mask;
    Hash_Map_Entry_Bookkeeping *bookkeeping =
      (void *)((s8 *)map->entries + effective_index * entry_byte_size);
    if (!bookkeeping->occupied) return effective_index;
  }
  return 0;
}

static void
hash_map_resize(
  Hash_Map_Internal *map,
  u64 entry_byte_size
) {
  u64 previous_capacity = map->capacity;
  map->capacity_power_of_2++;
  map->capacity = 1llu << map->capacity_power_of_2;
  map->occupied = 0;
  map->hash_mask = u64_to_s32(map->capacity - 1);
  s8 *entries = (s8 *)map->entries;
  u64 entries_byte_size = entry_byte_size * map->capacity;
  map->entries = allocator_allocate_bytes(map->allocator, entries_byte_size, entry_byte_size);
  memset(map->entries, 0, entries_byte_size);

  // Rehash occupied entries
  for (u64 i = 0; i < previous_capacity; ++i) {
    void *old_entry = entries + i * entry_byte_size;
    Hash_Map_Entry_Bookkeeping *old_bookkeeping = old_entry;
    if (old_bookkeeping->occupied) {
      u64 insertion_index = hash_map_get_insert_index_internal(
        map, entry_byte_size, old_bookkeeping->hash
      );
      void *new_entry = (void *)((s8 *)map->entries + insertion_index * entry_byte_size);
      memcpy(new_entry, old_entry, entry_byte_size);
    }
  }
  allocator_deallocate(map->allocator, entries);
}

#define hash_map_template(_hash_map_type_, _key_type_, _value_type_, _key_hash_fn_, _key_equality_fn_)\
  hash_map_type_internal(_hash_map_type_, _key_type_, _value_type_)\
  \
  const _hash_map_type_##__Methods *_hash_map_type_##__methods;\
  \
  static inline _hash_map_type_ *\
  _hash_map_type_##__make(\
    const Allocator *allocator\
  ) {\
    _hash_map_type_ *map = allocator_allocate(allocator, _hash_map_type_);\
    u32 capacity_power_of_2 = 5;\
    u64 capacity = 1llu << capacity_power_of_2;\
    u64 entry_byte_size = sizeof(map->entries[0]);\
    u64 entry_array_byte_size = entry_byte_size * capacity;\
    *map = (_hash_map_type_) {\
      .methods = _hash_map_type_##__methods,\
      .allocator = allocator,\
      .capacity_power_of_2 = capacity_power_of_2,\
      .capacity = capacity,\
      .hash_mask = u64_to_s32(capacity - 1),\
      .occupied = 0,\
      .entries = allocator_allocate_bytes(allocator, entry_array_byte_size, entry_byte_size),\
    };\
    memset(map->entries, 0, entry_array_byte_size);\
    return map;\
  }\
  \
  static inline _hash_map_type_##__Entry *\
  _hash_map_type_##__get_by_hash_internal(\
    _hash_map_type_ *map,\
    s32 hash,\
    _key_type_ key\
  ) {\
    s32 hash_index = hash & map->hash_mask;\
    for (u64 i = 0; i < map->capacity; ++i) {\
      /* Using hash_mask to wrap around in a cheap way */\
      u64 effective_index = (i + hash_index) & map->hash_mask;\
      _hash_map_type_##__Entry *entry = &map->entries[effective_index];\
      /* We are past hash collisions - entry not found */\
      if (!entry->bookkeeping.occupied) break;\
      if (entry->bookkeeping.hash != hash) continue;\
      if (_key_equality_fn_(key, entry->key)) return entry;\
    }\
    return 0;\
  }\
  \
  static inline _value_type_ *\
  _hash_map_type_##__get_by_hash(\
    _hash_map_type_ *map,\
    s32 hash,\
    _key_type_ key\
  ) {\
    _hash_map_type_##__Entry *entry =\
      _hash_map_type_##__get_by_hash_internal(map, _key_hash_fn_(key), key);\
    return entry ? &entry->value : 0;\
  }\
  \
  static inline _value_type_ *\
  _hash_map_type_##__get(\
    _hash_map_type_ *map,\
    _key_type_ key\
  ) {\
    return _hash_map_type_##__get_by_hash(map, _key_hash_fn_(key), key);\
  }\
  \
  static inline void\
  _hash_map_type_##__delete_by_hash(\
    _hash_map_type_ *map,\
    s32 hash,\
    _key_type_ key\
  ) {\
    _hash_map_type_##__Entry *entry = _hash_map_type_##__get_by_hash_internal(map, hash, key);\
    if (entry) memset(entry, 0, sizeof(entry));\
  }\
  \
  static inline void\
  _hash_map_type_##__delete(\
    _hash_map_type_ *map,\
    _key_type_ key\
  ) {\
    _hash_map_type_##__delete_by_hash(map, _key_hash_fn_(key), key);\
  }\
  \
  static inline void\
  _hash_map_type_##__set_by_hash(\
    _hash_map_type_ *map,\
    s32 hash,\
    _key_type_ key,\
    _value_type_ value\
  ) {\
    u64 entry_size = sizeof(map->entries[0]);\
    Hash_Map_Internal *internal = (Hash_Map_Internal *)map;\
    /* Fast check for 50% occupancy */ \
    if (((++map->occupied) << 1) > map->capacity) {\
      hash_map_resize(internal, entry_size);\
    }\
    u64 index = hash_map_get_insert_index_internal(internal, entry_size, hash);\
    map->entries[index] = (_hash_map_type_##__Entry){\
      .bookkeeping.hash = hash, \
      .bookkeeping.occupied = true, \
      .key = key, \
      .value = value, \
    };\
  }\
  static inline void\
  _hash_map_type_##__set(\
    _hash_map_type_ *map,\
    _key_type_ key,\
    _value_type_ value\
  ) {\
    _hash_map_type_##__set_by_hash(map, _key_hash_fn_(key), key, value);\
  }\
  \
  const _hash_map_type_##__Methods *_hash_map_type_##__methods = &(_hash_map_type_##__Methods){\
    .make = _hash_map_type_##__make,\
    .get = _hash_map_type_##__get,\
    .set = _hash_map_type_##__set,\
    .delete = _hash_map_type_##__delete,\
    .get_by_hash = _hash_map_type_##__get_by_hash,\
    .set_by_hash = _hash_map_type_##__set_by_hash,\
    .delete_by_hash = _hash_map_type_##__delete_by_hash,\
  };\

#define hash_map_c_string_template(_hash_map_type_, _value_type_)\
  hash_map_template(_hash_map_type_, const char *, _value_type_, hash_c_string, c_string_equal)

#define hash_map_slice_template(_hash_map_type_, _value_type_)\
  hash_map_template(_hash_map_type_, Slice, _value_type_, hash_slice, slice_equal)

#define hash_map_make(_hash_map_type_)\
  _hash_map_type_##__make(allocator_default)

#define hash_map_destroy(_map_)\
  do {\
    Hash_Map_Internal *internal = (void *)(_map_);\
    allocator_deallocate(internal->allocator, internal->entries);\
    allocator_deallocate(internal->allocator, internal);\
  } while(0)

#define hash_map_get(_map_, _key_)\
  (_map_)->methods->get((_map_), (_key_))

#define hash_map_get_by_hash(_map_, _hash_, _key_)\
  (_map_)->methods->get_by_hash((_map_), _hash_, (_key_))

#define hash_map_has(_map_, _key_)\
  (hash_map_get((_map_), (_key_)) != 0)

#define hash_map_has_by_hash(_map_, _hash_, _key_)\
  (hash_map_get_by_hash((_map_), (_hash_), (_key_)) != 0)

#define hash_map_delete(_map_, _key_)\
  (_map_)->methods->delete((_map_), (_key_))

#define hash_map_delete_by_hash(_map_, _hash_, _key_)\
  (_map_)->methods->delete((_map_), (_hash_), (_key_))

#define hash_map_set(_map_, _key_, _value_)\
  (_map_)->methods->set((_map_), (_key_), (_value_))

#define hash_map_set_by_hash(_map_, _hash_, _key_, _value_)\
  (_map_)->methods->set_by_hash((_map_), (_hash_), (_key_), (_value_))

////////////////////////////////////////////////////////////////////
// Threads
////////////////////////////////////////////////////////////////////

typedef struct {
  void *native_handle;
} Thread;

typedef void (*Thread_Proc)(void *);

#ifdef _WIN32
struct Win32_Thread_Proc_And_Data {
  Thread_Proc proc;
  void *data;
};

DWORD
thread_make_win32_wrapper_proc(
  struct Win32_Thread_Proc_And_Data *raw_proc_and_data
) {
  Thread_Proc proc = raw_proc_and_data->proc;
  void *data = raw_proc_and_data->data;
  allocator_deallocate(allocator_system, raw_proc_and_data);
  proc(data);
  ExitThread(0);
}

Thread
thread_make(Thread_Proc proc, void *data) {
  struct Win32_Thread_Proc_And_Data *proc_and_data =
    allocator_allocate(allocator_system, struct Win32_Thread_Proc_And_Data);
  proc_and_data->proc = proc;
  proc_and_data->data = data;
  HANDLE handle = CreateThread(0, 0, thread_make_win32_wrapper_proc, proc_and_data, 0, &(DWORD){0});
  return (Thread){.native_handle = handle};
}

void
thread_join(Thread thread) {
  WaitForSingleObject(thread.native_handle, INFINITE);
}
#else
static_assert(false, TODO_implement_thread_wrappers)
#endif

////////////////////////////////////////////////////////////////////
// Debug
////////////////////////////////////////////////////////////////////

typedef struct {
  const Allocator *allocator;
  u64 size_in_bytes;
  u64 alignment;
  const char *function;
  const char *file;
  u64 line;
  void *result;
} Prelude_Allocation_Info;
typedef dyn_array_type(Prelude_Allocation_Info) Array_Prelude_Allocation_Info;

#ifdef PRELUDE_TRACK_ALLOCATION
  inline void *
  allocator_debug_allocate_bytes(
    const Allocator *allocator,
    u64 size_in_bytes,
    u64 alignment,
    const char *function,
    const char *file,
    u64 line
  ) {
    void *result = allocator_allocate_bytes_internal(allocator, size_in_bytes, alignment);
    Prelude_Allocation_Info *info = &(Prelude_Allocation_Info) {
      .allocator = allocator,
      .size_in_bytes = size_in_bytes,
      .alignment = alignment,
      .function = function,
      .file = file,
      .line = line,
      .result = result,
    };
    PRELUDE_TRACK_ALLOCATION(info);
    return result;
  }
#endif

#endif PRELUDE_H
