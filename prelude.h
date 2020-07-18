#ifndef PRELUDE_H
#define PRELUDE_H

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#ifndef countof
#define countof(_array_)\
  (sizeof(_array_) / sizeof((_array_)[0]))
#endif

#define static_assert(_condition_, _message_)\
  struct _message_ { int _message_ : !!(_condition_); }


#define static_assert_type_alignment(_type_, _alignment_)\
  static_assert(\
    sizeof(_type_) % (_alignment_) == 0,\
    _type_##__expected_to_be_aligned_to__##_alignment_\
  );

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
// Allocator
//////////////////////////////////////////////////////////////////////////////
typedef struct {
  void *raw;
} Allocator_Handle;

typedef struct {
  void *(*const allocate)(Allocator_Handle handle, u64 size_in_bytes, u64 alignment);
  void  (*const deallocate)(Allocator_Handle handle, void *address);
  void *(*const reallocate)(
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

const Allocator allocator_default = {
  allocator_default_allocate,
  allocator_default_deallocate,
  allocator_default_reallocate
};

#ifdef PRELUDE_WIN32
#include <windows.h>
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

const Allocator allocator_system = {
  allocator_system_allocate,
  allocator_system_deallocate,
  allocator_system_reallocate,
};
#endif

inline void *
allocator_allocate_bytes(
  const Allocator *allocator,
  u64 size_in_bytes,
  u64 alignment
) {
  return allocator->allocate(allocator->handle, size_in_bytes, alignment);
}

#define allocator_allocate(_allocator_, _type_)\
  (_type_ *)allocator_allocate_bytes((_allocator_), sizeof(_type_), sizeof(_type_))


#define allocator_allocate_array(_allocator_, _type_, _count_)\
  (_type_ *)allocator_allocate_bytes((_allocator_), sizeof(_type_) * (_count_), sizeof(_type_))

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

s32
hash_bytes(
  const void *address,
  u32 size
) {
  const s8 *bytes = address;
  const s8 *end = bytes + size;
  s32 hash = 7;
  while (bytes != end) {
    hash = hash * 31 + *bytes++;
  }
  return hash;
}

s32
hash_c_string(
  const char *string
) {
  s32 hash = 7;
  while (*string) {
    hash = hash * 31 + *string++;
  }
  return hash;
}

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

inline Bucket_Array_Internal *
bucket_array_alloc_internal(
  const Allocator *allocator,
  u64 single_bucket_byte_size,
  u64 item_byte_size,
  u64 initial_bucket_tail_capacity
) {
  u64 allocation_size =
    sizeof(Bucket_Array_Internal) +
    sizeof(Bucket_Array_Bucket_Internal) * initial_bucket_tail_capacity;
  Bucket_Array_Internal *result = allocator_allocate_bytes(
    allocator,
    allocation_size,
    sizeof(Bucket_Array_Bucket_Internal)
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

#define bucket_array_make_with_allocator(_allocator_, _array_type_)\
  ((_array_type_) {\
    .internal = bucket_array_alloc_internal(\
      (_allocator_),\
      sizeof(*((_array_type_ *) 0)->typed->bucket_tail[0].items),\
      sizeof((*((_array_type_ *) 0)->typed->bucket_tail[0].items)[0]),\
      BUCKET_ARRAY_INITIAL_TAIL_CAPACITY\
    )\
  })

#define bucket_array_make(_array_type_)\
  bucket_array_make_with_allocator(&allocator_default, _array_type_)

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

inline Dyn_Array_Internal *
dyn_array_alloc_internal(
  const Allocator *allocator,
  u64 item_byte_size,
  u64 capacity
) {
  u64 new_allocation_size = sizeof(Dyn_Array_Internal) + item_byte_size * capacity;
  Dyn_Array_Internal *result = allocator_allocate_bytes(
    allocator, new_allocation_size, item_byte_size
  );
  *result = (Dyn_Array_Internal) {
    .allocator = allocator,
    .capacity = capacity,
  };
  return result;
}

#define dyn_array_is_initialized(_array_)\
  (!!(_array_).data)

#define dyn_array_capacity(_array_)\
  ((_array_).data->capacity)

#define dyn_array_length(_array_)\
  ((_array_).data->length)

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

#define dyn_array_get_unsafe(_array_, _index_)\
  (&(_array_).data->items[_index_])

#define dyn_array_clear(_array_)\
  ((_array_).data->length = 0)

inline void
dyn_array_ensure_capacity(
  Dyn_Array_Internal **internal,
  u64 item_byte_size
) {
  if ((*internal)->length + 1 <= (*internal)->capacity) return;
  u64 new_capacity = (*internal)->capacity + ((*internal)->capacity >> 1);
  *internal = dyn_array_realloc_internal(
    *internal, (*internal)->length, item_byte_size, new_capacity
  );
}

#define dyn_array_item_size(_array_type_)\
  sizeof(((_array_type_ *) 0)->data->items[0])

#define dyn_array_make_with_allocator(_allocator_, _array_type_, _count_)\
  ((_array_type_) {\
    .internal = dyn_array_alloc_internal(\
      (_allocator_),\
      dyn_array_item_size(_array_type_),\
      (_count_)\
    )\
  })

#define dyn_array_make(_array_type_, _count_)\
  dyn_array_make_with_allocator(&allocator_default, _array_type_, (_count_))

#define dyn_array_destroy(_array_)\
  allocator_deallocate((_array_).data->allocator, (_array_).data)

#define dyn_array_push(_array_, ...)\
  (\
    dyn_array_ensure_capacity(&((_array_).internal), sizeof((_array_).data->items[0])),\
    ((_array_).data->items[(_array_).data->length] = (__VA_ARGS__)),\
    &(_array_).data->items[(_array_).data->length++]\
  )

#define dyn_array_pop(_array_, ...)\
  (dyn_array_length(_array_) > 0 ? &(_array_).data->items[((_array_).data->length--) - 1] : 0)

inline void
dyn_array_delete_internal(
  Dyn_Array_Internal *internal,
  u64 item_byte_size,
  u64 index
) {
  assert(index < internal->length);
  u64 item_offset = item_byte_size * index;
  s8 *item_pointer = internal->items + item_offset;
  u64 move_size = (item_byte_size * internal->length) - item_offset - item_byte_size;
  memmove(item_pointer, item_pointer + item_byte_size, move_size);
  internal->length--;
}

#define dyn_array_delete(_array_, _index_)\
  dyn_array_delete_internal(\
    (_array_).internal,\
    sizeof((_array_).data->items[0]),\
    (_index_)\
  )

#define dyn_array_last(_array_)\
  (dyn_array_length(_array_) > 0 ? &(_array_).data->items[(_array_).data->length - 1] : 0)

//////////////////////////////////////////////////////////////////////////////
// Slice
//////////////////////////////////////////////////////////////////////////////

typedef struct {
  const s8 *bytes;
  u64 length;
} Slice;

typedef dyn_array_type(Slice) Array_Slice;

typedef struct {
  s64 value;
  bool success;
} Slice_Parse_S64_Result;

/// Input slice is expected to just contain the number without any surrounding whitespace.
Slice_Parse_S64_Result
slice_parse_s64(
  Slice slice
) {
  Slice_Parse_S64_Result result = {0};
  int integer = 0;
  int multiplier = 1;
  for (s64 index = slice.length - 1; index >= 0; --index) {
    s8 digit = slice.bytes[index];
    if (digit >= '0' && digit <= '9') {
      integer += (digit - '0') * multiplier;
      multiplier *= 10;
    } else if (digit == '-') {
      if (index != 0) {
        return result;
      }
      integer = -integer;
    } else {
      return result;
    }
  }
  result.value = integer;
  result.success = true;
  return result;
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

#define slice_from_string_literal(_literal_)\
  ((Slice){ .bytes = (_literal_), .length = countof(_literal_) - 1, })

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
  char *result = allocator_allocate_bytes(allocator, slice.length + 1, sizeof(char));
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
    return dyn_array_make_with_allocator(allocator, Array_Slice, 0);
  }
  // In case of zero-length separator return individual bytes as slices
  if (separator.length == 0) {
    Array_Slice result = dyn_array_make_with_allocator(allocator, Array_Slice, (u32)slice.length);
    for (u64 i = 0; i < slice.length; ++i) {
      dyn_array_push(result, slice_sub_length(slice, i, 1));
    }
    return result;
  }
  Array_Slice result = dyn_array_make_with_allocator(allocator, Array_Slice, 8);
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
    return dyn_array_make_with_allocator(allocator, Array_Slice, 0);
  }
  Array_Slice result = dyn_array_make_with_allocator(allocator, Array_Slice, 8);
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
    return dyn_array_make_with_allocator(allocator, Array_Slice, 0);
  }
  Array_Slice result = dyn_array_make_with_allocator(allocator, Array_Slice, 8);
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
    return dyn_array_make_with_allocator(allocator, Array_Slice, 0);
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
    return dyn_array_make_with_allocator(allocator, Array_Slice, 0);
  }
  Array_Slice result = dyn_array_make_with_allocator(allocator, Array_Slice, 8);
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
    return dyn_array_make_with_allocator(allocator, Array_Slice, 0);
  }
  Array_Slice result = dyn_array_make_with_allocator(allocator, Array_Slice, 8);
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
  s32 ch
) {
  return (ch >= '0' && ch <= '9');
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

//////////////////////////////////////////////////////////////////////////////
// LPEG
// Loosly based on ideas presented in a paper:
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

// In both choices and sequences we only go through the loop ones
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
  u64 _padding;
  s8 memory[];
} Fixed_Buffer;

// Since the buffer can be used as an allocator we need to make sure
// That the first address is aligned to 16 bytes to match malloc
static_assert_type_alignment(Fixed_Buffer, 16);

inline Fixed_Buffer *
fixed_buffer_make(
  const Allocator *allocator,
  u64 capacity
) {
  u64 allocation_size = sizeof(Fixed_Buffer) + capacity;
  Fixed_Buffer *buffer = allocator_allocate_bytes(allocator, allocation_size, sizeof(s8));
  *buffer = (Fixed_Buffer) {
    .allocator = allocator,
    .capacity = capacity,
    .occupied = 0,
  };
  return buffer;
}

inline void
fixed_buffer_destroy(
  Fixed_Buffer *buffer
) {
  allocator_deallocate(buffer->allocator, buffer);
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
  assert(buffer->capacity - aligned_occupied >= byte_size);
  buffer->occupied = aligned_occupied;
  void *result = buffer->memory + buffer->occupied;
  buffer->occupied += byte_size;
  return result;
}

#define fixed_buffer_allocate_unaligned(_buffer_, _type_)\
  ((_type_ *)fixed_buffer_allocate_bytes((_buffer_), sizeof(_type_), 1))

#define fixed_buffer_allocate_array(_buffer_, _type_, _count_)\
  ((_type_ *)fixed_buffer_allocate_bytes((_buffer_), (_count_) * sizeof(_type_), sizeof(_type_)))

#define fixed_buffer_allocate(_buffer_, _type_)\
  ((_type_ *)fixed_buffer_allocate_bytes((_buffer_), sizeof(_type_), sizeof(_type_)))

#define PRELUDE_PROCESS_TYPE(_type_)\
  _type_ *fixed_buffer_append_##_type_(Fixed_Buffer *buffer, _type_ value) {\
    _type_ *result = fixed_buffer_allocate_bytes(buffer, sizeof(_type_), 1);\
    *result = value;\
    return result;\
  }
PRELUDE_ENUMERATE_NUMERIC_TYPES
#undef PRELUDE_PROCESS_TYPE

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
  void *result = fixed_buffer_allocator_allocate(handle, new_size_in_bytes, alignment);
  memcpy(result, address, old_size_in_bytes);
  fixed_buffer_allocator_deallocate(handle, address);
  return result;
}

Allocator *
fixed_buffer_create_allocator(
  Fixed_Buffer *buffer
) {
  Allocator *result = fixed_buffer_allocate(buffer, Allocator);
  Allocator temp = {
    .allocate = fixed_buffer_allocator_allocate,
    .reallocate = fixed_buffer_allocator_reallocate,
    .deallocate = fixed_buffer_allocator_deallocate,
    .handle = {buffer},
  };
  memcpy(result, &temp, sizeof(Allocator));
  return result;
}

//////////////////////////////////////////////////////////////////////////////
// Bucket Buffer
//////////////////////////////////////////////////////////////////////////////

typedef struct _Bucket_Buffer_Bucket {
  // TODO could save 16 bytes by extracting allocator and last_bucket
  //      to only be part of the root bucket
  const Allocator *allocator;
  struct _Bucket_Buffer_Bucket *last_bucket;
  struct _Bucket_Buffer_Bucket *next_bucket;
  u64 capacity;
  u64 occupied;
  u64 _padding;
  s8 memory[];
} Bucket_Buffer_Bucket;

// Since bucket buffer can be used as an allocator we need to make sure
// That the first address is aligned to 16 bytes to match malloc
static_assert_type_alignment(Bucket_Buffer_Bucket, 16);

typedef struct _Bucket_Buffer {
  Bucket_Buffer_Bucket *bucket;
} Bucket_Buffer;

typedef struct {
  const Allocator *allocator;
  u64 capacity;
} Bucket_Buffer_Options;

inline Bucket_Buffer_Bucket *
bucket_buffer_make_internal(
  const Allocator *allocator,
  u64 capacity
) {
  u64 allocation_size = sizeof(Bucket_Buffer_Bucket) + capacity;
  Bucket_Buffer_Bucket *bucket = allocator_allocate_bytes(allocator, allocation_size, sizeof(s8));
  *bucket = (Bucket_Buffer_Bucket) {
    .allocator = allocator,
    .capacity = capacity,
    .occupied = 0,
  };
  return bucket;
}

inline const Bucket_Buffer
bucket_buffer_make_with_options(
  const Bucket_Buffer_Options options
) {
  const Allocator *allocator = options.allocator ? options.allocator : &allocator_default;
  u64 capacity = options.capacity ? options.capacity : (4096 - sizeof(Bucket_Buffer_Bucket));
  Bucket_Buffer_Bucket *bucket = bucket_buffer_make_internal(allocator, capacity);
  bucket->last_bucket = bucket;
  return (Bucket_Buffer){bucket};
}

inline const Bucket_Buffer
bucket_buffer_make() {
  return bucket_buffer_make_with_options((const Bucket_Buffer_Options){0});
}

void
bucket_buffer_destroy_internal(
  const Allocator *allocator,
  Bucket_Buffer_Bucket *bucket
) {
  if (bucket->next_bucket) bucket_buffer_destroy_internal(allocator, bucket->next_bucket);
  allocator_deallocate(allocator, bucket);
}

inline void
bucket_buffer_destroy(
  const Bucket_Buffer handle
) {
  bucket_buffer_destroy_internal(handle.bucket->allocator, handle.bucket);
}

inline void *
bucket_buffer_allocate_bytes(
  const Bucket_Buffer handle,
  u64 byte_size,
  u64 alignment
) {
  Bucket_Buffer_Bucket *bucket = handle.bucket->last_bucket;

  u64 aligned_occupied = bucket->occupied;
  if (alignment != 1) {
    aligned_occupied = u64_align(aligned_occupied, alignment);
    byte_size = u64_align(byte_size, alignment);
  }

  if (bucket->capacity - aligned_occupied < byte_size) {
    u64 capacity = handle.bucket->capacity;
    if (byte_size > capacity) capacity = byte_size;
    bucket = bucket_buffer_make_internal(handle.bucket->allocator, capacity);
    handle.bucket->last_bucket->next_bucket = bucket;
    handle.bucket->last_bucket = bucket;
  } else {
    bucket->occupied = aligned_occupied;
  }
  void *result = bucket->memory + bucket->occupied;
  bucket->occupied += byte_size;
  return result;
}

#define bucket_buffer_allocate(_buffer_, _type_)\
  ((_type_ *)bucket_buffer_allocate_bytes((_buffer_), sizeof(_type_), sizeof(_type_)))

#define bucket_buffer_allocate_array(_buffer_, _type_, _count_)\
  ((_type_ *)bucket_buffer_allocate_bytes((_buffer_), (_count_) * sizeof(_type_), sizeof(_type_)))

inline void *
bucket_buffer_allocator_allocate(
  Allocator_Handle handle,
  u64 size_in_bytes,
  u64 alignment
) {
  return bucket_buffer_allocate_bytes(
    (Bucket_Buffer){.bucket = handle.raw},
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

Allocator *
bucket_buffer_create_allocator(
  const Bucket_Buffer buffer
) {
  Allocator *result = bucket_buffer_allocate(buffer, Allocator);
  Allocator temp = {
    .allocate = bucket_buffer_allocator_allocate,
    .reallocate = bucket_buffer_allocator_reallocate,
    .deallocate = bucket_buffer_allocator_deallocate,
    .handle = {buffer.bucket},
  };
  memcpy(result, &temp, sizeof(Allocator));
  return result;
}

//////////////////////////////////////////////////////////////////////////////
// HashMap
//////////////////////////////////////////////////////////////////////////////

typedef s32 (*hash_map_hash)(const void *);
typedef bool (*hash_map_key_equality)(const void *, const void *);

#define hash_map_entry_type(_key_type_, _value_type_)\
  struct {\
    Hash_Map_Entry_Bookkeeping bookkeeping;\
    _value_type_ value;\
    _key_type_ key;\
  }

#define hash_map_type_base_fields(_key_type_, _value_type_)\
  const Allocator *allocator;\
  s32 (*hash)(_key_type_);\
  bool (*compare)(_key_type_ *, _key_type_ *);\
  hash_map_entry_type(_key_type_, _value_type_) *get_result;\
  s32 hash_mask;\
  u32 capacity_power_of_2;\
  u64 capacity;\
  u64 occupied

typedef struct {
  bool occupied;
  s32 hash;
} Hash_Map_Entry_Bookkeeping;

typedef struct {
  hash_map_type_base_fields(const void *, void *);
  s8 *entries;
} Hash_Map_Internal;

#define hash_map_type(_key_type_, _value_type_)\
  union {\
    struct {\
      hash_map_type_base_fields(_key_type_, _value_type_);\
      hash_map_entry_type(_key_type_, _value_type_) *entries;\
    } typed;\
    Hash_Map_Internal internal;\
    s8 (*key_offset_dummy)[offsetof(hash_map_entry_type(_key_type_, _value_type_), key)];\
  }

static inline void *
hash_map_make_internal(
  const Allocator *allocator,
  u64 entry_byte_size,
  hash_map_hash hash_fn,
  hash_map_key_equality comparator
) {
  Hash_Map_Internal *map = allocator_allocate(allocator, Hash_Map_Internal);
  u32 capacity_power_of_2 = 5;
  u64 capacity = 1llu << capacity_power_of_2;
  u64 entries_byte_size = entry_byte_size * capacity;
  *map = (Hash_Map_Internal) {
    .hash = hash_fn,
    .compare = comparator,
    .allocator = allocator,
    .capacity_power_of_2 = capacity_power_of_2,
    .capacity = capacity,
    .hash_mask = u64_to_s32(capacity - 1),
    .occupied = 0,
    .entries = allocator_allocate_bytes(allocator, entries_byte_size, entry_byte_size),
  };
  memset(map->entries, 0, entries_byte_size);
  return map;
}

static inline bool
hash_map_c_string_equal(
  const char **a,
  const char **b
) {
  return strcmp(*a, *b) == 0;
}

#define hash_map_make(_map_type_, _hash_fn_, _comparator_)\
  hash_map_make_internal(\
    &allocator_default,\
    sizeof(((_map_type_ *) 0)->typed.entries[0]) + \
      /* The line below will be optimized out, but it ensures type safety */\
      0 * sizeof((_map_type_){\
        .typed = {.hash = (_hash_fn_), .compare = (_comparator_)}\
      }),\
    (hash_map_hash)(_hash_fn_),\
    (hash_map_key_equality)(_comparator_)\
  )

#define hash_map_c_string_make(_map_type_)\
  hash_map_make(_map_type_, hash_c_string, hash_map_c_string_equal)

#define hash_map_destroy(_map_)\
  do {\
    allocator_deallocate((_map_)->typed.allocator, (_map_)->typed.entries);\
    allocator_deallocate((_map_)->typed.allocator, (_map_));\
  } while(0)

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
      (void *)(map->entries + effective_index * entry_byte_size);
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
  s8 *entries = map->entries;
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
      void *new_entry = (void *)(map->entries + insertion_index * entry_byte_size);
      memcpy(new_entry, old_entry, entry_byte_size);
    }
  }
  allocator_deallocate(map->allocator, entries);
}

static inline u64
hash_map_set_internal(
  Hash_Map_Internal *map,
  u64 entry_byte_size,
  s32 hash
) {
  // Fast check for 50% occupancy
  if (((++map->occupied) << 1) > map->capacity) {
    hash_map_resize(map, entry_byte_size);
  }

  u64 insertion_index = hash_map_get_insert_index_internal(map, entry_byte_size, hash);
  Hash_Map_Entry_Bookkeeping *bookkeeping =
    (void *)(map->entries + insertion_index * entry_byte_size);

  bookkeeping->occupied = true;
  bookkeeping->hash = hash;
  return insertion_index;
}

#define hash_map_set(_map_, _key_, _value_)\
  do {\
    u64 index = hash_map_set_internal(\
      &(_map_)->internal,\
      sizeof((_map_)->typed.entries[0]),\
      (_map_)->typed.hash(_key_)\
    );\
    ((_map_)->typed.entries[index].key = (_key_));\
    ((_map_)->typed.entries[index].value = (_value_));\
  } while (0)

static void *
hash_map_get_internal(
  Hash_Map_Internal *map,
  const void *key,
  u64 entry_byte_size,
  u64 key_offset_in_entry,
  s32 hash
) {
  s32 hash_index = hash & map->hash_mask;
  for (u64 i = 0; i < map->capacity; ++i) {
    // Using hash_mask to wrap around in a cheap way
    u64 effective_index = (i + hash_index) & map->hash_mask;
    s8 *entry = map->entries + effective_index * entry_byte_size;
    Hash_Map_Entry_Bookkeeping *bookkeeping = (void *)entry;
    // We are past hash collisions - entry not found
    if (!bookkeeping->occupied) break;
    void *payload_key = entry + key_offset_in_entry;
    if (map->compare(&key, payload_key)) {
      return entry;
    }
  }
  return 0;
}

#define hash_map_get_entry_internal(_map_, _key_)\
  hash_map_get_internal(\
    &(_map_)->internal,\
    (_key_),\
    sizeof((_map_)->typed.entries[0]),\
    sizeof(*(_map_)->key_offset_dummy),\
    (_map_)->typed.hash(_key_)\
  )

#define hash_map_has(_map_, _key_)\
  (hash_map_get_entry_internal((_map_), (_key_)) != 0)

#define hash_map_delete(_map_, _key_)\
  (\
    (_map_)->typed.get_result = hash_map_get_entry_internal((_map_), (_key_)),\
    ((_map_)->typed.get_result ? ((_map_)->typed.get_result->bookkeeping.occupied = 0) : 0)\
  )

#define hash_map_get(_map_, _key_)\
  (\
    (_map_)->typed.get_result = hash_map_get_entry_internal((_map_), (_key_)),\
    &(_map_)->typed.get_result->value\
  )

#endif PRELUDE_H
