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
dynamic_array_ensure_capacity_internal(
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

#define dynamic_array_ensure_capacity(_array_, _count_)\
  dynamic_array_ensure_capacity_internal(\
    &((_array_).internal), sizeof((_array_).array->items[0]), (_count_) \
  )

#define array_push(_array_, ...)\
  (\
    (dynamic_array_ensure_capacity(_array_, 1)),\
    (*(_array_).array->next_free = (__VA_ARGS__)),\
    ((_array_).array->next_free++)\
  )

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

//////////////////////////////////////////////////////////////////////////////
// Allocator
//////////////////////////////////////////////////////////////////////////////

typedef struct {
  void *raw;
} Allocator_Handle;

typedef struct {
  void *(*const allocate)(Allocator_Handle handle, u64 size_in_bytes);
  void  (*const deallocate)(Allocator_Handle handle, void *address);
  void *(*const reallocate)(
    Allocator_Handle handle, void *address, u64 old_size_in_bytes, u64 new_size_in_bytes
  );
  Allocator_Handle handle;
} Allocator;

inline void *
allocator_default_allocate(
  Allocator_Handle handle,
  u64 size_in_bytes
) {
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
  u64 new_size_in_bytes
) {
  return realloc(address, new_size_in_bytes);
}

const Allocator allocator_default = {
  allocator_default_allocate,
  allocator_default_deallocate,
  allocator_default_reallocate
};

inline void *
allocator_allocate(
  const Allocator *allocator,
  u64 size_in_bytes
) {
  return allocator->allocate(allocator->handle, size_in_bytes);
}

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
  u64 new_size_in_bytes
) {
  return allocator->reallocate(allocator->handle, address, old_size_in_bytes, new_size_in_bytes);
}

//////////////////////////////////////////////////////////////////////////////
// HashMap
//////////////////////////////////////////////////////////////////////////////

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
  Hash_Map_Internal *map = allocator_allocate(allocator, sizeof(Hash_Map_Internal));
  u32 capacity_power_of_2 = 5;
  u64 capacity = 1llu << capacity_power_of_2;
  u64 entries_byte_size = entry_byte_size * capacity;
  *map = (Hash_Map_Internal) {
    .hash = hash_fn,
    .compare = comparator,
    .allocator = allocator,
    .capacity_power_of_2 = capacity_power_of_2,
    .capacity = capacity,
    .hash_mask = (s32)(capacity - 1),
    .occupied = 0,
    .entries = allocator_allocate(allocator, entries_byte_size),
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
  map->hash_mask = (s32)(map->capacity - 1);
  s8 *entries = map->entries;
  u64 entries_byte_size = entry_byte_size * map->capacity;
  map->entries = allocator_allocate(map->allocator, entries_byte_size);
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
