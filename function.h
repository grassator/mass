#ifndef FUNCTION_H
#define FUNCTION_H

#include "prelude.h"
#include "value.h"

static void
program_init_startup_code(
  Mass_Context *context
);

#define MAX_ESTIMATED_TRAMPOLINE_SIZE 32

static u32
make_trampoline(
  Virtual_Memory_Buffer *buffer,
  s64 address
);

static void
move_value(
  Function_Builder *builder,
  const Scope *scope,
  const Source_Range *source_range,
  const Storage *target,
  const Storage *source
);

static void
fn_encode(
  Program *program,
  Virtual_Memory_Buffer *buffer,
  const Function_Builder *builder,
  Function_Layout *out_layout
);

// Register bitset manipulation


static Register
register_find_available(
  Function_Builder *builder,
  u64 register_disallowed_bit_mask
);

static inline void
register_bitset_set(
  u64 *bitset,
  Register reg
) {
  *bitset |= 1llu << reg;
}

static inline void
register_bitset_unset(
  u64 *bitset,
  Register reg
) {
  *bitset &= ~(1llu << reg);
}

static inline bool
register_bitset_get(
  u64 bitset,
  Register reg
) {
  return !!(bitset & (1llu << reg));
}

static inline u64
register_bitset_occupied_count(
  u64 bitset
) {
  return u64_count_set_bits(bitset);
}

static inline void
register_acquire_bitset(
  Function_Builder *builder,
  u64 to_acquire_bitset
) {
  assert(!(builder->register_occupied_bitset.bits & to_acquire_bitset));
  builder->register_occupied_bitset.bits |= to_acquire_bitset;
  builder->register_used_bitset.bits |= to_acquire_bitset;
}

static inline void
register_release_bitset(
  Function_Builder *builder,
  u64 to_release_bitset
) {
  assert((builder->register_occupied_bitset.bits & to_release_bitset) == to_release_bitset);
  builder->register_occupied_bitset.bits &= ~to_release_bitset;
}

static inline Register
register_acquire(
  Function_Builder *builder,
  Register reg_index
) {
  register_acquire_bitset(builder, 1llu << reg_index);
  return reg_index;
}

static inline Register
register_acquire_temp(
  Function_Builder *builder
) {
  return register_acquire(builder, register_find_available(builder, 0));
}

static inline void
register_release(
  Function_Builder *builder,
  Register reg_index
) {
  register_release_bitset(builder, 1llu << reg_index);
}

static u64
register_bitset_from_storage(
  const Storage *storage
);

#endif










