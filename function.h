#ifndef FUNCTION_H
#define FUNCTION_H

#include "prelude.h"
#include "value.h"

static Value *
ensure_function_instance(
  Execution_Context *context,
  Value *fn_value,
  Value_View args
);

static void
program_init_startup_code(
  Execution_Context *context
);

#define maybe_constant_fold(_context_, _builder_, _loc_, _result_, _a_, _b_, _operator_)\
  do {\
    const Expected_Result *fold_result = (_result_);\
    const Descriptor *fold_descriptor = expected_result_descriptor(fold_result);\
    const Source_Range *fold_range = (_loc_);\
    Execution_Context *fold_context = (_context_);\
    Value *fold_a = (_a_);\
    Value *fold_b = (_b_);\
    if (\
      fold_a->descriptor != &descriptor_lazy_value &&\
      fold_b->descriptor != &descriptor_lazy_value &&\
      fold_a->storage.tag == Storage_Tag_Static &&\
      fold_b->storage.tag == Storage_Tag_Static\
    ) {\
      if (descriptor_is_signed_integer(fold_descriptor)) {\
        fold_a = maybe_coerce_i64_to_integer(fold_context, fold_a, &descriptor_s64, fold_range);\
        fold_b = maybe_coerce_i64_to_integer(fold_context, fold_b, &descriptor_s64, fold_range);\
        s64 a_s64 = storage_static_value_up_to_s64(&fold_a->storage);\
        s64 b_s64 = storage_static_value_up_to_s64(&fold_b->storage);\
        s64 constant_result = a_s64 _operator_ b_s64;\
        return maybe_constant_fold_internal(fold_context, (_builder_), constant_result, fold_result, fold_range);\
      } else {\
        fold_a = maybe_coerce_i64_to_integer(fold_context, fold_a, &descriptor_u64, fold_range);\
        fold_b = maybe_coerce_i64_to_integer(fold_context, fold_b, &descriptor_u64, fold_range);\
        u64 a_u64 = storage_static_value_up_to_u64(&fold_a->storage);\
        u64 b_u64 = storage_static_value_up_to_u64(&fold_b->storage);\
        u64 constant_result = a_u64 _operator_ b_u64;\
        return maybe_constant_fold_internal(fold_context, (_builder_), constant_result, fold_result, fold_range);\
      }\
    }\
  } while(0)

#define MAX_ESTIMATED_TRAMPOLINE_SIZE 32

static u32
make_trampoline(
  Virtual_Memory_Buffer *buffer,
  s64 address
);

static void
move_value(
  Function_Builder *builder,
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

static void
load_address(
  Execution_Context *context,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *result_value,
  Storage storage
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
  assert(!(builder->register_occupied_bitset & to_acquire_bitset));
  builder->register_occupied_bitset |= to_acquire_bitset;
  builder->register_used_bitset |= to_acquire_bitset;
}

static inline void
register_release_bitset(
  Function_Builder *builder,
  Register to_release_bitset
) {
  assert((builder->register_occupied_bitset & to_release_bitset) == to_release_bitset);
  builder->register_occupied_bitset &= ~to_release_bitset;
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

#endif










