#ifndef VALUE_H
#define VALUE_H
#include "prelude.h"
#include <inttypes.h>
#include "types.h"

static const Source_File COMPILER_SOURCE_FILE = {
  .path = slice_literal_fields("__mass_compiler__"),
  .text = {0},
  .lines = {&dyn_array_zero_items},
};

static const Source_Range COMPILER_SOURCE_RANGE = {
  .file = &COMPILER_SOURCE_FILE,
  .offsets = {0},
};

typedef struct {
  u64 occupied;
} Temp_Mark;

static inline Temp_Mark
compilation_temp_mark(
  Compilation *compilation
) {
  return (Temp_Mark){compilation->temp_buffer.occupied};
}

static inline void
compilation_temp_reset_to_mark(
  Compilation *compilation,
  Temp_Mark mark
) {
  compilation->temp_buffer.occupied = mark.occupied;
}

static inline void
compilation_temp_reset(
  Compilation *compilation
) {
  compilation->temp_buffer.occupied = 0;
}

static inline Temp_Mark
context_temp_mark(
  Execution_Context *context
) {
  return compilation_temp_mark(context->compilation);
}

static inline void
context_temp_reset_to_mark(
  Execution_Context *context,
  Temp_Mark mark
) {
  compilation_temp_reset_to_mark(context->compilation, mark);
}

static inline void
context_temp_reset(
  Execution_Context *context
) {
  compilation_temp_reset(context->compilation);
}

static inline bool
storage_is_stack(
  const Storage *operand
);

static inline Storage
storage_register_for_descriptor(
  Register reg,
  const Descriptor *descriptor
);

static inline Storage
memory_layout_item_storage_at_index(
  const Storage *base,
  const Memory_Layout *layout,
  u64 index
);

static inline Storage
storage_stack(
  s32 offset,
  u64 byte_size,
  Stack_Area area
);

static inline Storage
storage_register(
  Register reg,
  u64 byte_size
);

static inline bool
register_is_xmm(
  Register reg
) {
  return !!(reg & Register_Xmm0);
}

static inline bool
storage_equal(
  const Storage *a,
  const Storage *b
);

#define storage_none ((Storage){.tag = Storage_Tag_None })

#define MASS_ON_ERROR(...)\
  if ((__VA_ARGS__).tag != Mass_Result_Tag_Success)

#define MASS_TRY(...)\
  for (Mass_Result _result = (__VA_ARGS__); _result.tag != Mass_Result_Tag_Success;) return _result;

#define MASS_MUST_SUCCEED(...)\
  do {\
    Mass_Result __result = (__VA_ARGS__);\
    (void)__result;\
    assert(__result.tag == Mass_Result_Tag_Success);\
  } while (0)\

PRELUDE_NO_DISCARD static inline Mass_Result
MASS_SUCCESS() {
  return (Mass_Result){.tag = Mass_Result_Tag_Success};
}

static Fixed_Buffer *
mass_error_to_string(
  Mass_Error const* error
);

static inline Value
type_value_for_descriptor(
  Descriptor *descriptor
) {
  return MASS_TYPE_VALUE(descriptor);
}

Value void_value = {
  .descriptor = &descriptor_void,
  .storage = { .tag = Storage_Tag_None },
};

static inline bool
descriptor_is_unsigned_integer(
  const Descriptor *descriptor
) {
  return (
    descriptor == &descriptor_u8  ||
    descriptor == &descriptor_u16 ||
    descriptor == &descriptor_u32 ||
    descriptor == &descriptor_u64
  );
}

static inline bool
descriptor_is_signed_integer(
  const Descriptor *descriptor
) {
  return (
    descriptor == &descriptor_s8  ||
    descriptor == &descriptor_s16 ||
    descriptor == &descriptor_s32 ||
    descriptor == &descriptor_s64
  );
}

static inline bool
descriptor_is_integer(
  const Descriptor *descriptor
) {
  return descriptor_is_signed_integer(descriptor) || descriptor_is_unsigned_integer(descriptor);
}

bool
descriptor_is_float(
  const Descriptor *descriptor
) {
  return descriptor == &descriptor_f32 || descriptor == &descriptor_f64;
}

static inline const Descriptor *
maybe_unwrap_pointer_descriptor(
  const Descriptor *descriptor
) {
  if (descriptor->tag == Descriptor_Tag_Pointer_To) {
    return descriptor->Pointer_To.descriptor;
  }
  if (descriptor->tag == Descriptor_Tag_Reference_To) {
    return descriptor->Reference_To.descriptor;
  }
  return descriptor;
}

static u64
descriptor_bit_size(
  const Descriptor *descriptor
);

static inline u64
descriptor_byte_size(
  const Descriptor *descriptor
);

static inline Descriptor *
descriptor_array_of(
  const Allocator *allocator,
  const Descriptor *item_descriptor,
  u64 length
);

static inline bool
same_type(
  const Descriptor *a,
  const Descriptor *b
);

static inline bool
same_value_type(
  Value *a,
  Value *b
);

static void
source_range_print_start_position(
  const Source_Range *source_range
);

static Slice
source_from_source_range(
  const Source_Range *source_range
);

static void *
rip_value_pointer(
  Program *program,
  Value *value
);

static inline bool
storage_is_label(
  const Storage *operand
);

static inline bool
storage_is_register_index(
  const Storage *storage,
  Register reg_index
);

static fn_type_opaque
value_as_function(
  Program *program,
  Value *value
);

static inline void *
rip_value_pointer_from_label_index(
  Program *program,
  Label_Index label_index
);

static void
print_storage(
  const Storage *operand
);


#endif
