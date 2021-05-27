#ifndef VALUE_H
#define VALUE_H
#include "prelude.h"
#include <inttypes.h>
#include "types.h"

static const Source_File COMPILER_SOURCE_FILE = {
  .path = slice_literal_fields("__mass_compiler__"),
  .text = {0},
  .line_ranges = {&dyn_array_zero_items},
};

static const Source_Range COMPILER_SOURCE_RANGE = {
  .file = &COMPILER_SOURCE_FILE,
  .offsets = {0},
};

static const Calling_Convention calling_convention_x86_64_windows = {
  .register_volatile_bitset = (
    // Arguments
    (1llu << Register_C) | (1llu << Register_D) | (1llu << Register_R8) | (1llu << Register_R9) |

    // Return
    (1llu << Register_A) |

    // Other
    (1llu << Register_R10) | (1llu << Register_R11)
  ),
};

#define X86_64_SYSTEM_V_REGISTER_VOLATILE_BIT_SET (\
  /* Arguments */\
  (1llu << Register_DI) | (1llu << Register_SI) | (1llu << Register_D) |\
  (1llu << Register_C) | (1llu << Register_R8) | (1llu << Register_R9) |\
  /* Varargs / Return */\
  (1llu << Register_A) |\
  /* Other */\
  (1llu << Register_R10) | (1llu << Register_R11)\
)

static const Calling_Convention calling_convention_x86_64_linux = {
  .register_volatile_bitset = X86_64_SYSTEM_V_REGISTER_VOLATILE_BIT_SET,
};

static const Calling_Convention calling_convention_x86_64_darwin = {
  .register_volatile_bitset = X86_64_SYSTEM_V_REGISTER_VOLATILE_BIT_SET,
};

static Array_Value_Ptr empty_value_array = {
  .internal = &(Dyn_Array_Internal){.allocator = &allocator_static},
};

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

static inline void
register_bitset_set(
  u64 *bitset,
  Register reg
);

static inline void
register_bitset_unset(
  u64 *bitset,
  Register reg
);

static inline bool
register_bitset_get(
  u64 bitset,
  Register reg
);

#define MASS_ON_ERROR(...)\
  if ((__VA_ARGS__).tag != Mass_Result_Tag_Success)

#define MASS_TRY(...)\
  for (Mass_Result _result = (__VA_ARGS__); _result.tag != Mass_Result_Tag_Success;) return _result;

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
  .compiler_source_location = COMPILER_SOURCE_LOCATION_GLOBAL_FIELDS,
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
  u32 length
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

void
program_jit(
  Jit *jit
);

static inline Label *
program_get_label(
  Program *program,
  Label_Index label
);

void
program_patch_labels(
  Program *program
);

void
program_set_label_offset(
  Program *program,
  Label_Index label_index,
  u32 offset_in_section
);

#endif
