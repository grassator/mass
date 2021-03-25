#ifndef VALUE_H
#define VALUE_H
#include "prelude.h"
#include <inttypes.h>
#include "types.h"
#include "encoding.h"

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
  return (const Mass_Result){ .tag = Mass_Result_Tag_Success };
}

PRELUDE_NO_DISCARD static inline Mass_Result
MASS_ERROR(Slice message, Source_Range source_range) {
  return (const Mass_Result){
    .tag = Mass_Result_Tag_Error,
    .Error.details = {
      .message = message,
      .source_range = source_range,
    }
  };
}


static inline Value
type_value_for_descriptor(
  Descriptor *descriptor
) {
  return MASS_TYPE_VALUE(descriptor);
}

// TODO Shrink to 4 bits when supported
MASS_DEFINE_OPAQUE_TYPE(register_8, 8);
MASS_DEFINE_OPAQUE_TYPE(register_16, 8);
MASS_DEFINE_OPAQUE_TYPE(register_32, 8);
MASS_DEFINE_OPAQUE_TYPE(register_64, 8);
MASS_DEFINE_OPAQUE_TYPE(immediate, sizeof(Slice));
MASS_DEFINE_OPAQUE_TYPE(eflags, sizeof(Storage_Eflags));

Value void_value = {
  .descriptor = &descriptor_void,
  .storage = { .tag = Storage_Tag_None },
  .compiler_source_location = COMPILER_SOURCE_LOCATION_GLOBAL_FIELDS,
};

Descriptor descriptor_any = {
  .tag = Descriptor_Tag_Any,
  .name = slice_literal_fields("any"),
};

Value *type_any_value = &(Value) {
  .descriptor = &descriptor_type,
  .storage = {
    .tag = Storage_Tag_Static,
    .Static.memory.Heap.pointer = &descriptor_any,
  },
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
  if (descriptor->tag == Descriptor_Tag_Pointer) {
    return descriptor->Pointer.to;
  }
  return descriptor;
}

u64
descriptor_byte_size(
  const Descriptor *descriptor
);

bool
same_type(
  const Descriptor *a,
  const Descriptor *b
);

bool
same_value_type(
  Value *a,
  Value *b
);

void
source_range_print_start_position(
  const Source_Range *source_range
);

Slice
source_from_source_range(
  const Source_Range *source_range
);

typedef enum {
  Instruction_Type_Assembly,
  Instruction_Type_Label,
  Instruction_Type_Bytes,
} Instruction_Type;

typedef struct {
  const X64_Mnemonic *mnemonic;
  Storage operands[3];
} Instruction_Assembly;

#define INSTRUCTION_BYTES_NO_LABEL 255
typedef struct {
  u8 memory[15];
  u8 length;
  Label_Index label_index;
  u8 label_offset_in_instruction;
} Instruction_Bytes;

typedef struct {
  Instruction_Type type;
  union {
    Instruction_Assembly assembly;
    Label_Index label;
    Instruction_Bytes Bytes;
  };
  Compiler_Source_Location compiler_source_location;
  Source_Range source_range;
  u8 encoded_byte_size;
} Instruction;

typedef dyn_array_type(Instruction) Array_Instruction;

typedef struct {
  Label_Index end_label;
  Array_Instruction instructions;
  u64 register_volatile_bitset;
  u64 register_occupied_bitset;
} Code_Block;

typedef enum {
  Function_Argument_Mode_Call,
  Function_Argument_Mode_Body,
} Function_Argument_Mode;

typedef struct {
  s32 stack_reserve;
  u8 size_of_prolog;
  u32 begin_rva;
  u32 end_rva;
  u8 stack_allocation_offset_in_prolog;
  u8 volatile_register_push_offsets[16];
} Function_Layout;
typedef dyn_array_type(Function_Layout) Array_Function_Layout;

typedef struct Function_Builder {
  bool frozen;
  s32 stack_reserve;
  u32 max_call_parameters_stack_size;
  Code_Block code_block;
  u64 used_register_bitset;
  Slice source;

  const Function_Info *function;
  Label_Index label_index;
} Function_Builder;
typedef dyn_array_type(Function_Builder) Array_Function_Builder;

MASS_DEFINE_OPAQUE_C_TYPE(function_builder, Function_Builder);

typedef struct {
  Virtual_Memory_Buffer buffer;
  union {
    struct {
      Section rw_data;
      Section code;
      Section ro_data;
    };
    Section list[3];
  } sections;
} Program_Memory;

typedef struct {
  Storage patch_at;
  Storage address_of;
} Relocation;
typedef dyn_array_type(Relocation) Array_Relocation;

typedef struct Program {
  Array_Import_Library import_libraries;
  Array_Label labels;
  Array_Label_Location_Diff_Patch_Info patch_info_array;
  Array_Value_Ptr startup_functions;
  Array_Relocation relocations;
  Value *entry_point;
  Array_Function_Builder functions;
  Program_Memory memory;
} Program;
MASS_DEFINE_OPAQUE_C_TYPE(program, Program);

hash_map_slice_template(Jit_Import_Library_Handle_Map, void *)
hash_map_slice_template(Imported_Module_Map, Module *)

typedef struct Jit {
  bool is_stack_unwinding_in_progress;
  Program *program;
  Jit_Import_Library_Handle_Map *import_library_handles;
  void *platform_specific_payload;
} Jit;

bool
pointer_equal(
  const void * const *a,
  const void * const *b
) {
  return *a == *b;
}
hash_map_template(Static_Pointer_Map, const void *, Value, hash_pointer, pointer_equal)

typedef struct Compilation {
  Virtual_Memory_Buffer allocation_buffer;
  Allocator *allocator;
  Jit jit;
  Module compiler_module;
  Static_Pointer_Map *static_pointer_map;
  Imported_Module_Map *module_map;
  Scope *root_scope;
  Program *runtime_program;
  Mass_Result *result;
} Compilation;

MASS_DEFINE_OPAQUE_C_TYPE(compilation, Compilation);

void *
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

fn_type_opaque
value_as_function(
  const Jit *jit,
  Value *value
);

static inline void *
rip_value_pointer_from_label_index(
  Program *program,
  Label_Index label_index
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
