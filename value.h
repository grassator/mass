#ifndef VALUE_H
#define VALUE_H
#include "prelude.h"
#include "types.h"
#include "encoding.h"

typedef void(*fn_type_opaque)();

static inline bool
register_is_xmm(
  Register reg
) {
  return !!(reg & Register_Xmm0);
}

static inline bool
operand_equal(
  const Operand *a,
  const Operand *b
);

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

#define COMPILER_SOURCE_LOCATION_GLOBAL_FIELDS\
  {\
    .filename = __FILE__,\
    .function_name = "GLOBAL",\
    .line_number = __LINE__,\
  }

#define COMPILER_SOURCE_LOCATION_FIELDS\
  {\
    .filename = __FILE__,\
    .function_name = __func__,\
    .line_number = __LINE__,\
  }
#define COMPILER_SOURCE_LOCATION\
  ((Compiler_Source_Location)COMPILER_SOURCE_LOCATION_FIELDS)

typedef struct Scope Scope;
typedef struct Function_Builder Function_Builder;

Descriptor descriptor_s8 = {
  .tag = { Descriptor_Tag_Opaque },
  .Opaque = { .bit_size = 8 },
};
Descriptor descriptor_s16 = {
  .tag = { Descriptor_Tag_Opaque },
  .Opaque = { .bit_size = 16 },
};
Descriptor descriptor_s32 = {
  .tag = { Descriptor_Tag_Opaque },
  .Opaque = { .bit_size = 32 },
};
Descriptor descriptor_s64 = {
  .tag = { Descriptor_Tag_Opaque },
  .Opaque = { .bit_size = 64 },
};


Descriptor descriptor_u8 = {
  .tag = { Descriptor_Tag_Opaque },
  .Opaque = { .bit_size = 8 },
};
Descriptor descriptor_u16 = {
  .tag = { Descriptor_Tag_Opaque },
  .Opaque = { .bit_size = 16 },
};
Descriptor descriptor_u32 = {
  .tag = { Descriptor_Tag_Opaque },
  .Opaque = { .bit_size = 32 },
};
Descriptor descriptor_u64 = {
  .tag = { Descriptor_Tag_Opaque },
  .Opaque = { .bit_size = 64 },
};


Descriptor descriptor_void = {
  .tag = Descriptor_Tag_Void,
};
Descriptor descriptor_any = {
  .tag = Descriptor_Tag_Any,
};
Descriptor descriptor_f32 = {
  .tag = { Descriptor_Tag_Opaque },
  .Opaque = { .bit_size = 32 },
};
Descriptor descriptor_f64 = {
  .tag = { Descriptor_Tag_Opaque },
  .Opaque = { .bit_size = 64 },
};

#define define_type_value(_TYPE_)\
  Value *type_##_TYPE_##_value = &(Value) {\
    .descriptor = &(Descriptor) {\
      .tag = Descriptor_Tag_Type,\
      .Type = {.descriptor = &descriptor_##_TYPE_ },\
    },\
    .operand = {.tag = Operand_Tag_None },\
    .compiler_source_location = COMPILER_SOURCE_LOCATION_GLOBAL_FIELDS,\
  }

define_type_value(s64);
define_type_value(s32);
define_type_value(s16);
define_type_value(s8);

define_type_value(u64);
define_type_value(u32);
define_type_value(u16);
define_type_value(u8);

define_type_value(f64);
define_type_value(f32);

static inline bool
descriptor_is_unsigned_integer(
  Descriptor *descriptor
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
  Descriptor *descriptor
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
  Descriptor *descriptor
) {
  return descriptor_is_signed_integer(descriptor) || descriptor_is_unsigned_integer(descriptor);
}

bool
descriptor_is_float(
  Descriptor *descriptor
) {
  return descriptor == &descriptor_f32 || descriptor == &descriptor_f64;
}

Value void_value = {
  .descriptor = &descriptor_void,
  .operand = { .tag = Operand_Tag_None },
  .compiler_source_location = COMPILER_SOURCE_LOCATION_GLOBAL_FIELDS,
};

u32
descriptor_byte_size(
  const Descriptor *descriptor
);

bool
same_type(
  Descriptor *a,
  Descriptor *b
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

typedef enum {
  Instruction_Type_Assembly,
  Instruction_Type_Label,
  Instruction_Type_Bytes,
} Instruction_Type;

typedef struct {
  const X64_Mnemonic *mnemonic;
  Operand operands[3];
} Instruction_Assembly;

typedef struct {
  Instruction_Type type;
  union {
    Instruction_Assembly assembly;
    Label_Index label;
    Slice bytes;
  };
  Compiler_Source_Location compiler_source_location;
  const Source_Range *source_range;
  u8 encoded_byte_size;
} Instruction;

typedef dyn_array_type(Instruction) Array_Instruction;

typedef struct _Program Program;

typedef struct {
  Label_Index end_label;
  Array_Instruction instructions;
  u64 register_volatile_bitset;
  u64 register_occupied_bitset;
} Code_Block;

typedef struct Function_Builder {
  s32 stack_reserve;
  u32 max_call_parameters_stack_size;
  Code_Block code_block;
  Descriptor *descriptor;
  u64 used_register_bitset;

  Value *value;
} Function_Builder;
typedef dyn_array_type(Function_Builder) Array_Function_Builder;

// https://docs.microsoft.com/en-us/cpp/build/exception-handling-x64?view=vs-2019#unwind-data-definitions-in-c
typedef enum _UNWIND_OP_CODES {
  UWOP_PUSH_NONVOL = 0, /* info == register number */
  UWOP_ALLOC_LARGE,     /* no info, alloc size in next 2 slots */
  UWOP_ALLOC_SMALL,     /* info == size of allocation / 8 - 1 */
  UWOP_SET_FPREG,       /* no info, FP = RSP + UNWIND_INFO.FPRegOffset*16 */
  UWOP_SAVE_NONVOL,     /* info == register number, offset in next slot */
  UWOP_SAVE_NONVOL_FAR, /* info == register number, offset in next 2 slots */
  UWOP_SAVE_XMM128 = 8, /* info == XMM reg number, offset in next slot */
  UWOP_SAVE_XMM128_FAR, /* info == XMM reg number, offset in next 2 slots */
  UWOP_PUSH_MACHFRAME   /* info == 0: no error-code, 1: error-code */
} UNWIND_CODE_OPS;

typedef union {
  struct {
    u8 CodeOffset;
    u8 UnwindOp : 4;
    u8 OpInfo   : 4;
  };
  u16 FrameOffset;
  u16 DataForPreviousCode;
} UNWIND_CODE;

typedef dyn_array_type(UNWIND_CODE) Array_UNWIND_CODE;

typedef struct {
  const Function_Builder *builder;
  Program *program;
} Exception_Data;

#define UNWIND_INFO_MAX_COUNT_OF_CODES_FOR_STACK 2
#define UNWIND_INFO_MAX_COUNT_OF_CODES_FOR_NON_VOLATILE_REGISTER_PUSH 16
#define UNWIND_INFO_EXCEPTION_HANDLER_SIZE_IN_UNWIND_CODES (sizeof(u32) / sizeof(UNWIND_CODE))
#define UNWIND_INFO_EXCEPTION_DATA_SIZE_IN_INWIND_CODES  (sizeof(Exception_Data) / sizeof(UNWIND_CODE))

typedef struct {
  u8 Version       : 3;
  u8 Flags         : 5;
  u8 SizeOfProlog;
  u8 CountOfCodes;
  u8 FrameRegister : 4;
  u8 FrameOffset   : 4;
  // FIXME actually turn this into a variadic struct with getter functions
  // :RegisterAllocation need to add more reserved space for UnwindCode
  UNWIND_CODE UnwindCode[
    UNWIND_INFO_MAX_COUNT_OF_CODES_FOR_STACK +
    UNWIND_INFO_MAX_COUNT_OF_CODES_FOR_NON_VOLATILE_REGISTER_PUSH +
    UNWIND_INFO_EXCEPTION_HANDLER_SIZE_IN_UNWIND_CODES +
    UNWIND_INFO_EXCEPTION_DATA_SIZE_IN_INWIND_CODES
  ];
} UNWIND_INFO;

typedef struct Scope Scope;

typedef struct _Program {
  Fixed_Buffer *jit_buffer;
  Array_Import_Library import_libraries;
  Array_Label labels;
  Array_Label_Location_Diff_Patch_Info patch_info_array;
  Value *entry_point;
  Array_Function_Builder functions;
  Section data_section;
  Section code_section;
  Scope *global_scope;
  Array_Parse_Error errors;
  bool is_stack_unwinding_in_progress;
} Program;

typedef struct {
  Bucket_Buffer *allocation_buffer;
  Allocator *allocator;
  Program *program;
} Compilation_Context;

void *
rip_value_pointer(
  Program *program,
  Value *value
);

u64
estimate_max_code_size_in_bytes(
  Program *program
);

void
program_jit(
  Compilation_Context *context
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

#endif VALUE_H
