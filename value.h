#ifndef VALUE_H
#define VALUE_H
#include "prelude.h"

typedef void(*fn_type_opaque)();

typedef enum {
  Operand_Type_None,
  Operand_Type_Any,
  Operand_Type_Eflags,
  Operand_Type_Register,
  Operand_Type_Xmm,
  Operand_Type_Immediate_8,
  Operand_Type_Immediate_16,
  Operand_Type_Immediate_32,
  Operand_Type_Immediate_64,
  Operand_Type_Memory_Indirect,
  Operand_Type_Sib,
  Operand_Type_RIP_Relative,
  Operand_Type_RIP_Relative_Import,
  Operand_Type_Label_32,
} Operand_Type;

typedef enum {
  Section_Permissions_Read    = 1 << 0,
  Section_Permissions_Write   = 1 << 1,
  Section_Permissions_Execute = 1 << 2,
} Section_Permissions;

typedef struct {
  Bucket_Buffer *buffer;
  Slice name;
  u32 base_rva;
  Section_Permissions permissions;
} Section;
typedef dyn_array_type(Section *) Array_Section_Ptr;

typedef enum {
  Register_A   = 0b0000,
  Register_C   = 0b0001,
  Register_D   = 0b0010,
  Register_B   = 0b0011,

  Register_SP  = 0b0100,
  Register_AH  = 0b0100,
  R_M_SIB      = 0b0100,

  Register_BP  = 0b0101,
  Register_CH  = 0b0100,

  Register_SI  = 0b0110,
  Register_DH  = 0b0100,

  Register_DI  = 0b0111,
  Register_BH  = 0b0100,

  Register_R8  = 0b1000,
  Register_R9  = 0b1001,
  Register_R10 = 0b1010,
  Register_R11 = 0b1011,
  Register_R12 = 0b1100,
  Register_R13 = 0b1101,
  Register_R14 = 0b1110,
  Register_R15 = 0b1111,

  Register_Xmm0 = 0b10000,
  Register_Xmm1 = 0b10001,
  Register_Xmm2 = 0b10010,
  Register_Xmm3 = 0b10011,
  Register_Xmm4 = 0b10100,
  Register_Xmm5 = 0b10101,
  Register_Xmm6 = 0b10110,
  Register_Xmm7 = 0b10111,

  Register_Xmm8  = 0b11000,
  Register_Xmm9  = 0b11001,
  Register_Xmm10 = 0b11010,
  Register_Xmm11 = 0b11011,
  Register_Xmm12 = 0b11100,
  Register_Xmm13 = 0b11101,
  Register_Xmm14 = 0b11110,
  Register_Xmm15 = 0b11111,
} Register;

static inline bool
register_is_xmm(
  Register reg
) {
  return !!(reg & Register_Xmm0);
}

typedef struct {
  Register reg;
  s32 displacement;
} Operand_Memory_Indirect;

typedef enum {
  SIB_Scale_1 = 0b00,
  SIB_Scale_2 = 0b01,
  SIB_Scale_4 = 0b10,
  SIB_Scale_8 = 0b11,
} SIB_Scale;

typedef struct {
  SIB_Scale scale;
  Register index;
  Register base;
  s32 displacement;
} Operand_Sib;

typedef struct {
  u64 value;
} Label_Index;

typedef struct {
  Section *section;
  u32 offset_in_section;
} Label;
typedef dyn_array_type(Label) Array_Label;

typedef struct {
  Label_Index target_label_index;
  Label from;
  s32 *patch_target;
} Label_Location_Diff_Patch_Info;
typedef dyn_array_type(Label_Location_Diff_Patch_Info) Array_Label_Location_Diff_Patch_Info;

typedef struct {
  Slice name;
  u32 name_rva;
  Label_Index label32;
} Import_Symbol;
typedef dyn_array_type(Import_Symbol) Array_Import_Symbol;

typedef struct {
  Slice name;
  u32 name_rva;
  u32 rva;
  Array_Import_Symbol symbols;
  u32 image_thunk_rva;
} Import_Library;
typedef dyn_array_type(Import_Library) Array_Import_Library;

typedef struct {
  Slice library_name;
  Slice symbol_name;
} Operand_RIP_Relative_Import;

typedef enum {
  Compare_Type_Equal = 1,
  Compare_Type_Not_Equal,

  Compare_Type_Unsigned_Below,
  Compare_Type_Unsigned_Below_Equal,
  Compare_Type_Unsigned_Above,
  Compare_Type_Unsigned_Above_Equal,

  Compare_Type_Signed_Less,
  Compare_Type_Signed_Less_Equal,
  Compare_Type_Signed_Greater,
  Compare_Type_Signed_Greater_Equal,
} Compare_Type;

typedef struct {
  Operand_Type type;
  u32 byte_size;
  union {
    Register reg;
    s8 s8;
    s16 s16;
    s32 s32;
    s64 s64;
    u8 u8;
    u16 u16;
    u32 u32;
    u64 u64;
    Label_Index label32;
    Operand_Memory_Indirect indirect;
    Operand_Sib sib;
    Operand_RIP_Relative_Import import;
    Compare_Type compare_type;
  };
} Operand;

const char *
operand_type_string(
  Operand_Type type
);

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

struct Descriptor;


typedef struct {
  const char *filename;
  const char *function_name;
  u32 line_number;
} Compiler_Source_Location;

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

typedef struct Value {
  struct Descriptor *descriptor;
  Operand operand;
  Compiler_Source_Location compiler_source_location;
} Value;

typedef dyn_array_type(Value) Array_Value;
typedef dyn_array_type(Value *) Array_Value_Ptr;

typedef enum {
  Descriptor_Type_Void,
  Descriptor_Type_Any,
  Descriptor_Type_Opaque,
  Descriptor_Type_Integer,
  Descriptor_Type_Float,
  Descriptor_Type_Pointer,
  Descriptor_Type_Fixed_Size_Array,
  Descriptor_Type_Function,
  Descriptor_Type_Struct,
  Descriptor_Type_Tagged_Union,
  Descriptor_Type_Type
} Descriptor_Type;

typedef struct Token Token;
typedef struct Scope Scope;

typedef struct Descriptor_Function {
  Array_Value_Ptr arguments;
  Array_Slice argument_names;
  Token *inline_body;
  Scope *parent_scope;

  Value *returns;
  Value *next_overload;
} Descriptor_Function;

typedef struct {
  Slice name;
  struct Descriptor *descriptor;
  s32 offset;
} Descriptor_Struct_Field;
typedef dyn_array_type(Descriptor_Struct_Field) Array_Descriptor_Struct_Field;

typedef struct {
  Slice name;
  Array_Descriptor_Struct_Field fields;
} Descriptor_Struct;

typedef struct {
  Descriptor_Struct *struct_list;
  s32 struct_count;
} Descriptor_Tagged_Union;

typedef struct {
  struct Descriptor *item;
  u32 length;
} Descriptor_Fixed_Size_Array;

typedef struct {
  u32 byte_size;
  bool is_signed;
} Descriptor_Integer;

typedef struct {
  u32 byte_size;
} Descriptor_Float;

typedef struct {
  u64 bit_size;
} Descriptor_Opaque;

typedef struct Descriptor {
  Descriptor_Type type;
  union {
    Descriptor_Integer integer;
    Descriptor_Float float_;
    Descriptor_Function function;
    Descriptor_Fixed_Size_Array array;
    Descriptor_Struct struct_;
    Descriptor_Tagged_Union tagged_union;
    struct Descriptor *pointer_to;
    struct Descriptor *type_descriptor;
    Descriptor_Opaque opaque;
  };
} Descriptor;


Descriptor descriptor_s8 = {
  .type = { Descriptor_Type_Integer },
  .integer = { .byte_size = 1, .is_signed = true },
};
Descriptor descriptor_s16 = {
  .type = { Descriptor_Type_Integer },
  .integer = { .byte_size = 2, .is_signed = true },
};
Descriptor descriptor_s32 = {
  .type = { Descriptor_Type_Integer },
  .integer = { .byte_size = 4, .is_signed = true },
};
Descriptor descriptor_s64 = {
  .type = { Descriptor_Type_Integer },
  .integer = { .byte_size = 8, .is_signed = true },
};


Descriptor descriptor_u8 = {
  .type = { Descriptor_Type_Integer },
  .integer = { .byte_size = 1, .is_signed = false },
};
Descriptor descriptor_u16 = {
  .type = { Descriptor_Type_Integer },
  .integer = { .byte_size = 2, .is_signed = false },
};
Descriptor descriptor_u32 = {
  .type = { Descriptor_Type_Integer },
  .integer = { .byte_size = 4, .is_signed = false },
};
Descriptor descriptor_u64 = {
  .type = { Descriptor_Type_Integer },
  .integer = { .byte_size = 8, .is_signed = false },
};


Descriptor descriptor_void = {
  .type = Descriptor_Type_Void,
};
Descriptor descriptor_any = {
  .type = Descriptor_Type_Any,
};
Descriptor descriptor_f32 = {
  .type = { Descriptor_Type_Float },
  .integer = { .byte_size = 4 },
};
Descriptor descriptor_f64 = {
  .type = { Descriptor_Type_Float },
  .integer = { .byte_size = 8 },
};

#define define_type_value(_TYPE_)\
  Value *type_##_TYPE_##_value = &(Value) {\
    .descriptor = &(Descriptor) {\
      .type = Descriptor_Type_Type,\
      .type_descriptor = &descriptor_##_TYPE_,\
    },\
    .operand = {.type = Operand_Type_None },\
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

Value void_value = {
  .descriptor = &descriptor_void,
  .operand = { .type = Operand_Type_None },
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

typedef enum {
  Instruction_Extension_Type_None,
  Instruction_Extension_Type_Register,
  Instruction_Extension_Type_Op_Code,
  Instruction_Extension_Type_Plus_Register,
} Instruction_Extension_Type;

typedef enum {
  Operand_Encoding_Type_None,
  Operand_Encoding_Type_Eflags,
  Operand_Encoding_Type_Register,
  Operand_Encoding_Type_Register_A,
  Operand_Encoding_Type_Register_Memory,
  Operand_Encoding_Type_Xmm,
  Operand_Encoding_Type_Xmm_Memory,
  Operand_Encoding_Type_Memory,
  Operand_Encoding_Type_Immediate,
} Operand_Encoding_Type;

typedef enum {
  Operand_Size_Any = 0,
  Operand_Size_8 = 1,
  Operand_Size_16 = 2,
  Operand_Size_32 = 4,
  Operand_Size_64 = 8,
} Operand_Size;

typedef struct {
  Operand_Encoding_Type type;
  Operand_Size size;
} Operand_Encoding;

typedef struct {
  u8 op_code[4];
  Instruction_Extension_Type extension_type;
  u8 op_code_extension;
  Operand_Encoding operands[3];
} Instruction_Encoding;

typedef struct {
  const char *name;
  const Instruction_Encoding *encoding_list;
  u32 encoding_count;
} X64_Mnemonic;

typedef dyn_array_type(Range_u64) Array_Range_u64;
typedef struct {
  Slice path;
  Slice text;
  Array_Range_u64 lines;
} Source_File;

typedef struct {
  u64 line;
  u64 column;
} Source_Position;

typedef struct {
  const Source_File *file;
  Range_u64 offsets;
} Source_Range;

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

typedef struct {
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

typedef struct {
  Slice message;
  Source_Range source_range;
} Parse_Error;
typedef dyn_array_type(Parse_Error) Array_Parse_Error;

typedef enum {
  Tokenizer_Result_Type_Error,
  Tokenizer_Result_Type_Success,
} Tokenizer_Result_Type;

typedef struct Token Token;

typedef struct {
  Tokenizer_Result_Type type;
  union {
    Token *root;
    Array_Parse_Error errors;
  };
} Tokenizer_Result;

typedef enum {
  Parse_Result_Type_Success,
  Parse_Result_Type_Error,
} Parse_Result_Type;

typedef struct {
  Parse_Result_Type type;
  Array_Parse_Error errors;
} Parse_Result;

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
