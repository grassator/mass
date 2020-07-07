#ifndef VALUE_H
#define VALUE_H
#include "prelude.h"

typedef enum {
  Operand_Type_None,
  Operand_Type_Register,
  Operand_Type_Immediate_8,
  Operand_Type_Immediate_32,
  Operand_Type_Immediate_64,
  Operand_Type_Memory_Indirect,
  Operand_Type_RIP_Relative,
  Operand_Type_RIP_Relative_Import,
  Operand_Type_Label_32,
} Operand_Type;

typedef enum {
  Register_A   = 0b0000,
  Register_C   = 0b0001,
  Register_D   = 0b0010,
  Register_B   = 0b0011,
  Register_SP  = 0b0100,
  Register_BP  = 0b0101,
  Register_SI  = 0b0110,
  Register_DI  = 0b0111,

  Register_R8  = 0b1000,
  Register_R9  = 0b1001,
  Register_R10 = 0b1010,
  Register_R11 = 0b1011,
  Register_R12 = 0b1100,
  Register_R13 = 0b1101,
  Register_R14 = 0b1110,
  Register_R15 = 0b1111,
} Register;

typedef struct {
  Register reg;
  s32 displacement;
} Operand_Memory_Indirect;

typedef struct {
  s32 *patch_target;
  u8 *from_offset;
} Label_Location;
typedef array_type(Label_Location) Array_Label_Location;

typedef struct {
  u8 *target;
  Array_Label_Location locations;
} Label;

typedef struct {
  const char *name;
  u32 name_rva;
  u32 iat_rva;
} Import_Name_To_Rva;
typedef array_type(Import_Name_To_Rva) Array_Import_Name_To_Rva;

typedef struct {
  Import_Name_To_Rva dll;
  Array_Import_Name_To_Rva symbols;
  u32 image_thunk_rva;
} Import_Library;
typedef array_type(Import_Library) Array_Import_Library;

typedef struct {
  const char *library_name;
  const char *symbol_name;
} Operand_RIP_Relative_Import;

typedef struct {
  Operand_Type type;
  u32 byte_size;
  union {
    Register reg;
    s8 imm8;
    s32 imm32;
    s64 imm64;
    Label *label32;
    Operand_Memory_Indirect indirect;
    Operand_RIP_Relative_Import import;
  };
} Operand;

const char *
operand_type_string(
  Operand_Type type
);

typedef enum {
  Descriptor_Type_Void,
  Descriptor_Type_Integer,
  Descriptor_Type_Pointer,
  Descriptor_Type_Fixed_Size_Array,
  Descriptor_Type_Function,
  Descriptor_Type_Struct,
  Descriptor_Type_Tagged_Union,
} Descriptor_Type;

struct Value;

typedef struct Descriptor_Function {
  struct Value *argument_list;
  s64 argument_count;

  struct Value *returns;
  bool frozen;
  struct Value *next_overload;
} Descriptor_Function;

typedef struct {
  const char *name;
  struct Descriptor *descriptor;
  s32 offset;
} Descriptor_Struct_Field;

typedef struct {
  const char *name;
  Descriptor_Struct_Field *field_list;
  s32 field_count;
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
} Descriptor_Integer;

typedef struct Descriptor {
  Descriptor_Type type;
  union {
    Descriptor_Integer integer;
    Descriptor_Function function;
    Descriptor_Fixed_Size_Array array;
    Descriptor_Struct struct_;
    Descriptor_Tagged_Union tagged_union;
    struct Descriptor *pointer_to;
  };
} Descriptor;

typedef struct Value {
  Descriptor *descriptor;
  Operand operand;
} Value;


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
  Operand_Encoding_Type_Register,
  Operand_Encoding_Type_Register_Memory,
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
  u8 op_code[2];
  Instruction_Extension_Type extension_type;
  u8 op_code_extension;
  Operand_Encoding operands[3];
} Instruction_Encoding;

typedef struct {
  const char *name;
  const Instruction_Encoding *encoding_list;
  u32 encoding_count;
} X64_Mnemonic;

typedef struct {
  X64_Mnemonic mnemonic;
  Operand operands[3];
  Label *maybe_label;
  const char *filename;
  u32 line_number;
} Instruction;
typedef array_type(Instruction) Array_Instruction;

typedef struct _Program Program;

typedef struct {
  s32 stack_reserve;
  u32 max_call_parameters_stack_size;
  u8 next_argument_index;

  Label *prolog_label;
  Label *epilog_label;

  Array_Instruction instructions;

  Descriptor *descriptor;
  Program *program;

  Value **result;
} Function_Builder;
typedef array_type(Function_Builder) Array_Function_Builder;

typedef struct _Program {
  Buffer data_buffer;
  Array_Import_Library import_libraries;
  Function_Builder *entry_point;
  Array_Function_Builder functions;
  s32 code_base_rva;
  s32 code_base_file_offset;
} Program;

typedef struct {
  Buffer code_buffer;
  Buffer data_buffer;
} Jit_Program;

#endif VALUE_H
