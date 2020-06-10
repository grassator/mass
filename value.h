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
  Operand_Type type;
  u32 byte_size;
  union {
    Register reg;
    s8 imm8;
    s32 imm32;
    s64 imm64;
    Operand_Memory_Indirect indirect;
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

typedef struct {
  s32 *location;
  u64 ip;
} Patch_32;

typedef struct Jump_Patch_List {
  Patch_32 patch;
  struct Jump_Patch_List *next;
} Jump_Patch_List;

typedef struct {
  s32 *location;
  u32 byte_size;
} Stack_Patch;


typedef struct {
  s32 stack_reserve;
  u32 max_call_parameters_stack_size;
  u8 next_argument_index;
  Buffer *buffer;
  u8 *code;

  Jump_Patch_List *return_patch_list;

  #define MAX_DISPLACEMENT_COUNT 128
  Stack_Patch *stack_displacements;
  u32 stack_displacement_count;

  Descriptor *descriptor;

  Value **result;
} Function_Builder;

#endif VALUE_H
