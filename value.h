#ifndef VALUE_H
#define VALUE_H
#include "types.h"

typedef enum {
  Operand_Type_None,
  Operand_Type_Register,
  Operand_Type_Immediate_8,
  Operand_Type_Immediate_32,
  Operand_Type_Immediate_64,
  Operand_Type_Memory_Indirect,
} Operand_Type;

typedef struct {
  u8 index;
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

typedef enum {
  Descriptor_Type_Void,
  Descriptor_Type_Integer,
  Descriptor_Type_Pointer,
  Descriptor_Type_Fixed_Size_Array,
  Descriptor_Type_Function,
  Descriptor_Type_Struct,
} Descriptor_Type;

struct Value;

typedef struct {
  struct Value *argument_list;
  s64 argument_count;

  struct Value *returns;
} Descriptor_Function;

typedef struct {
  struct Descriptor *descriptor;
  s32 offset;
} Descriptor_Struct_Field;

typedef struct {
  Descriptor_Struct_Field *field_list;
  s64 field_count;
} Descriptor_Struct;

typedef struct {
  struct Descriptor *item;
  s64 length;
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
    struct Descriptor *pointer_to;
  };
} Descriptor;

typedef struct Value {
  Descriptor descriptor;
  Operand operand;
} Value;

u32
descriptor_byte_size(
  const Descriptor *descriptor
);

#endif VALUE_H
