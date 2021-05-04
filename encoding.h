#ifndef ENCODING_H
#define ENCODING_H

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

typedef struct X64_Mnemonic {
  const char *name;
  const Instruction_Encoding *encoding_list;
  u32 encoding_count;
} X64_Mnemonic;

#endif