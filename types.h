typedef enum {
  Operand_Type_None = 0,
  Operand_Type_Any = 1,
  Operand_Type_Eflags = 2,
  Operand_Type_Register = 3,
  Operand_Type_Xmm = 4,
  Operand_Type_Immediate_8 = 5,
  Operand_Type_Immediate_16 = 6,
  Operand_Type_Immediate_32 = 7,
  Operand_Type_Immediate_64 = 8,
  Operand_Type_Memory_Indirect = 9,
  Operand_Type_Sib = 10,
  Operand_Type_RIP_Relative = 11,
  Operand_Type_RIP_Relative_Import = 12,
  Operand_Type_Label_32 = 13,
} Operand_Type;

typedef struct {
  bool resolved;
  u32 target_rva;
} Label;

