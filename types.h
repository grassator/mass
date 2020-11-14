// Forward declarations

typedef enum Operand_Type Operand_Type;
typedef struct Parse_Error Parse_Error;
typedef struct Parse_Result Parse_Result;

// Type Definitions

typedef enum Operand_Type {
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

typedef struct Parse_Error {
  Slice message;
  Source_Location location;
} Parse_Error;
typedef dyn_array_type(Parse_Error) Array_Parse_Error;
typedef dyn_array_type(Parse_Error *) Array_Parse_Error_Ptr;

typedef enum {
  Parse_Result_Tag_Success = 0,
  Parse_Result_Tag_Error = 1,
} Parse_Result_Tag;

typedef struct {
  list Array_Parse_Error;
} Parse_Result_Error;
typedef struct Parse_Result {
  Parse_Result_Tag tag;
  Parse_Result_Error Error;
} Parse_Result;
typedef dyn_array_type(Parse_Result) Array_Parse_Result;
typedef dyn_array_type(Parse_Result *) Array_Parse_Result_Ptr;

