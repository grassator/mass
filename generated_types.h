#ifndef GENERATED_TYPES_H
#define GENERATED_TYPES_H
#include "prelude.h"
_Pragma("warning (push)") _Pragma("warning (default: 4820)")
typedef void(*fn_type_opaque)();

typedef struct { u8 bits; } i8;

typedef struct { u16 bits; } i16;

typedef struct { u32 bits; } i32;

typedef struct { u64 bits; } i64;

// Forward declarations

typedef struct Bits Bits;
typedef dyn_array_type(Bits *) Array_Bits_Ptr;
typedef dyn_array_type(const Bits *) Array_Const_Bits_Ptr;

typedef struct Source_Position Source_Position;
typedef dyn_array_type(Source_Position *) Array_Source_Position_Ptr;
typedef dyn_array_type(const Source_Position *) Array_Const_Source_Position_Ptr;

typedef struct Source_File Source_File;
typedef dyn_array_type(Source_File *) Array_Source_File_Ptr;
typedef dyn_array_type(const Source_File *) Array_Const_Source_File_Ptr;

typedef struct Source_Range Source_Range;
typedef dyn_array_type(Source_Range *) Array_Source_Range_Ptr;
typedef dyn_array_type(const Source_Range *) Array_Const_Source_Range_Ptr;

typedef struct Mass_While Mass_While;
typedef dyn_array_type(Mass_While *) Array_Mass_While_Ptr;
typedef dyn_array_type(const Mass_While *) Array_Const_Mass_While_Ptr;

typedef struct Assignment Assignment;
typedef dyn_array_type(Assignment *) Array_Assignment_Ptr;
typedef dyn_array_type(const Assignment *) Array_Const_Assignment_Ptr;

typedef struct Module_Exports Module_Exports;
typedef struct Module_Exports_Selective Module_Exports_Selective;
typedef dyn_array_type(Module_Exports *) Array_Module_Exports_Ptr;
typedef dyn_array_type(const Module_Exports *) Array_Const_Module_Exports_Ptr;

typedef struct Module Module;
typedef dyn_array_type(Module *) Array_Module_Ptr;
typedef dyn_array_type(const Module *) Array_Const_Module_Ptr;

typedef struct Parse_Error Parse_Error;
typedef dyn_array_type(Parse_Error *) Array_Parse_Error_Ptr;
typedef dyn_array_type(const Parse_Error *) Array_Const_Parse_Error_Ptr;

typedef struct Value_View Value_View;
typedef dyn_array_type(Value_View *) Array_Value_View_Ptr;
typedef dyn_array_type(const Value_View *) Array_Const_Value_View_Ptr;

typedef struct Symbol Symbol;
typedef dyn_array_type(Symbol *) Array_Symbol_Ptr;
typedef dyn_array_type(const Symbol *) Array_Const_Symbol_Ptr;

typedef struct Group_Paren Group_Paren;
typedef dyn_array_type(Group_Paren *) Array_Group_Paren_Ptr;
typedef dyn_array_type(const Group_Paren *) Array_Const_Group_Paren_Ptr;

typedef struct Ast_Return Ast_Return;
typedef dyn_array_type(Ast_Return *) Array_Ast_Return_Ptr;
typedef dyn_array_type(const Ast_Return *) Array_Const_Ast_Return_Ptr;

typedef struct Ast_Statement Ast_Statement;
typedef dyn_array_type(Ast_Statement *) Array_Ast_Statement_Ptr;
typedef dyn_array_type(const Ast_Statement *) Array_Const_Ast_Statement_Ptr;

typedef struct Ast_Block Ast_Block;
typedef dyn_array_type(Ast_Block *) Array_Ast_Block_Ptr;
typedef dyn_array_type(const Ast_Block *) Array_Const_Ast_Block_Ptr;

typedef struct Group_Square Group_Square;
typedef dyn_array_type(Group_Square *) Array_Group_Square_Ptr;
typedef dyn_array_type(const Group_Square *) Array_Const_Group_Square_Ptr;

typedef struct Ast_Using Ast_Using;
typedef dyn_array_type(Ast_Using *) Array_Ast_Using_Ptr;
typedef dyn_array_type(const Ast_Using *) Array_Const_Ast_Using_Ptr;

typedef enum Section_Permissions {
  Section_Permissions_Read = 1,
  Section_Permissions_Write = 2,
  Section_Permissions_Execute = 4,
} Section_Permissions;

const char *section_permissions_name(Section_Permissions value) {
  if (value == 1) return "Section_Permissions_Read";
  if (value == 2) return "Section_Permissions_Write";
  if (value == 4) return "Section_Permissions_Execute";
  assert(!"Unexpected value for enum Section_Permissions");
  return 0;
};

typedef dyn_array_type(Section_Permissions *) Array_Section_Permissions_Ptr;
typedef dyn_array_type(const Section_Permissions *) Array_Const_Section_Permissions_Ptr;

typedef struct Section Section;
typedef dyn_array_type(Section *) Array_Section_Ptr;
typedef dyn_array_type(const Section *) Array_Const_Section_Ptr;

typedef struct Program_Memory Program_Memory;
typedef dyn_array_type(Program_Memory *) Array_Program_Memory_Ptr;
typedef dyn_array_type(const Program_Memory *) Array_Const_Program_Memory_Ptr;

typedef enum Register {
  Register_A = 0,
  Register_C = 1,
  Register_D = 2,
  Register_B = 3,
  Register_SP = 4,
  Register_BP = 5,
  Register_SI = 6,
  Register_DI = 7,
  Register_R8 = 8,
  Register_R9 = 9,
  Register_R10 = 10,
  Register_R11 = 11,
  Register_R12 = 12,
  Register_R13 = 13,
  Register_R14 = 14,
  Register_R15 = 15,
  Register_Xmm0 = 16,
  Register_Xmm1 = 17,
  Register_Xmm2 = 18,
  Register_Xmm3 = 19,
  Register_Xmm4 = 20,
  Register_Xmm5 = 21,
  Register_Xmm6 = 22,
  Register_Xmm7 = 23,
  Register_Xmm8 = 24,
  Register_Xmm9 = 25,
  Register_Xmm10 = 26,
  Register_Xmm11 = 27,
  Register_Xmm12 = 28,
  Register_Xmm13 = 29,
  Register_Xmm14 = 30,
  Register_Xmm15 = 31,
} Register;

const char *register_name(Register value) {
  if (value == 0) return "Register_A";
  if (value == 1) return "Register_C";
  if (value == 2) return "Register_D";
  if (value == 3) return "Register_B";
  if (value == 4) return "Register_SP";
  if (value == 5) return "Register_BP";
  if (value == 6) return "Register_SI";
  if (value == 7) return "Register_DI";
  if (value == 8) return "Register_R8";
  if (value == 9) return "Register_R9";
  if (value == 10) return "Register_R10";
  if (value == 11) return "Register_R11";
  if (value == 12) return "Register_R12";
  if (value == 13) return "Register_R13";
  if (value == 14) return "Register_R14";
  if (value == 15) return "Register_R15";
  if (value == 16) return "Register_Xmm0";
  if (value == 17) return "Register_Xmm1";
  if (value == 18) return "Register_Xmm2";
  if (value == 19) return "Register_Xmm3";
  if (value == 20) return "Register_Xmm4";
  if (value == 21) return "Register_Xmm5";
  if (value == 22) return "Register_Xmm6";
  if (value == 23) return "Register_Xmm7";
  if (value == 24) return "Register_Xmm8";
  if (value == 25) return "Register_Xmm9";
  if (value == 26) return "Register_Xmm10";
  if (value == 27) return "Register_Xmm11";
  if (value == 28) return "Register_Xmm12";
  if (value == 29) return "Register_Xmm13";
  if (value == 30) return "Register_Xmm14";
  if (value == 31) return "Register_Xmm15";
  assert(!"Unexpected value for enum Register");
  return 0;
};

typedef dyn_array_type(Register *) Array_Register_Ptr;
typedef dyn_array_type(const Register *) Array_Const_Register_Ptr;

typedef struct Label Label;
typedef dyn_array_type(Label *) Array_Label_Ptr;
typedef dyn_array_type(const Label *) Array_Const_Label_Ptr;

typedef struct Label_Location_Diff_Patch_Info Label_Location_Diff_Patch_Info;
typedef dyn_array_type(Label_Location_Diff_Patch_Info *) Array_Label_Location_Diff_Patch_Info_Ptr;
typedef dyn_array_type(const Label_Location_Diff_Patch_Info *) Array_Const_Label_Location_Diff_Patch_Info_Ptr;

typedef enum Number_Base {
  Number_Base_2 = 2,
  Number_Base_10 = 10,
  Number_Base_16 = 16,
} Number_Base;

const char *number_base_name(Number_Base value) {
  if (value == 2) return "Number_Base_2";
  if (value == 10) return "Number_Base_10";
  if (value == 16) return "Number_Base_16";
  assert(!"Unexpected value for enum Number_Base");
  return 0;
};

typedef dyn_array_type(Number_Base *) Array_Number_Base_Ptr;
typedef dyn_array_type(const Number_Base *) Array_Const_Number_Base_Ptr;

typedef struct Quoted Quoted;
typedef dyn_array_type(Quoted *) Array_Quoted_Ptr;
typedef dyn_array_type(const Quoted *) Array_Const_Quoted_Ptr;

typedef struct Spread Spread;
typedef dyn_array_type(Spread *) Array_Spread_Ptr;
typedef dyn_array_type(const Spread *) Array_Const_Spread_Ptr;

typedef struct Named_Accessor Named_Accessor;
typedef dyn_array_type(Named_Accessor *) Array_Named_Accessor_Ptr;
typedef dyn_array_type(const Named_Accessor *) Array_Const_Named_Accessor_Ptr;

typedef struct External_Symbol External_Symbol;
typedef dyn_array_type(External_Symbol *) Array_External_Symbol_Ptr;
typedef dyn_array_type(const External_Symbol *) Array_Const_External_Symbol_Ptr;

typedef struct Import_Symbol Import_Symbol;
typedef dyn_array_type(Import_Symbol *) Array_Import_Symbol_Ptr;
typedef dyn_array_type(const Import_Symbol *) Array_Const_Import_Symbol_Ptr;

typedef struct Import_Library Import_Library;
typedef dyn_array_type(Import_Library *) Array_Import_Library_Ptr;
typedef dyn_array_type(const Import_Library *) Array_Const_Import_Library_Ptr;

typedef enum Compare_Type {
  Compare_Type_Equal = 1,
  Compare_Type_Not_Equal = 2,
  Compare_Type_Unsigned_Below = 3,
  Compare_Type_Unsigned_Below_Equal = 4,
  Compare_Type_Unsigned_Above = 5,
  Compare_Type_Unsigned_Above_Equal = 6,
  Compare_Type_Signed_Less = 7,
  Compare_Type_Signed_Less_Equal = 8,
  Compare_Type_Signed_Greater = 9,
  Compare_Type_Signed_Greater_Equal = 10,
} Compare_Type;

const char *compare_type_name(Compare_Type value) {
  if (value == 1) return "Compare_Type_Equal";
  if (value == 2) return "Compare_Type_Not_Equal";
  if (value == 3) return "Compare_Type_Unsigned_Below";
  if (value == 4) return "Compare_Type_Unsigned_Below_Equal";
  if (value == 5) return "Compare_Type_Unsigned_Above";
  if (value == 6) return "Compare_Type_Unsigned_Above_Equal";
  if (value == 7) return "Compare_Type_Signed_Less";
  if (value == 8) return "Compare_Type_Signed_Less_Equal";
  if (value == 9) return "Compare_Type_Signed_Greater";
  if (value == 10) return "Compare_Type_Signed_Greater_Equal";
  assert(!"Unexpected value for enum Compare_Type");
  return 0;
};

typedef dyn_array_type(Compare_Type *) Array_Compare_Type_Ptr;
typedef dyn_array_type(const Compare_Type *) Array_Const_Compare_Type_Ptr;

typedef enum Stack_Area {
  Stack_Area_Local = 0,
  Stack_Area_Received_Argument = 1,
  Stack_Area_Call_Target_Argument = 2,
} Stack_Area;

const char *stack_area_name(Stack_Area value) {
  if (value == 0) return "Stack_Area_Local";
  if (value == 1) return "Stack_Area_Received_Argument";
  if (value == 2) return "Stack_Area_Call_Target_Argument";
  assert(!"Unexpected value for enum Stack_Area");
  return 0;
};

typedef dyn_array_type(Stack_Area *) Array_Stack_Area_Ptr;
typedef dyn_array_type(const Stack_Area *) Array_Const_Stack_Area_Ptr;

typedef struct Memory_Location Memory_Location;
typedef struct Memory_Location_Instruction_Pointer_Relative Memory_Location_Instruction_Pointer_Relative;
typedef struct Memory_Location_Indirect Memory_Location_Indirect;
typedef struct Memory_Location_Stack Memory_Location_Stack;
typedef dyn_array_type(Memory_Location *) Array_Memory_Location_Ptr;
typedef dyn_array_type(const Memory_Location *) Array_Const_Memory_Location_Ptr;

typedef enum Storage_Flags {
  Storage_Flags_None = 0,
  Storage_Flags_Temporary = 1,
} Storage_Flags;

const char *storage_flags_name(Storage_Flags value) {
  if (value == 0) return "Storage_Flags_None";
  if (value == 1) return "Storage_Flags_Temporary";
  assert(!"Unexpected value for enum Storage_Flags");
  return 0;
};

typedef dyn_array_type(Storage_Flags *) Array_Storage_Flags_Ptr;
typedef dyn_array_type(const Storage_Flags *) Array_Const_Storage_Flags_Ptr;

typedef struct Storage Storage;
typedef struct Storage_Immediate Storage_Immediate;
typedef struct Storage_Eflags Storage_Eflags;
typedef struct Storage_Register Storage_Register;
typedef struct Storage_Xmm Storage_Xmm;
typedef struct Storage_Static Storage_Static;
typedef struct Storage_Memory Storage_Memory;
typedef struct Storage_Disjoint Storage_Disjoint;
typedef dyn_array_type(Storage *) Array_Storage_Ptr;
typedef dyn_array_type(const Storage *) Array_Const_Storage_Ptr;

typedef struct Relocation Relocation;
typedef dyn_array_type(Relocation *) Array_Relocation_Ptr;
typedef dyn_array_type(const Relocation *) Array_Const_Relocation_Ptr;

typedef struct Instruction_Assembly Instruction_Assembly;
typedef dyn_array_type(Instruction_Assembly *) Array_Instruction_Assembly_Ptr;
typedef dyn_array_type(const Instruction_Assembly *) Array_Const_Instruction_Assembly_Ptr;

typedef struct Instruction Instruction;
typedef struct Instruction_Label Instruction_Label;
typedef struct Instruction_Bytes Instruction_Bytes;
typedef struct Instruction_Label_Patch Instruction_Label_Patch;
typedef struct Instruction_Stack_Patch Instruction_Stack_Patch;
typedef struct Instruction_Location Instruction_Location;
typedef dyn_array_type(Instruction *) Array_Instruction_Ptr;
typedef dyn_array_type(const Instruction *) Array_Const_Instruction_Ptr;

typedef struct Instruction_Bucket Instruction_Bucket;
typedef dyn_array_type(Instruction_Bucket *) Array_Instruction_Bucket_Ptr;
typedef dyn_array_type(const Instruction_Bucket *) Array_Const_Instruction_Bucket_Ptr;

typedef struct Code_Block Code_Block;
typedef dyn_array_type(Code_Block *) Array_Code_Block_Ptr;
typedef dyn_array_type(const Code_Block *) Array_Const_Code_Block_Ptr;

typedef struct Epoch Epoch;
typedef dyn_array_type(Epoch *) Array_Epoch_Ptr;
typedef dyn_array_type(const Epoch *) Array_Const_Epoch_Ptr;

typedef struct Function_Layout Function_Layout;
typedef dyn_array_type(Function_Layout *) Array_Function_Layout_Ptr;
typedef dyn_array_type(const Function_Layout *) Array_Const_Function_Layout_Ptr;

typedef struct Mass_Context Mass_Context;
typedef dyn_array_type(Mass_Context *) Array_Mass_Context_Ptr;
typedef dyn_array_type(const Mass_Context *) Array_Const_Mass_Context_Ptr;

typedef enum Parser_Flags {
  Parser_Flags_None = 0,
  Parser_Flags_Global = 1,
  Parser_Flags_Type_Only = 2,
} Parser_Flags;

const char *parser_flags_name(Parser_Flags value) {
  if (value == 0) return "Parser_Flags_None";
  if (value == 1) return "Parser_Flags_Global";
  if (value == 2) return "Parser_Flags_Type_Only";
  assert(!"Unexpected value for enum Parser_Flags");
  return 0;
};

typedef dyn_array_type(Parser_Flags *) Array_Parser_Flags_Ptr;
typedef dyn_array_type(const Parser_Flags *) Array_Const_Parser_Flags_Ptr;

typedef struct Parser Parser;
typedef dyn_array_type(Parser *) Array_Parser_Ptr;
typedef dyn_array_type(const Parser *) Array_Const_Parser_Ptr;

typedef enum Operator_Fixity {
  Operator_Fixity_Infix = 1,
  Operator_Fixity_Prefix = 2,
  Operator_Fixity_Postfix = 4,
} Operator_Fixity;

const char *operator_fixity_name(Operator_Fixity value) {
  if (value == 1) return "Operator_Fixity_Infix";
  if (value == 2) return "Operator_Fixity_Prefix";
  if (value == 4) return "Operator_Fixity_Postfix";
  assert(!"Unexpected value for enum Operator_Fixity");
  return 0;
};

typedef dyn_array_type(Operator_Fixity *) Array_Operator_Fixity_Ptr;
typedef dyn_array_type(const Operator_Fixity *) Array_Const_Operator_Fixity_Ptr;

typedef enum Operator_Associativity {
  Operator_Associativity_Left = 0,
  Operator_Associativity_Right = 1,
} Operator_Associativity;

const char *operator_associativity_name(Operator_Associativity value) {
  if (value == 0) return "Operator_Associativity_Left";
  if (value == 1) return "Operator_Associativity_Right";
  assert(!"Unexpected value for enum Operator_Associativity");
  return 0;
};

typedef dyn_array_type(Operator_Associativity *) Array_Operator_Associativity_Ptr;
typedef dyn_array_type(const Operator_Associativity *) Array_Const_Operator_Associativity_Ptr;

typedef enum Operator_Flags {
  Operator_Flags_None = 0,
  Operator_Flags_Optional_Rhs = 1,
} Operator_Flags;

const char *operator_flags_name(Operator_Flags value) {
  if (value == 0) return "Operator_Flags_None";
  if (value == 1) return "Operator_Flags_Optional_Rhs";
  assert(!"Unexpected value for enum Operator_Flags");
  return 0;
};

typedef dyn_array_type(Operator_Flags *) Array_Operator_Flags_Ptr;
typedef dyn_array_type(const Operator_Flags *) Array_Const_Operator_Flags_Ptr;

typedef struct Operator Operator;
typedef struct Operator_Alias Operator_Alias;
typedef struct Operator_Intrinsic Operator_Intrinsic;
typedef dyn_array_type(Operator *) Array_Operator_Ptr;
typedef dyn_array_type(const Operator *) Array_Const_Operator_Ptr;

typedef struct Scope_Entry Scope_Entry;
typedef dyn_array_type(Scope_Entry *) Array_Scope_Entry_Ptr;
typedef dyn_array_type(const Scope_Entry *) Array_Const_Scope_Entry_Ptr;

typedef struct Operator_Map Operator_Map;

typedef struct Operator_Symbol_Map Operator_Symbol_Map;

typedef struct Scope Scope;
typedef struct Scope_Imperative Scope_Imperative;
typedef struct Scope_Declarative Scope_Declarative;
typedef dyn_array_type(Scope *) Array_Scope_Ptr;
typedef dyn_array_type(const Scope *) Array_Const_Scope_Ptr;

typedef struct Overload Overload;
typedef dyn_array_type(Overload *) Array_Overload_Ptr;
typedef dyn_array_type(const Overload *) Array_Const_Overload_Ptr;

typedef struct Undecidable_Match Undecidable_Match;
typedef dyn_array_type(Undecidable_Match *) Array_Undecidable_Match_Ptr;
typedef dyn_array_type(const Undecidable_Match *) Array_Const_Undecidable_Match_Ptr;

typedef struct Overload_Match Overload_Match;
typedef struct Overload_Match_Undecidable Overload_Match_Undecidable;
typedef struct Overload_Match_Found Overload_Match_Found;
typedef dyn_array_type(Overload_Match *) Array_Overload_Match_Ptr;
typedef dyn_array_type(const Overload_Match *) Array_Const_Overload_Match_Ptr;

typedef struct Overload_Match_Summary Overload_Match_Summary;
typedef dyn_array_type(Overload_Match_Summary *) Array_Overload_Match_Summary_Ptr;
typedef dyn_array_type(const Overload_Match_Summary *) Array_Const_Overload_Match_Summary_Ptr;

typedef struct Overload_Match_State Overload_Match_State;
typedef dyn_array_type(Overload_Match_State *) Array_Overload_Match_State_Ptr;
typedef dyn_array_type(const Overload_Match_State *) Array_Const_Overload_Match_State_Ptr;

typedef enum Value_Flags {
  Value_Flags_None = 0,
  Value_Flags_Constant = 1,
} Value_Flags;

const char *value_flags_name(Value_Flags value) {
  if (value == 0) return "Value_Flags_None";
  if (value == 1) return "Value_Flags_Constant";
  assert(!"Unexpected value for enum Value_Flags");
  return 0;
};

typedef dyn_array_type(Value_Flags *) Array_Value_Flags_Ptr;
typedef dyn_array_type(const Value_Flags *) Array_Const_Value_Flags_Ptr;

typedef struct Value Value;
typedef struct Value_Lazy Value_Lazy;
typedef struct Value_Forced Value_Forced;
typedef dyn_array_type(Value *) Array_Value_Ptr;
typedef dyn_array_type(const Value *) Array_Const_Value_Ptr;

typedef struct Register_Bitset Register_Bitset;
typedef dyn_array_type(Register_Bitset *) Array_Register_Bitset_Ptr;
typedef dyn_array_type(const Register_Bitset *) Array_Const_Register_Bitset_Ptr;

typedef struct Function_Builder Function_Builder;
typedef dyn_array_type(Function_Builder *) Array_Function_Builder_Ptr;
typedef dyn_array_type(const Function_Builder *) Array_Const_Function_Builder_Ptr;

typedef struct Expected_Result Expected_Result;
typedef struct Expected_Result_Exact Expected_Result_Exact;
typedef struct Expected_Result_Flexible Expected_Result_Flexible;
typedef dyn_array_type(Expected_Result *) Array_Expected_Result_Ptr;
typedef dyn_array_type(const Expected_Result *) Array_Const_Expected_Result_Ptr;

typedef struct Lazy_Static_Value Lazy_Static_Value;
typedef dyn_array_type(Lazy_Static_Value *) Array_Lazy_Static_Value_Ptr;
typedef dyn_array_type(const Lazy_Static_Value *) Array_Const_Lazy_Static_Value_Ptr;

typedef Value * (*Mass_Intrinsic_Proc)
  (Mass_Context * context, Parser * parser, Value_View view);

typedef struct Function_Parameter Function_Parameter;
typedef struct Function_Parameter_Generic Function_Parameter_Generic;
typedef struct Function_Parameter_Exact_Static Function_Parameter_Exact_Static;
typedef dyn_array_type(Function_Parameter *) Array_Function_Parameter_Ptr;
typedef dyn_array_type(const Function_Parameter *) Array_Const_Function_Parameter_Ptr;

typedef struct Resolved_Function_Parameter Resolved_Function_Parameter;
typedef struct Resolved_Function_Parameter_Known Resolved_Function_Parameter_Known;
typedef dyn_array_type(Resolved_Function_Parameter *) Array_Resolved_Function_Parameter_Ptr;
typedef dyn_array_type(const Resolved_Function_Parameter *) Array_Const_Resolved_Function_Parameter_Ptr;

typedef enum Function_Info_Flags {
  Function_Info_Flags_None = 0,
  Function_Info_Flags_Compile_Time = 2,
  Function_Info_Flags_Intrinsic = 4,
} Function_Info_Flags;

const char *function_info_flags_name(Function_Info_Flags value) {
  if (value == 0) return "Function_Info_Flags_None";
  if (value == 2) return "Function_Info_Flags_Compile_Time";
  if (value == 4) return "Function_Info_Flags_Intrinsic";
  assert(!"Unexpected value for enum Function_Info_Flags");
  return 0;
};

typedef dyn_array_type(Function_Info_Flags *) Array_Function_Info_Flags_Ptr;
typedef dyn_array_type(const Function_Info_Flags *) Array_Const_Function_Info_Flags_Ptr;

typedef struct Function_Return Function_Return;
typedef struct Function_Return_Generic Function_Return_Generic;
typedef struct Function_Return_Exact Function_Return_Exact;
typedef dyn_array_type(Function_Return *) Array_Function_Return_Ptr;
typedef dyn_array_type(const Function_Return *) Array_Const_Function_Return_Ptr;

typedef struct Function_Info Function_Info;
typedef dyn_array_type(Function_Info *) Array_Function_Info_Ptr;
typedef dyn_array_type(const Function_Info *) Array_Const_Function_Info_Ptr;

typedef enum Function_Header_Flags {
  Function_Header_Flags_None = 0,
  Function_Header_Flags_Intrinsic = 2,
  Function_Header_Flags_Compile_Time = 4,
} Function_Header_Flags;

const char *function_header_flags_name(Function_Header_Flags value) {
  if (value == 0) return "Function_Header_Flags_None";
  if (value == 2) return "Function_Header_Flags_Intrinsic";
  if (value == 4) return "Function_Header_Flags_Compile_Time";
  assert(!"Unexpected value for enum Function_Header_Flags");
  return 0;
};

typedef dyn_array_type(Function_Header_Flags *) Array_Function_Header_Flags_Ptr;
typedef dyn_array_type(const Function_Header_Flags *) Array_Const_Function_Header_Flags_Ptr;

typedef struct Function_Specialization Function_Specialization;
typedef dyn_array_type(Function_Specialization *) Array_Function_Specialization_Ptr;
typedef dyn_array_type(const Function_Specialization *) Array_Const_Function_Specialization_Ptr;

typedef struct Function_Header Function_Header;
typedef dyn_array_type(Function_Header *) Array_Function_Header_Ptr;
typedef dyn_array_type(const Function_Header *) Array_Const_Function_Header_Ptr;

typedef struct Function_Literal Function_Literal;
typedef dyn_array_type(Function_Literal *) Array_Function_Literal_Ptr;
typedef dyn_array_type(const Function_Literal *) Array_Const_Function_Literal_Ptr;

typedef enum Function_Call_Parameter_Flags {
  Function_Call_Parameter_Flags_None = 0,
  Function_Call_Parameter_Flags_Uninitialized = 1,
  Function_Call_Parameter_Flags_Implicit_Pointer = 2,
} Function_Call_Parameter_Flags;

const char *function_call_parameter_flags_name(Function_Call_Parameter_Flags value) {
  if (value == 0) return "Function_Call_Parameter_Flags_None";
  if (value == 1) return "Function_Call_Parameter_Flags_Uninitialized";
  if (value == 2) return "Function_Call_Parameter_Flags_Implicit_Pointer";
  assert(!"Unexpected value for enum Function_Call_Parameter_Flags");
  return 0;
};

typedef dyn_array_type(Function_Call_Parameter_Flags *) Array_Function_Call_Parameter_Flags_Ptr;
typedef dyn_array_type(const Function_Call_Parameter_Flags *) Array_Const_Function_Call_Parameter_Flags_Ptr;

typedef struct Function_Call_Parameter Function_Call_Parameter;
typedef dyn_array_type(Function_Call_Parameter *) Array_Function_Call_Parameter_Ptr;
typedef dyn_array_type(const Function_Call_Parameter *) Array_Const_Function_Call_Parameter_Ptr;

typedef void (*Mass_Call_Encode_Proc)
  (Function_Builder * builder, Storage address_storage, const Source_Range * source_range, const Scope * scope);

typedef struct Function_Call_Setup Function_Call_Setup;
typedef dyn_array_type(Function_Call_Setup *) Array_Function_Call_Setup_Ptr;
typedef dyn_array_type(const Function_Call_Setup *) Array_Const_Function_Call_Setup_Ptr;

typedef enum SYSTEM_V_ARGUMENT_CLASS {
  SYSTEM_V_ARGUMENT_CLASS_NO_CLASS = 0,
  SYSTEM_V_ARGUMENT_CLASS_INTEGER = 1,
  SYSTEM_V_ARGUMENT_CLASS_SSE = 2,
  SYSTEM_V_ARGUMENT_CLASS_SSEUP = 3,
  SYSTEM_V_ARGUMENT_CLASS_X87 = 4,
  SYSTEM_V_ARGUMENT_CLASS_X87UP = 5,
  SYSTEM_V_ARGUMENT_CLASS_COMPLEX_X87 = 6,
  SYSTEM_V_ARGUMENT_CLASS_MEMORY = 7,
} SYSTEM_V_ARGUMENT_CLASS;

const char *system_v_argument_class_name(SYSTEM_V_ARGUMENT_CLASS value) {
  if (value == 0) return "SYSTEM_V_ARGUMENT_CLASS_NO_CLASS";
  if (value == 1) return "SYSTEM_V_ARGUMENT_CLASS_INTEGER";
  if (value == 2) return "SYSTEM_V_ARGUMENT_CLASS_SSE";
  if (value == 3) return "SYSTEM_V_ARGUMENT_CLASS_SSEUP";
  if (value == 4) return "SYSTEM_V_ARGUMENT_CLASS_X87";
  if (value == 5) return "SYSTEM_V_ARGUMENT_CLASS_X87UP";
  if (value == 6) return "SYSTEM_V_ARGUMENT_CLASS_COMPLEX_X87";
  if (value == 7) return "SYSTEM_V_ARGUMENT_CLASS_MEMORY";
  assert(!"Unexpected value for enum SYSTEM_V_ARGUMENT_CLASS");
  return 0;
};

typedef dyn_array_type(SYSTEM_V_ARGUMENT_CLASS *) Array_SYSTEM_V_ARGUMENT_CLASS_Ptr;
typedef dyn_array_type(const SYSTEM_V_ARGUMENT_CLASS *) Array_Const_SYSTEM_V_ARGUMENT_CLASS_Ptr;

typedef struct System_V_Classification System_V_Classification;
typedef dyn_array_type(System_V_Classification *) Array_System_V_Classification_Ptr;
typedef dyn_array_type(const System_V_Classification *) Array_Const_System_V_Classification_Ptr;

typedef struct System_V_Registers System_V_Registers;
typedef dyn_array_type(System_V_Registers *) Array_System_V_Registers_Ptr;
typedef dyn_array_type(const System_V_Registers *) Array_Const_System_V_Registers_Ptr;

typedef struct System_V_Register_State System_V_Register_State;
typedef dyn_array_type(System_V_Register_State *) Array_System_V_Register_State_Ptr;
typedef dyn_array_type(const System_V_Register_State *) Array_Const_System_V_Register_State_Ptr;

typedef struct System_V_Eightbyte_Array System_V_Eightbyte_Array;
typedef dyn_array_type(System_V_Eightbyte_Array *) Array_System_V_Eightbyte_Array_Ptr;
typedef dyn_array_type(const System_V_Eightbyte_Array *) Array_Const_System_V_Eightbyte_Array_Ptr;

typedef struct Mass_Function_Call_Lazy_Payload Mass_Function_Call_Lazy_Payload;
typedef dyn_array_type(Mass_Function_Call_Lazy_Payload *) Array_Mass_Function_Call_Lazy_Payload_Ptr;
typedef dyn_array_type(const Mass_Function_Call_Lazy_Payload *) Array_Const_Mass_Function_Call_Lazy_Payload_Ptr;

typedef struct Tuple Tuple;
typedef dyn_array_type(Tuple *) Array_Tuple_Ptr;
typedef dyn_array_type(const Tuple *) Array_Const_Tuple_Ptr;

typedef struct List_Node List_Node;
typedef dyn_array_type(List_Node *) Array_List_Node_Ptr;
typedef dyn_array_type(const List_Node *) Array_Const_List_Node_Ptr;

typedef struct Typed_Symbol Typed_Symbol;
typedef dyn_array_type(Typed_Symbol *) Array_Typed_Symbol_Ptr;
typedef dyn_array_type(const Typed_Symbol *) Array_Const_Typed_Symbol_Ptr;

typedef struct Struct_Field Struct_Field;
typedef dyn_array_type(Struct_Field *) Array_Struct_Field_Ptr;
typedef dyn_array_type(const Struct_Field *) Array_Const_Struct_Field_Ptr;

typedef struct Descriptor Descriptor;
typedef struct Descriptor_Integer Descriptor_Integer;
typedef struct Descriptor_Function_Instance Descriptor_Function_Instance;
typedef struct Descriptor_Fixed_Array Descriptor_Fixed_Array;
typedef struct Descriptor_Struct Descriptor_Struct;
typedef struct Descriptor_Pointer_To Descriptor_Pointer_To;
typedef dyn_array_type(Descriptor *) Array_Descriptor_Ptr;
typedef dyn_array_type(const Descriptor *) Array_Const_Descriptor_Ptr;

typedef struct Type Type;
typedef dyn_array_type(Type *) Array_Type_Ptr;
typedef dyn_array_type(const Type *) Array_Const_Type_Ptr;

typedef _Bool (*Mass_Type_Constraint_Proc)
  (Type type);

typedef struct Mass_Error Mass_Error;
typedef struct Mass_Error_User_Defined Mass_Error_User_Defined;
typedef struct Mass_Error_Circular_Dependency Mass_Error_Circular_Dependency;
typedef struct Mass_Error_Integer_Range Mass_Error_Integer_Range;
typedef struct Mass_Error_File_Open Mass_Error_File_Open;
typedef struct Mass_Error_File_Too_Large Mass_Error_File_Too_Large;
typedef struct Mass_Error_Dynamic_Library_Load Mass_Error_Dynamic_Library_Load;
typedef struct Mass_Error_Dynamic_Library_Symbol_Not_Found Mass_Error_Dynamic_Library_Symbol_Not_Found;
typedef struct Mass_Error_Operator_Fixity_Conflict Mass_Error_Operator_Fixity_Conflict;
typedef struct Mass_Error_Undefined_Variable Mass_Error_Undefined_Variable;
typedef struct Mass_Error_Redefinition Mass_Error_Redefinition;
typedef struct Mass_Error_Unknown_Field Mass_Error_Unknown_Field;
typedef struct Mass_Error_Invalid_Identifier Mass_Error_Invalid_Identifier;
typedef struct Mass_Error_Type_Mismatch Mass_Error_Type_Mismatch;
typedef struct Mass_Error_No_Matching_Overload Mass_Error_No_Matching_Overload;
typedef struct Mass_Error_Undecidable_Overload Mass_Error_Undecidable_Overload;
typedef dyn_array_type(Mass_Error *) Array_Mass_Error_Ptr;
typedef dyn_array_type(const Mass_Error *) Array_Const_Mass_Error_Ptr;

typedef struct Mass_Result Mass_Result;
typedef struct Mass_Result_Error Mass_Result_Error;
typedef dyn_array_type(Mass_Result *) Array_Mass_Result_Ptr;
typedef dyn_array_type(const Mass_Result *) Array_Const_Mass_Result_Ptr;

typedef enum Os {
  Os_Windows = 1,
  Os_Linux = 2,
  Os_Mac = 3,
} Os;

const char *os_name(Os value) {
  if (value == 1) return "Os_Windows";
  if (value == 2) return "Os_Linux";
  if (value == 3) return "Os_Mac";
  assert(!"Unexpected value for enum Os");
  return 0;
};

typedef dyn_array_type(Os *) Array_Os_Ptr;
typedef dyn_array_type(const Os *) Array_Const_Os_Ptr;

typedef struct Program Program;
typedef dyn_array_type(Program *) Array_Program_Ptr;
typedef dyn_array_type(const Program *) Array_Const_Program_Ptr;

typedef Function_Call_Setup (*Calling_Convention_Call_Setup_Proc)
  (const Allocator * allocator, const Function_Info * function_info);

typedef struct Calling_Convention Calling_Convention;
typedef dyn_array_type(Calling_Convention *) Array_Calling_Convention_Ptr;
typedef dyn_array_type(const Calling_Convention *) Array_Const_Calling_Convention_Ptr;

typedef void (*Mass_Trampoline_Proc)
  (void * returns, void * args);

typedef struct Mass_Trampoline Mass_Trampoline;
typedef dyn_array_type(Mass_Trampoline *) Array_Mass_Trampoline_Ptr;
typedef dyn_array_type(const Mass_Trampoline *) Array_Const_Mass_Trampoline_Ptr;

typedef struct Struct_Field_Set Struct_Field_Set;

typedef struct Slice_Set Slice_Set;

typedef struct Symbol_Map Symbol_Map;

typedef struct Trampoline_Map Trampoline_Map;

typedef struct Scope_Map Scope_Map;

typedef struct Macro_Replacement_Map Macro_Replacement_Map;

typedef struct Jit_Import_Library_Handle_Map Jit_Import_Library_Handle_Map;

typedef struct Imported_Module_Map Imported_Module_Map;

typedef struct Jit_Counters Jit_Counters;
typedef dyn_array_type(Jit_Counters *) Array_Jit_Counters_Ptr;
typedef dyn_array_type(const Jit_Counters *) Array_Const_Jit_Counters_Ptr;

typedef struct Jit Jit;
typedef dyn_array_type(Jit *) Array_Jit_Ptr;
typedef dyn_array_type(const Jit *) Array_Const_Jit_Ptr;

typedef struct Static_Pointer_Length_Map Static_Pointer_Length_Map;

typedef struct Descriptor_Pointer_To_Cache_Map Descriptor_Pointer_To_Cache_Map;

typedef struct Intrinsic_Proc_Cache_Map Intrinsic_Proc_Cache_Map;

typedef struct Common_Symbols Common_Symbols;
typedef dyn_array_type(Common_Symbols *) Array_Common_Symbols_Ptr;
typedef dyn_array_type(const Common_Symbols *) Array_Const_Common_Symbols_Ptr;

typedef struct Compilation Compilation;
typedef dyn_array_type(Compilation *) Array_Compilation_Ptr;
typedef dyn_array_type(const Compilation *) Array_Const_Compilation_Ptr;

typedef Value * (*Lazy_Value_Proc)
  (Mass_Context * context, Function_Builder * builder, const Expected_Result * expected_result, const Scope * scope, const Source_Range * source_range, const void * payload);

typedef enum Instruction_Extension_Type {
  Instruction_Extension_Type_None = 0,
  Instruction_Extension_Type_Register = 1,
  Instruction_Extension_Type_Op_Code = 2,
  Instruction_Extension_Type_Plus_Register = 3,
} Instruction_Extension_Type;

const char *instruction_extension_type_name(Instruction_Extension_Type value) {
  if (value == 0) return "Instruction_Extension_Type_None";
  if (value == 1) return "Instruction_Extension_Type_Register";
  if (value == 2) return "Instruction_Extension_Type_Op_Code";
  if (value == 3) return "Instruction_Extension_Type_Plus_Register";
  assert(!"Unexpected value for enum Instruction_Extension_Type");
  return 0;
};

typedef dyn_array_type(Instruction_Extension_Type *) Array_Instruction_Extension_Type_Ptr;
typedef dyn_array_type(const Instruction_Extension_Type *) Array_Const_Instruction_Extension_Type_Ptr;

typedef enum Operand_Encoding_Type {
  Operand_Encoding_Type_None = 0,
  Operand_Encoding_Type_Register = 2,
  Operand_Encoding_Type_Register_A = 3,
  Operand_Encoding_Type_Register_Or_Memory = 4,
  Operand_Encoding_Type_Xmm = 5,
  Operand_Encoding_Type_Memory = 7,
  Operand_Encoding_Type_Immediate = 8,
} Operand_Encoding_Type;

const char *operand_encoding_type_name(Operand_Encoding_Type value) {
  if (value == 0) return "Operand_Encoding_Type_None";
  if (value == 2) return "Operand_Encoding_Type_Register";
  if (value == 3) return "Operand_Encoding_Type_Register_A";
  if (value == 4) return "Operand_Encoding_Type_Register_Or_Memory";
  if (value == 5) return "Operand_Encoding_Type_Xmm";
  if (value == 7) return "Operand_Encoding_Type_Memory";
  if (value == 8) return "Operand_Encoding_Type_Immediate";
  assert(!"Unexpected value for enum Operand_Encoding_Type");
  return 0;
};

typedef dyn_array_type(Operand_Encoding_Type *) Array_Operand_Encoding_Type_Ptr;
typedef dyn_array_type(const Operand_Encoding_Type *) Array_Const_Operand_Encoding_Type_Ptr;

typedef struct Operand_Encoding Operand_Encoding;
typedef dyn_array_type(Operand_Encoding *) Array_Operand_Encoding_Ptr;
typedef dyn_array_type(const Operand_Encoding *) Array_Const_Operand_Encoding_Ptr;

typedef struct Instruction_Encoding Instruction_Encoding;
typedef dyn_array_type(Instruction_Encoding *) Array_Instruction_Encoding_Ptr;
typedef dyn_array_type(const Instruction_Encoding *) Array_Const_Instruction_Encoding_Ptr;

typedef struct X64_Mnemonic X64_Mnemonic;
typedef dyn_array_type(X64_Mnemonic *) Array_X64_Mnemonic_Ptr;
typedef dyn_array_type(const X64_Mnemonic *) Array_Const_X64_Mnemonic_Ptr;

static System_V_Classification x86_64_system_v_classify
  (const Descriptor * descriptor);

static Function_Call_Parameter x86_64_system_v_parameter_for_classification
  (const Allocator * allocator, System_V_Register_State * registers, const System_V_Classification * classification, u64 * stack_offset);

static void call_setup_fill_parameter_register_bitset
  (Function_Call_Setup * setup);

static void push_instruction
  (Code_Block * code_block, Instruction instruction);

static Value * mass_get
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_get_from_descriptor_module
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_import
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_intrinsic
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_apply
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_call
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_using
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_return
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_operator_assignment
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_define_inferred
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_comma
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_function_literal
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_dereference
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_array_like_get
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_struct_get
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_named_accessor
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_spread
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_typed_symbol
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_pointer_to
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_pointer_to_type
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_eval
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_inline_module
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_c_struct
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_exports
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_cast
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_zero_extend
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_parse_type
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_type_of
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_size_of
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_static_assert
  (Mass_Context * context, Parser * parser, Value_View args);

static void * allocator_allocate_bytes
  (const Allocator * allocator, u64 byte_size, u64 byte_alignment);

static _Bool mass_constraint_integer_type
  (Type type);

static _Bool mass_constraint_float_type
  (Type type);

static _Bool mass_constraint_pointer_type
  (Type type);

static _Bool mass_constraint_struct_type
  (Type type);

static _Bool mass_constraint_fixed_array_type
  (Type type);

static _Bool mass_constraint_function_instance_type
  (Type type);

static u64 mass_tuple_length
  (const Tuple * tuple);

static Value * mass_tuple_get
  (const Tuple * tuple, u64 index);

static const Descriptor * descriptor_pointer_to
  (Compilation * compilation, const Descriptor * descriptor);

static Scope * scope_make_imperative
  (const Allocator * allocator, const Scope * parent, const Scope_Entry * entry);

static Scope * scope_make_declarative
  (const Allocator * allocator, const Scope * parent);

static const Symbol * mass_ensure_symbol
  (Compilation * compilation, Slice name);

static void scope_define_value
  (Scope * scope, Epoch epoch, Source_Range source_range, const Symbol * symbol, Value * value);

static _Bool same_type
  (const Descriptor * a, const Descriptor * b);

static Register register_acquire
  (Function_Builder * builder, Register reg);

static Register register_acquire_temp
  (Function_Builder * builder);

static void register_release
  (Function_Builder * builder, Register reg);

static Storage storage_register
  (Register reg, Bits bit_size);

static Storage storage_register_temp
  (Function_Builder * builder, Bits bit_size);

static void storage_release_if_temporary
  (Function_Builder * builder, const Storage * storage);

static Expected_Result mass_expected_result_exact
  (const Descriptor * descriptor, Storage storage);

static Expected_Result mass_expected_result_exact_type
  (Type type, Storage storage);

static Value * value_force
  (Mass_Context * context, Function_Builder * builder, const Scope * scope, const Expected_Result * expected_result, Value * value);

static Value * mass_module_get_impl
  (Mass_Context * context, const Module * module, const Symbol * symbol, const Source_Range * source_range);

static Value * mass_forward_call_to_alias
  (Mass_Context * context, Parser * parser, Value_View args, const Symbol * symbol);

static i64 mass_i64_logical_shift_left
  (i64 a, i64 b);

static i64 mass_i64_logical_shift_right
  (i64 a, i64 b);

static i64 mass_i64_bitwise_and
  (i64 a, i64 b);

static i64 mass_i64_bitwise_or
  (i64 a, i64 b);

static i64 mass_i64_add
  (i64 a, i64 b);

static i64 mass_i64_subtract
  (i64 a, i64 b);

static i64 mass_i64_signed_multiply
  (i64 a, i64 b);

static i64 mass_i64_unsigned_multiply
  (i64 a, i64 b);

static i64 mass_i64_signed_divide
  (i64 a, i64 b);

static i64 mass_i64_unsigned_divide
  (i64 a, i64 b);

static i64 mass_i64_signed_remainder
  (i64 a, i64 b);

static i64 mass_i64_unsigned_remainder
  (i64 a, i64 b);

static _Bool mass_i64_signed_less
  (i64 a, i64 b);

static _Bool mass_i64_unsigned_less
  (i64 a, i64 b);

static _Bool mass_i64_signed_less_equal
  (i64 a, i64 b);

static _Bool mass_i64_unsigned_less_equal
  (i64 a, i64 b);

static _Bool mass_i64_signed_greater
  (i64 a, i64 b);

static _Bool mass_i64_unsigned_greater
  (i64 a, i64 b);

static _Bool mass_i64_signed_greater_equal
  (i64 a, i64 b);

static _Bool mass_i64_unsigned_greater_equal
  (i64 a, i64 b);

static _Bool slice_equal
  (Slice a, Slice b);

static Value * value_make
  (Mass_Context * context, const Descriptor * descriptor, Storage storage, Source_Range source_range);

static Descriptor * descriptor_function_instance
  (const Allocator * allocator, const Function_Info * info, Function_Call_Setup call_setup, const Program * program);

static _Bool mass_result_is_error
  (Mass_Result * result);

static Function_Call_Setup calling_convention_x86_64_system_v_call_setup_proc
  (const Allocator * allocator, const Function_Info * function);

static void mass_function_info_init_for_header_and_maybe_body
  (Mass_Context * context, const Scope * scope, const Function_Header * header, Value * maybe_body, Function_Info * out_info);

static Value * call_function_overload
  (Mass_Context * context, Function_Builder * builder, const Expected_Result * expected_result, const Scope * scope, const Source_Range * source_range, const Mass_Function_Call_Lazy_Payload * payload);

static Value * mass_integer_add
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_integer_subtract
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_integer_multiply
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_integer_divide
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_integer_remainder
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_integer_less
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_integer_greater
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_integer_less_equal
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_integer_greater_equal
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_integer_equal
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_integer_not_equal
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_generic_equal
  (Mass_Context * context, Parser * parser, Value_View args);

static Value * mass_generic_not_equal
  (Mass_Context * context, Parser * parser, Value_View args);

typedef dyn_array_type(_Bool *) Array__Bool_Ptr;
typedef dyn_array_type(const _Bool *) Array_Const__Bool_Ptr;

typedef dyn_array_type(char *) Array_char_Ptr;
typedef dyn_array_type(const char *) Array_Const_char_Ptr;

typedef dyn_array_type(int *) Array_int_Ptr;
typedef dyn_array_type(const int *) Array_Const_int_Ptr;

typedef dyn_array_type(Allocator *) Array_Allocator_Ptr;
typedef dyn_array_type(const Allocator *) Array_Const_Allocator_Ptr;

typedef dyn_array_type(Virtual_Memory_Buffer *) Array_Virtual_Memory_Buffer_Ptr;
typedef dyn_array_type(const Virtual_Memory_Buffer *) Array_Const_Virtual_Memory_Buffer_Ptr;

typedef dyn_array_type(i8 *) Array_i8_Ptr;
typedef dyn_array_type(const i8 *) Array_Const_i8_Ptr;

typedef dyn_array_type(i16 *) Array_i16_Ptr;
typedef dyn_array_type(const i16 *) Array_Const_i16_Ptr;

typedef dyn_array_type(i32 *) Array_i32_Ptr;
typedef dyn_array_type(const i32 *) Array_Const_i32_Ptr;

typedef dyn_array_type(i64 *) Array_i64_Ptr;
typedef dyn_array_type(const i64 *) Array_Const_i64_Ptr;

typedef dyn_array_type(u8 *) Array_u8_Ptr;
typedef dyn_array_type(const u8 *) Array_Const_u8_Ptr;

typedef dyn_array_type(u16 *) Array_u16_Ptr;
typedef dyn_array_type(const u16 *) Array_Const_u16_Ptr;

typedef dyn_array_type(u32 *) Array_u32_Ptr;
typedef dyn_array_type(const u32 *) Array_Const_u32_Ptr;

typedef dyn_array_type(u64 *) Array_u64_Ptr;
typedef dyn_array_type(const u64 *) Array_Const_u64_Ptr;

typedef dyn_array_type(s8 *) Array_s8_Ptr;
typedef dyn_array_type(const s8 *) Array_Const_s8_Ptr;

typedef dyn_array_type(s16 *) Array_s16_Ptr;
typedef dyn_array_type(const s16 *) Array_Const_s16_Ptr;

typedef dyn_array_type(s32 *) Array_s32_Ptr;
typedef dyn_array_type(const s32 *) Array_Const_s32_Ptr;

typedef dyn_array_type(s64 *) Array_s64_Ptr;
typedef dyn_array_type(const s64 *) Array_Const_s64_Ptr;

typedef dyn_array_type(f32 *) Array_f32_Ptr;
typedef dyn_array_type(const f32 *) Array_Const_f32_Ptr;

typedef dyn_array_type(f64 *) Array_f64_Ptr;
typedef dyn_array_type(const f64 *) Array_Const_f64_Ptr;

typedef dyn_array_type(Range_u8 *) Array_Range_u8_Ptr;
typedef dyn_array_type(const Range_u8 *) Array_Const_Range_u8_Ptr;

typedef dyn_array_type(Range_u16 *) Array_Range_u16_Ptr;
typedef dyn_array_type(const Range_u16 *) Array_Const_Range_u16_Ptr;

typedef dyn_array_type(Range_u32 *) Array_Range_u32_Ptr;
typedef dyn_array_type(const Range_u32 *) Array_Const_Range_u32_Ptr;

typedef dyn_array_type(Range_u64 *) Array_Range_u64_Ptr;
typedef dyn_array_type(const Range_u64 *) Array_Const_Range_u64_Ptr;

typedef dyn_array_type(Range_s8 *) Array_Range_s8_Ptr;
typedef dyn_array_type(const Range_s8 *) Array_Const_Range_s8_Ptr;

typedef dyn_array_type(Range_s16 *) Array_Range_s16_Ptr;
typedef dyn_array_type(const Range_s16 *) Array_Const_Range_s16_Ptr;

typedef dyn_array_type(Range_s32 *) Array_Range_s32_Ptr;
typedef dyn_array_type(const Range_s32 *) Array_Const_Range_s32_Ptr;

typedef dyn_array_type(Range_s64 *) Array_Range_s64_Ptr;
typedef dyn_array_type(const Range_s64 *) Array_Const_Range_s64_Ptr;

typedef dyn_array_type(Range_f32 *) Array_Range_f32_Ptr;
typedef dyn_array_type(const Range_f32 *) Array_Const_Range_f32_Ptr;

typedef dyn_array_type(Range_f64 *) Array_Range_f64_Ptr;
typedef dyn_array_type(const Range_f64 *) Array_Const_Range_f64_Ptr;

typedef dyn_array_type(Slice *) Array_Slice_Ptr;
typedef dyn_array_type(const Slice *) Array_Const_Slice_Ptr;

typedef dyn_array_type(Dyn_Array_Internal *) Array_Dyn_Array_Internal_Ptr;
typedef dyn_array_type(const Dyn_Array_Internal *) Array_Const_Dyn_Array_Internal_Ptr;


// Type Definitions

typedef struct Bits {
  u64 as_u64;
} Bits;
typedef dyn_array_type(Bits) Array_Bits;

typedef struct Source_Position {
  u64 line;
  u64 column;
} Source_Position;
typedef dyn_array_type(Source_Position) Array_Source_Position;

typedef struct Source_File {
  Slice path;
  Slice text;
} Source_File;
typedef dyn_array_type(Source_File) Array_Source_File;

typedef struct Source_Range {
  const Source_File * file;
  Range_u32 offsets;
} Source_Range;
typedef dyn_array_type(Source_Range) Array_Source_Range;

typedef struct Mass_While {
  Value * condition;
  Value * body;
} Mass_While;
typedef dyn_array_type(Mass_While) Array_Mass_While;

typedef struct Assignment {
  Value * target;
  Value * source;
} Assignment;
typedef dyn_array_type(Assignment) Array_Assignment;

typedef enum {
  Module_Exports_Tag_Not_Specified = 0,
  Module_Exports_Tag_All = 1,
  Module_Exports_Tag_Selective = 2,
} Module_Exports_Tag;

const char *module_exports_tag_name(Module_Exports_Tag value) {
  if (value == 0) return "Module_Exports_Not_Specified";
  if (value == 1) return "Module_Exports_All";
  if (value == 2) return "Module_Exports_Selective";
  return "<unknown value>";}

typedef struct Module_Exports_Selective {
  const Tuple * tuple;
} Module_Exports_Selective;
typedef struct Module_Exports {
  Module_Exports_Tag tag;
  char _tag_padding[4];
  Scope * scope;
  Source_Range source_range;
  union {
    Module_Exports_Selective Selective;
  };
} Module_Exports;
static inline const Module_Exports_Selective *
module_exports_as_selective(const Module_Exports *module_exports) {
  assert(module_exports->tag == Module_Exports_Tag_Selective);
  return &module_exports->Selective;
}
typedef dyn_array_type(Module_Exports) Array_Module_Exports;
typedef struct Module {
  Source_Range source_range;
  Scope * own_scope;
  Module_Exports exports;
} Module;
typedef dyn_array_type(Module) Array_Module;

typedef struct Parse_Error {
  Slice message;
  Source_Range source_range;
} Parse_Error;
typedef dyn_array_type(Parse_Error) Array_Parse_Error;

typedef struct Value_View {
  Value * * values;
  u32 length;
  u32 _length_padding;
  Source_Range source_range;
} Value_View;
typedef dyn_array_type(Value_View) Array_Value_View;

typedef struct Symbol {
  Slice name;
} Symbol;
typedef dyn_array_type(Symbol) Array_Symbol;

typedef struct Group_Paren {
  Value_View children;
} Group_Paren;
typedef dyn_array_type(Group_Paren) Array_Group_Paren;

typedef struct Ast_Return {
  Value * value;
} Ast_Return;
typedef dyn_array_type(Ast_Return) Array_Ast_Return;

typedef struct Ast_Statement {
  Value_View children;
  Ast_Statement * next;
} Ast_Statement;
typedef dyn_array_type(Ast_Statement) Array_Ast_Statement;

typedef struct Ast_Block {
  Ast_Statement * first_statement;
  Ast_Statement * last_statement;
} Ast_Block;
typedef dyn_array_type(Ast_Block) Array_Ast_Block;

typedef struct Group_Square {
  Value_View children;
} Group_Square;
typedef dyn_array_type(Group_Square) Array_Group_Square;

typedef struct Ast_Using {
  const Module * module;
} Ast_Using;
typedef dyn_array_type(Ast_Using) Array_Ast_Using;

typedef struct Section {
  Virtual_Memory_Buffer buffer;
  Slice name;
  u32 base_rva;
  Section_Permissions permissions;
} Section;
typedef dyn_array_type(Section) Array_Section;

typedef struct Program_Memory {
  Virtual_Memory_Buffer buffer;
  Section rw_data;
  Section code;
  Section ro_data;
} Program_Memory;
typedef dyn_array_type(Program_Memory) Array_Program_Memory;

typedef struct Label {
  u32 resolved;
  u32 offset_in_section;
  Slice name;
  Section * section;
  const Program * program;
} Label;
typedef dyn_array_type(Label) Array_Label;

typedef struct Label_Location_Diff_Patch_Info {
  Label * target;
  Label from;
  void * patch32_at;
  s32 offset_from_label;
  u32 _offset_from_label_padding;
} Label_Location_Diff_Patch_Info;
typedef dyn_array_type(Label_Location_Diff_Patch_Info) Array_Label_Location_Diff_Patch_Info;

typedef struct Quoted {
  Value * value;
} Quoted;
typedef dyn_array_type(Quoted) Array_Quoted;

typedef struct Spread {
  Value * value;
} Spread;
typedef dyn_array_type(Spread) Array_Spread;

typedef struct Named_Accessor {
  const Symbol * symbol;
} Named_Accessor;
typedef dyn_array_type(Named_Accessor) Array_Named_Accessor;

typedef struct External_Symbol {
  Slice library_name;
  Slice symbol_name;
} External_Symbol;
typedef dyn_array_type(External_Symbol) Array_External_Symbol;

typedef struct Import_Symbol {
  Slice name;
  Label * label32;
} Import_Symbol;
typedef dyn_array_type(Import_Symbol) Array_Import_Symbol;

typedef struct Import_Library {
  Slice name;
  Array_Import_Symbol symbols;
} Import_Library;
typedef dyn_array_type(Import_Library) Array_Import_Library;

typedef enum {
  Memory_Location_Tag_Instruction_Pointer_Relative = 0,
  Memory_Location_Tag_Indirect = 1,
  Memory_Location_Tag_Stack = 2,
} Memory_Location_Tag;

const char *memory_location_tag_name(Memory_Location_Tag value) {
  if (value == 0) return "Memory_Location_Instruction_Pointer_Relative";
  if (value == 1) return "Memory_Location_Indirect";
  if (value == 2) return "Memory_Location_Stack";
  return "<unknown value>";}

typedef struct Memory_Location_Instruction_Pointer_Relative {
  Label * label;
  s64 offset;
} Memory_Location_Instruction_Pointer_Relative;
typedef struct Memory_Location_Indirect {
  Register base_register;
  s32 offset;
} Memory_Location_Indirect;
typedef struct Memory_Location_Stack {
  Stack_Area area;
  s32 offset;
} Memory_Location_Stack;
typedef struct Memory_Location {
  Memory_Location_Tag tag;
  char _tag_padding[4];
  union {
    Memory_Location_Instruction_Pointer_Relative Instruction_Pointer_Relative;
    Memory_Location_Indirect Indirect;
    Memory_Location_Stack Stack;
  };
} Memory_Location;
static inline const Memory_Location_Instruction_Pointer_Relative *
memory_location_as_instruction_pointer_relative(const Memory_Location *memory_location) {
  assert(memory_location->tag == Memory_Location_Tag_Instruction_Pointer_Relative);
  return &memory_location->Instruction_Pointer_Relative;
}
static inline const Memory_Location_Indirect *
memory_location_as_indirect(const Memory_Location *memory_location) {
  assert(memory_location->tag == Memory_Location_Tag_Indirect);
  return &memory_location->Indirect;
}
static inline const Memory_Location_Stack *
memory_location_as_stack(const Memory_Location *memory_location) {
  assert(memory_location->tag == Memory_Location_Tag_Stack);
  return &memory_location->Stack;
}
typedef dyn_array_type(Memory_Location) Array_Memory_Location;
typedef enum {
  Storage_Tag_Immediate = 0,
  Storage_Tag_Eflags = 1,
  Storage_Tag_Register = 2,
  Storage_Tag_Xmm = 3,
  Storage_Tag_Static = 4,
  Storage_Tag_Memory = 5,
  Storage_Tag_Disjoint = 6,
} Storage_Tag;

const char *storage_tag_name(Storage_Tag value) {
  if (value == 0) return "Storage_Immediate";
  if (value == 1) return "Storage_Eflags";
  if (value == 2) return "Storage_Register";
  if (value == 3) return "Storage_Xmm";
  if (value == 4) return "Storage_Static";
  if (value == 5) return "Storage_Memory";
  if (value == 6) return "Storage_Disjoint";
  return "<unknown value>";}

typedef struct Storage_Immediate {
  u64 bits;
} Storage_Immediate;
typedef struct Storage_Eflags {
  Compare_Type compare_type;
} Storage_Eflags;
typedef struct Storage_Register {
  Register index;
  u16 packed;
  u16 offset_in_bits;
} Storage_Register;
typedef struct Storage_Xmm {
  Register index;
  u32 offset;
} Storage_Xmm;
typedef struct Storage_Static {
  const void * pointer;
} Storage_Static;
typedef struct Storage_Memory {
  Memory_Location location;
} Storage_Memory;
typedef struct Storage_Disjoint {
  Array_Storage_Ptr pieces;
  u32 packed;
  u32 offset_in_bits;
} Storage_Disjoint;
typedef struct Storage {
  Storage_Tag tag;
  char _tag_padding[4];
  Storage_Flags flags;
  u32 _flags_padding;
  Bits bit_size;
  union {
    Storage_Immediate Immediate;
    Storage_Eflags Eflags;
    Storage_Register Register;
    Storage_Xmm Xmm;
    Storage_Static Static;
    Storage_Memory Memory;
    Storage_Disjoint Disjoint;
  };
} Storage;
static inline const Storage_Immediate *
storage_as_immediate(const Storage *storage) {
  assert(storage->tag == Storage_Tag_Immediate);
  return &storage->Immediate;
}
static inline const Storage_Eflags *
storage_as_eflags(const Storage *storage) {
  assert(storage->tag == Storage_Tag_Eflags);
  return &storage->Eflags;
}
static inline const Storage_Register *
storage_as_register(const Storage *storage) {
  assert(storage->tag == Storage_Tag_Register);
  return &storage->Register;
}
static inline const Storage_Xmm *
storage_as_xmm(const Storage *storage) {
  assert(storage->tag == Storage_Tag_Xmm);
  return &storage->Xmm;
}
static inline const Storage_Static *
storage_as_static(const Storage *storage) {
  assert(storage->tag == Storage_Tag_Static);
  return &storage->Static;
}
static inline const Storage_Memory *
storage_as_memory(const Storage *storage) {
  assert(storage->tag == Storage_Tag_Memory);
  return &storage->Memory;
}
static inline const Storage_Disjoint *
storage_as_disjoint(const Storage *storage) {
  assert(storage->tag == Storage_Tag_Disjoint);
  return &storage->Disjoint;
}
typedef dyn_array_type(Storage) Array_Storage;
typedef struct Relocation {
  Storage patch_at;
  Storage address_of;
} Relocation;
typedef dyn_array_type(Relocation) Array_Relocation;

typedef struct Instruction_Assembly {
  const X64_Mnemonic * mnemonic;
  Storage operands[3];
} Instruction_Assembly;
typedef dyn_array_type(Instruction_Assembly) Array_Instruction_Assembly;

typedef enum {
  Instruction_Tag_Label = 0,
  Instruction_Tag_Bytes = 1,
  Instruction_Tag_Label_Patch = 2,
  Instruction_Tag_Stack_Patch = 3,
  Instruction_Tag_Location = 4,
} Instruction_Tag;

const char *instruction_tag_name(Instruction_Tag value) {
  if (value == 0) return "Instruction_Label";
  if (value == 1) return "Instruction_Bytes";
  if (value == 2) return "Instruction_Label_Patch";
  if (value == 3) return "Instruction_Stack_Patch";
  if (value == 4) return "Instruction_Location";
  return "<unknown value>";}

typedef struct Instruction_Label {
  Label * pointer;
} Instruction_Label;
typedef struct Instruction_Bytes {
  u8 memory[15];
  u8 length;
} Instruction_Bytes;
typedef struct Instruction_Label_Patch {
  s32 offset_in_instruction;
  s32 offset_from_label;
  Label * label;
} Instruction_Label_Patch;
typedef struct Instruction_Stack_Patch {
  s32 mod_r_m_offset_in_previous_instruction;
  Stack_Area stack_area;
} Instruction_Stack_Patch;
typedef struct Instruction_Location {
  Source_Range source_range;
} Instruction_Location;
typedef struct Instruction {
  Instruction_Tag tag;
  char _tag_padding[4];
  const Scope * scope;
  union {
    Instruction_Label Label;
    Instruction_Bytes Bytes;
    Instruction_Label_Patch Label_Patch;
    Instruction_Stack_Patch Stack_Patch;
    Instruction_Location Location;
  };
} Instruction;
static inline const Instruction_Label *
instruction_as_label(const Instruction *instruction) {
  assert(instruction->tag == Instruction_Tag_Label);
  return &instruction->Label;
}
static inline const Instruction_Bytes *
instruction_as_bytes(const Instruction *instruction) {
  assert(instruction->tag == Instruction_Tag_Bytes);
  return &instruction->Bytes;
}
static inline const Instruction_Label_Patch *
instruction_as_label_patch(const Instruction *instruction) {
  assert(instruction->tag == Instruction_Tag_Label_Patch);
  return &instruction->Label_Patch;
}
static inline const Instruction_Stack_Patch *
instruction_as_stack_patch(const Instruction *instruction) {
  assert(instruction->tag == Instruction_Tag_Stack_Patch);
  return &instruction->Stack_Patch;
}
static inline const Instruction_Location *
instruction_as_location(const Instruction *instruction) {
  assert(instruction->tag == Instruction_Tag_Location);
  return &instruction->Location;
}
typedef dyn_array_type(Instruction) Array_Instruction;
typedef struct Instruction_Bucket {
  Instruction items[15];
  u64 length;
  Instruction_Bucket * next;
} Instruction_Bucket;
typedef dyn_array_type(Instruction_Bucket) Array_Instruction_Bucket;

typedef struct Code_Block {
  const Allocator * allocator;
  Label * start_label;
  Label * end_label;
  Instruction_Bucket * first_bucket;
  Instruction_Bucket * last_bucket;
} Code_Block;
typedef dyn_array_type(Code_Block) Array_Code_Block;

typedef struct Epoch {
  u64 as_u64;
} Epoch;
typedef dyn_array_type(Epoch) Array_Epoch;

typedef struct Function_Layout {
  s32 stack_reserve;
  u8 stack_allocation_offset_in_prolog;
  u8 size_of_prolog;
  u16 _padding;
  u32 begin_rva;
  u32 end_rva;
  u8 volatile_register_push_offsets[16];
} Function_Layout;
typedef dyn_array_type(Function_Layout) Array_Function_Layout;

typedef struct Mass_Context {
  Allocator * allocator;
  Allocator * temp_allocator;
  Compilation * compilation;
  Program * program;
  Mass_Result * result;
} Mass_Context;
typedef dyn_array_type(Mass_Context) Array_Mass_Context;

typedef struct Parser {
  Parser_Flags flags;
  s32 _flags_padding;
  Epoch epoch;
  Scope * scope;
  Module * module;
  const Descriptor * * return_descriptor_pointer;
} Parser;
typedef dyn_array_type(Parser) Array_Parser;

typedef enum {
  Operator_Tag_Alias = 0,
  Operator_Tag_Intrinsic = 1,
} Operator_Tag;

const char *operator_tag_name(Operator_Tag value) {
  if (value == 0) return "Operator_Alias";
  if (value == 1) return "Operator_Intrinsic";
  return "<unknown value>";}

typedef struct Operator_Alias {
  const Symbol * symbol;
} Operator_Alias;
typedef struct Operator_Intrinsic {
  Value * body;
} Operator_Intrinsic;
typedef struct Operator {
  Operator_Tag tag;
  char _tag_padding[4];
  Operator_Flags flags;
  Operator_Fixity fixity;
  Operator_Associativity associativity;
  u32 precedence;
  union {
    Operator_Alias Alias;
    Operator_Intrinsic Intrinsic;
  };
} Operator;
static inline const Operator_Alias *
operator_as_alias(const Operator *operator) {
  assert(operator->tag == Operator_Tag_Alias);
  return &operator->Alias;
}
static inline const Operator_Intrinsic *
operator_as_intrinsic(const Operator *operator) {
  assert(operator->tag == Operator_Tag_Intrinsic);
  return &operator->Intrinsic;
}
typedef dyn_array_type(Operator) Array_Operator;
typedef struct Scope_Entry {
  Value * value;
  Slice name;
  Epoch epoch;
  const Value * latest_forced_value;
  Source_Range source_range;
} Scope_Entry;
typedef dyn_array_type(Scope_Entry) Array_Scope_Entry;

hash_map_template(Operator_Map, const Symbol *, Operator *, hash_pointer, const_void_pointer_equal)
hash_map_template(Operator_Symbol_Map, const Symbol *, const Symbol *, hash_pointer, const_void_pointer_equal)
typedef enum {
  Scope_Tag_Imperative = 0,
  Scope_Tag_Declarative = 1,
} Scope_Tag;

const char *scope_tag_name(Scope_Tag value) {
  if (value == 0) return "Scope_Imperative";
  if (value == 1) return "Scope_Declarative";
  return "<unknown value>";}

typedef struct Scope_Imperative {
  Scope_Entry entry;
} Scope_Imperative;
typedef struct Scope_Declarative {
  Scope_Map * map;
} Scope_Declarative;
typedef struct Scope {
  Scope_Tag tag;
  char _tag_padding[4];
  const Allocator * allocator;
  const Scope * parent;
  union {
    Scope_Imperative Imperative;
    Scope_Declarative Declarative;
  };
} Scope;
static inline const Scope_Imperative *
scope_as_imperative(const Scope *scope) {
  assert(scope->tag == Scope_Tag_Imperative);
  return &scope->Imperative;
}
static inline const Scope_Declarative *
scope_as_declarative(const Scope *scope) {
  assert(scope->tag == Scope_Tag_Declarative);
  return &scope->Declarative;
}
typedef dyn_array_type(Scope) Array_Scope;
typedef struct Overload {
  Value * value;
  Value * next;
} Overload;
typedef dyn_array_type(Overload) Array_Overload;

typedef struct Undecidable_Match {
  const Function_Info * info;
  Value * value;
} Undecidable_Match;
typedef dyn_array_type(Undecidable_Match) Array_Undecidable_Match;

typedef enum {
  Overload_Match_Tag_No_Match = 0,
  Overload_Match_Tag_Undecidable = 1,
  Overload_Match_Tag_Found = 2,
} Overload_Match_Tag;

const char *overload_match_tag_name(Overload_Match_Tag value) {
  if (value == 0) return "Overload_Match_No_Match";
  if (value == 1) return "Overload_Match_Undecidable";
  if (value == 2) return "Overload_Match_Found";
  return "<unknown value>";}

typedef struct Overload_Match_Undecidable {
  Array_Undecidable_Match matches;
} Overload_Match_Undecidable;
typedef struct Overload_Match_Found {
  Value * value;
  const Function_Info * info;
} Overload_Match_Found;
typedef struct Overload_Match {
  Overload_Match_Tag tag;
  char _tag_padding[4];
  union {
    Overload_Match_Undecidable Undecidable;
    Overload_Match_Found Found;
  };
} Overload_Match;
static inline const Overload_Match_Undecidable *
overload_match_as_undecidable(const Overload_Match *overload_match) {
  assert(overload_match->tag == Overload_Match_Tag_Undecidable);
  return &overload_match->Undecidable;
}
static inline const Overload_Match_Found *
overload_match_as_found(const Overload_Match *overload_match) {
  assert(overload_match->tag == Overload_Match_Tag_Found);
  return &overload_match->Found;
}
typedef dyn_array_type(Overload_Match) Array_Overload_Match;
typedef struct Overload_Match_Summary {
  u16 inverted_generic_count;
  u16 inverted_cast_count;
  u16 exact_count;
  _Bool compile_time;
  _Bool matched;
} Overload_Match_Summary;
typedef dyn_array_type(Overload_Match_Summary) Array_Overload_Match_Summary;

typedef struct Overload_Match_State {
  Value * value;
  const Function_Info * info;
  Overload_Match_Summary summary;
} Overload_Match_State;
typedef dyn_array_type(Overload_Match_State) Array_Overload_Match_State;

typedef enum {
  Value_Tag_Lazy = 0,
  Value_Tag_Forced = 1,
} Value_Tag;

const char *value_tag_name(Value_Tag value) {
  if (value == 0) return "Value_Lazy";
  if (value == 1) return "Value_Forced";
  return "<unknown value>";}

typedef struct Value_Lazy {
  _Bool is_factory;
  u8 _is_factory_padding[7];
  Epoch epoch;
  const void * payload;
  const Scope * scope;
  Lazy_Value_Proc proc;
} Value_Lazy;
typedef struct Value_Forced {
  Storage storage;
} Value_Forced;
typedef struct Value {
  Value_Tag tag;
  char _tag_padding[4];
  Value_Flags flags;
  u32 _flags_padding;
  const Descriptor * descriptor;
  Source_Range source_range;
  union {
    Value_Lazy Lazy;
    Value_Forced Forced;
  };
} Value;
static inline const Value_Lazy *
value_as_lazy(const Value *value) {
  assert(value->tag == Value_Tag_Lazy);
  return &value->Lazy;
}
static inline const Value_Forced *
value_as_forced(const Value *value) {
  assert(value->tag == Value_Tag_Forced);
  return &value->Forced;
}
typedef dyn_array_type(Value) Array_Value;
typedef struct Register_Bitset {
  u64 bits;
} Register_Bitset;
typedef dyn_array_type(Register_Bitset) Array_Register_Bitset;

typedef struct Function_Builder {
  Epoch epoch;
  s32 stack_reserve;
  u32 max_call_parameters_stack_size;
  Value return_value;
  Code_Block code_block;
  Register_Bitset register_used_bitset;
  Register_Bitset register_volatile_bitset;
  Register_Bitset register_occupied_bitset;
  Slice source;
  const Function_Info * function;
} Function_Builder;
typedef dyn_array_type(Function_Builder) Array_Function_Builder;

typedef enum {
  Expected_Result_Tag_Exact = 0,
  Expected_Result_Tag_Flexible = 1,
} Expected_Result_Tag;

const char *expected_result_tag_name(Expected_Result_Tag value) {
  if (value == 0) return "Expected_Result_Exact";
  if (value == 1) return "Expected_Result_Flexible";
  return "<unknown value>";}

typedef struct Expected_Result_Exact {
  const Descriptor * descriptor;
  Storage storage;
} Expected_Result_Exact;
typedef struct Expected_Result_Flexible {
  const Descriptor * descriptor;
} Expected_Result_Flexible;
typedef struct Expected_Result {
  Expected_Result_Tag tag;
  char _tag_padding[4];
  union {
    Expected_Result_Exact Exact;
    Expected_Result_Flexible Flexible;
  };
} Expected_Result;
static inline const Expected_Result_Exact *
expected_result_as_exact(const Expected_Result *expected_result) {
  assert(expected_result->tag == Expected_Result_Tag_Exact);
  return &expected_result->Exact;
}
static inline const Expected_Result_Flexible *
expected_result_as_flexible(const Expected_Result *expected_result) {
  assert(expected_result->tag == Expected_Result_Tag_Flexible);
  return &expected_result->Flexible;
}
typedef dyn_array_type(Expected_Result) Array_Expected_Result;
typedef struct Lazy_Static_Value {
  Mass_Context context;
  Parser parser;
  Value_View expression;
  u64 resolving;
} Lazy_Static_Value;
typedef dyn_array_type(Lazy_Static_Value) Array_Lazy_Static_Value;

typedef enum {
  Function_Parameter_Tag_Runtime = 0,
  Function_Parameter_Tag_Generic = 1,
  Function_Parameter_Tag_Exact_Static = 2,
} Function_Parameter_Tag;

const char *function_parameter_tag_name(Function_Parameter_Tag value) {
  if (value == 0) return "Function_Parameter_Runtime";
  if (value == 1) return "Function_Parameter_Generic";
  if (value == 2) return "Function_Parameter_Exact_Static";
  return "<unknown value>";}

typedef struct Function_Parameter_Generic {
  u64 is_static;
  Mass_Type_Constraint_Proc maybe_type_constraint;
} Function_Parameter_Generic;
typedef struct Function_Parameter_Exact_Static {
  Storage storage;
} Function_Parameter_Exact_Static;
typedef struct Function_Parameter {
  Function_Parameter_Tag tag;
  char _tag_padding[4];
  const Descriptor * descriptor;
  const Symbol * symbol;
  Source_Range source_range;
  Value_View maybe_type_expression;
  Value * maybe_default_value;
  union {
    Function_Parameter_Generic Generic;
    Function_Parameter_Exact_Static Exact_Static;
  };
} Function_Parameter;
static inline const Function_Parameter_Generic *
function_parameter_as_generic(const Function_Parameter *function_parameter) {
  assert(function_parameter->tag == Function_Parameter_Tag_Generic);
  return &function_parameter->Generic;
}
static inline const Function_Parameter_Exact_Static *
function_parameter_as_exact_static(const Function_Parameter *function_parameter) {
  assert(function_parameter->tag == Function_Parameter_Tag_Exact_Static);
  return &function_parameter->Exact_Static;
}
typedef dyn_array_type(Function_Parameter) Array_Function_Parameter;
typedef enum {
  Resolved_Function_Parameter_Tag_Unknown = 0,
  Resolved_Function_Parameter_Tag_Known = 1,
} Resolved_Function_Parameter_Tag;

const char *resolved_function_parameter_tag_name(Resolved_Function_Parameter_Tag value) {
  if (value == 0) return "Resolved_Function_Parameter_Unknown";
  if (value == 1) return "Resolved_Function_Parameter_Known";
  return "<unknown value>";}

typedef struct Resolved_Function_Parameter_Known {
  Storage storage;
} Resolved_Function_Parameter_Known;
typedef struct Resolved_Function_Parameter {
  Resolved_Function_Parameter_Tag tag;
  char _tag_padding[4];
  u64 was_generic;
  const Descriptor * descriptor;
  const Symbol * symbol;
  Source_Range source_range;
  Value * maybe_default_value;
  union {
    Resolved_Function_Parameter_Known Known;
  };
} Resolved_Function_Parameter;
static inline const Resolved_Function_Parameter_Known *
resolved_function_parameter_as_known(const Resolved_Function_Parameter *resolved_function_parameter) {
  assert(resolved_function_parameter->tag == Resolved_Function_Parameter_Tag_Known);
  return &resolved_function_parameter->Known;
}
typedef dyn_array_type(Resolved_Function_Parameter) Array_Resolved_Function_Parameter;
typedef enum {
  Function_Return_Tag_Inferred = 0,
  Function_Return_Tag_Generic = 1,
  Function_Return_Tag_Exact = 2,
} Function_Return_Tag;

const char *function_return_tag_name(Function_Return_Tag value) {
  if (value == 0) return "Function_Return_Inferred";
  if (value == 1) return "Function_Return_Generic";
  if (value == 2) return "Function_Return_Exact";
  return "<unknown value>";}

typedef struct Function_Return_Generic {
  Value_View type_expression;
} Function_Return_Generic;
typedef struct Function_Return_Exact {
  const Descriptor * descriptor;
} Function_Return_Exact;
typedef struct Function_Return {
  Function_Return_Tag tag;
  char _tag_padding[4];
  Source_Range source_range;
  union {
    Function_Return_Generic Generic;
    Function_Return_Exact Exact;
  };
} Function_Return;
static inline const Function_Return_Generic *
function_return_as_generic(const Function_Return *function_return) {
  assert(function_return->tag == Function_Return_Tag_Generic);
  return &function_return->Generic;
}
static inline const Function_Return_Exact *
function_return_as_exact(const Function_Return *function_return) {
  assert(function_return->tag == Function_Return_Tag_Exact);
  return &function_return->Exact;
}
typedef dyn_array_type(Function_Return) Array_Function_Return;
typedef struct Function_Info {
  Function_Info_Flags flags;
  u32 _flags_padding;
  Array_Resolved_Function_Parameter parameters;
  const Descriptor * return_descriptor;
} Function_Info;
typedef dyn_array_type(Function_Info) Array_Function_Info;

typedef struct Function_Specialization {
  Array_Function_Parameter parameters;
  Function_Info * info;
} Function_Specialization;
typedef dyn_array_type(Function_Specialization) Array_Function_Specialization;

typedef struct Function_Header {
  Function_Header_Flags flags;
  u32 generic_parameter_count;
  Array_Function_Parameter parameters;
  Function_Return returns;
} Function_Header;
typedef dyn_array_type(Function_Header) Array_Function_Header;

typedef struct Function_Literal {
  Function_Header header;
  Scope * own_scope;
  Value * body;
  u64 * overload_lock_count;
  Array_Value_Ptr instances;
  Array_Function_Specialization specializations;
} Function_Literal;
typedef dyn_array_type(Function_Literal) Array_Function_Literal;

typedef struct Function_Call_Parameter {
  Function_Call_Parameter_Flags flags;
  u32 _flags_padding;
  const Descriptor * descriptor;
  Storage storage;
  u64 original_index;
} Function_Call_Parameter;
typedef dyn_array_type(Function_Call_Parameter) Array_Function_Call_Parameter;

typedef struct Function_Call_Setup {
  u32 parameters_stack_size;
  u32 _parameters_stack_size_padding;
  Mass_Call_Encode_Proc call_encode_proc;
  const Calling_Convention * calling_convention;
  Array_Function_Call_Parameter parameters;
  Register_Bitset parameter_registers_bitset;
  Storage caller_return;
  Storage callee_return;
} Function_Call_Setup;
typedef dyn_array_type(Function_Call_Setup) Array_Function_Call_Setup;

typedef struct System_V_Classification {
  SYSTEM_V_ARGUMENT_CLASS class;
  u32 _flags_padding;
  const Descriptor * descriptor;
  u64 eightbyte_count;
} System_V_Classification;
typedef dyn_array_type(System_V_Classification) Array_System_V_Classification;

typedef struct System_V_Registers {
  const Register * items;
  u32 count;
  u32 index;
} System_V_Registers;
typedef dyn_array_type(System_V_Registers) Array_System_V_Registers;

typedef struct System_V_Register_State {
  System_V_Registers general;
  System_V_Registers vector;
} System_V_Register_State;
typedef dyn_array_type(System_V_Register_State) Array_System_V_Register_State;

typedef struct System_V_Eightbyte_Array {
  SYSTEM_V_ARGUMENT_CLASS classes[8];
  u64 count;
} System_V_Eightbyte_Array;
typedef dyn_array_type(System_V_Eightbyte_Array) Array_System_V_Eightbyte_Array;

typedef struct Mass_Function_Call_Lazy_Payload {
  Value_View args;
  Value * overload;
  const Function_Info * info;
} Mass_Function_Call_Lazy_Payload;
typedef dyn_array_type(Mass_Function_Call_Lazy_Payload) Array_Mass_Function_Call_Lazy_Payload;

typedef struct Tuple {
  Epoch epoch;
  const Scope * scope_where_it_was_created;
  Array_Value_Ptr items;
} Tuple;
typedef dyn_array_type(Tuple) Array_Tuple;

typedef struct List_Node {
  const List_Node * maybe_previous;
  Value * value;
} List_Node;
typedef dyn_array_type(List_Node) Array_List_Node;

typedef struct Typed_Symbol {
  const Symbol * symbol;
  const Descriptor * descriptor;
} Typed_Symbol;
typedef dyn_array_type(Typed_Symbol) Array_Typed_Symbol;

typedef struct Struct_Field {
  Slice name;
  const Descriptor * descriptor;
  u64 offset;
} Struct_Field;
typedef dyn_array_type(Struct_Field) Array_Struct_Field;

typedef enum {
  Descriptor_Tag_Void = 0,
  Descriptor_Tag_Never = 1,
  Descriptor_Tag_Raw = 2,
  Descriptor_Tag_Float = 3,
  Descriptor_Tag_Integer = 4,
  Descriptor_Tag_Function_Instance = 5,
  Descriptor_Tag_Fixed_Array = 6,
  Descriptor_Tag_Struct = 7,
  Descriptor_Tag_Pointer_To = 8,
} Descriptor_Tag;

const char *descriptor_tag_name(Descriptor_Tag value) {
  if (value == 0) return "Descriptor_Void";
  if (value == 1) return "Descriptor_Never";
  if (value == 2) return "Descriptor_Raw";
  if (value == 3) return "Descriptor_Float";
  if (value == 4) return "Descriptor_Integer";
  if (value == 5) return "Descriptor_Function_Instance";
  if (value == 6) return "Descriptor_Fixed_Array";
  if (value == 7) return "Descriptor_Struct";
  if (value == 8) return "Descriptor_Pointer_To";
  return "<unknown value>";}

typedef struct Descriptor_Integer {
  u64 is_signed;
} Descriptor_Integer;
typedef struct Descriptor_Function_Instance {
  const Function_Info * info;
  Function_Call_Setup call_setup;
  const Program * program;
} Descriptor_Function_Instance;
typedef struct Descriptor_Fixed_Array {
  const Descriptor * item;
  u64 length;
} Descriptor_Fixed_Array;
typedef struct Descriptor_Struct {
  Array_Struct_Field fields;
} Descriptor_Struct;
typedef struct Descriptor_Pointer_To {
  const Descriptor * descriptor;
} Descriptor_Pointer_To;
typedef struct Descriptor {
  Descriptor_Tag tag;
  char _tag_padding[4];
  Module * own_module;
  const Symbol * brand;
  Bits bit_size;
  Bits bit_alignment;
  union {
    Descriptor_Integer Integer;
    Descriptor_Function_Instance Function_Instance;
    Descriptor_Fixed_Array Fixed_Array;
    Descriptor_Struct Struct;
    Descriptor_Pointer_To Pointer_To;
  };
} Descriptor;
static inline const Descriptor_Integer *
descriptor_as_integer(const Descriptor *descriptor) {
  assert(descriptor->tag == Descriptor_Tag_Integer);
  return &descriptor->Integer;
}
static inline const Descriptor_Function_Instance *
descriptor_as_function_instance(const Descriptor *descriptor) {
  assert(descriptor->tag == Descriptor_Tag_Function_Instance);
  return &descriptor->Function_Instance;
}
static inline const Descriptor_Fixed_Array *
descriptor_as_fixed_array(const Descriptor *descriptor) {
  assert(descriptor->tag == Descriptor_Tag_Fixed_Array);
  return &descriptor->Fixed_Array;
}
static inline const Descriptor_Struct *
descriptor_as_struct(const Descriptor *descriptor) {
  assert(descriptor->tag == Descriptor_Tag_Struct);
  return &descriptor->Struct;
}
static inline const Descriptor_Pointer_To *
descriptor_as_pointer_to(const Descriptor *descriptor) {
  assert(descriptor->tag == Descriptor_Tag_Pointer_To);
  return &descriptor->Pointer_To;
}
typedef dyn_array_type(Descriptor) Array_Descriptor;
typedef struct Type {
  const Descriptor * descriptor;
} Type;
typedef dyn_array_type(Type) Array_Type;

typedef enum {
  Mass_Error_Tag_Unimplemented = 0,
  Mass_Error_Tag_Unreachable_Statement = 1,
  Mass_Error_Tag_Parse = 2,
  Mass_Error_Tag_Assignment_To_Constant = 3,
  Mass_Error_Tag_User_Defined = 4,
  Mass_Error_Tag_Circular_Dependency = 5,
  Mass_Error_Tag_Non_Trailing_Default_Argument = 6,
  Mass_Error_Tag_Expected_Static = 7,
  Mass_Error_Tag_Tokenizer = 8,
  Mass_Error_Tag_Integer_Range = 9,
  Mass_Error_Tag_File_Open = 10,
  Mass_Error_Tag_File_Too_Large = 11,
  Mass_Error_Tag_Dynamic_Library_Load = 12,
  Mass_Error_Tag_Dynamic_Library_Symbol_Not_Found = 13,
  Mass_Error_Tag_Operator_Fixity_Conflict = 14,
  Mass_Error_Tag_Undefined_Variable = 15,
  Mass_Error_Tag_Redefinition = 16,
  Mass_Error_Tag_Unknown_Field = 17,
  Mass_Error_Tag_Invalid_Identifier = 18,
  Mass_Error_Tag_Type_Mismatch = 19,
  Mass_Error_Tag_Epoch_Mismatch = 20,
  Mass_Error_Tag_No_Matching_Overload = 21,
  Mass_Error_Tag_Undecidable_Overload = 22,
  Mass_Error_Tag_Non_Function_Overload = 23,
  Mass_Error_Tag_No_Runtime_Use = 24,
  Mass_Error_Tag_Recursive_Intrinsic_Use = 25,
} Mass_Error_Tag;

const char *mass_error_tag_name(Mass_Error_Tag value) {
  if (value == 0) return "Mass_Error_Unimplemented";
  if (value == 1) return "Mass_Error_Unreachable_Statement";
  if (value == 2) return "Mass_Error_Parse";
  if (value == 3) return "Mass_Error_Assignment_To_Constant";
  if (value == 4) return "Mass_Error_User_Defined";
  if (value == 5) return "Mass_Error_Circular_Dependency";
  if (value == 6) return "Mass_Error_Non_Trailing_Default_Argument";
  if (value == 7) return "Mass_Error_Expected_Static";
  if (value == 8) return "Mass_Error_Tokenizer";
  if (value == 9) return "Mass_Error_Integer_Range";
  if (value == 10) return "Mass_Error_File_Open";
  if (value == 11) return "Mass_Error_File_Too_Large";
  if (value == 12) return "Mass_Error_Dynamic_Library_Load";
  if (value == 13) return "Mass_Error_Dynamic_Library_Symbol_Not_Found";
  if (value == 14) return "Mass_Error_Operator_Fixity_Conflict";
  if (value == 15) return "Mass_Error_Undefined_Variable";
  if (value == 16) return "Mass_Error_Redefinition";
  if (value == 17) return "Mass_Error_Unknown_Field";
  if (value == 18) return "Mass_Error_Invalid_Identifier";
  if (value == 19) return "Mass_Error_Type_Mismatch";
  if (value == 20) return "Mass_Error_Epoch_Mismatch";
  if (value == 21) return "Mass_Error_No_Matching_Overload";
  if (value == 22) return "Mass_Error_Undecidable_Overload";
  if (value == 23) return "Mass_Error_Non_Function_Overload";
  if (value == 24) return "Mass_Error_No_Runtime_Use";
  if (value == 25) return "Mass_Error_Recursive_Intrinsic_Use";
  return "<unknown value>";}

typedef struct Mass_Error_User_Defined {
  Slice name;
} Mass_Error_User_Defined;
typedef struct Mass_Error_Circular_Dependency {
  Slice name;
} Mass_Error_Circular_Dependency;
typedef struct Mass_Error_Integer_Range {
  const Descriptor * descriptor;
} Mass_Error_Integer_Range;
typedef struct Mass_Error_File_Open {
  Slice path;
} Mass_Error_File_Open;
typedef struct Mass_Error_File_Too_Large {
  Slice path;
} Mass_Error_File_Too_Large;
typedef struct Mass_Error_Dynamic_Library_Load {
  Slice library_name;
} Mass_Error_Dynamic_Library_Load;
typedef struct Mass_Error_Dynamic_Library_Symbol_Not_Found {
  Slice library_name;
  Slice symbol_name;
} Mass_Error_Dynamic_Library_Symbol_Not_Found;
typedef struct Mass_Error_Operator_Fixity_Conflict {
  Operator_Fixity fixity;
  u32 _fixity_padding;
  Slice symbol;
} Mass_Error_Operator_Fixity_Conflict;
typedef struct Mass_Error_Undefined_Variable {
  Slice name;
  u64 is_operator;
} Mass_Error_Undefined_Variable;
typedef struct Mass_Error_Redefinition {
  Slice name;
} Mass_Error_Redefinition;
typedef struct Mass_Error_Unknown_Field {
  const Descriptor * type;
  Slice name;
} Mass_Error_Unknown_Field;
typedef struct Mass_Error_Invalid_Identifier {
  Value * id;
} Mass_Error_Invalid_Identifier;
typedef struct Mass_Error_Type_Mismatch {
  const Descriptor * expected;
  const Descriptor * actual;
} Mass_Error_Type_Mismatch;
typedef struct Mass_Error_No_Matching_Overload {
  Value * target;
  Array_Resolved_Function_Parameter arguments;
} Mass_Error_No_Matching_Overload;
typedef struct Mass_Error_Undecidable_Overload {
  Array_Undecidable_Match matches;
} Mass_Error_Undecidable_Overload;
typedef struct Mass_Error {
  Mass_Error_Tag tag;
  char _tag_padding[4];
  Slice detailed_message;
  Source_Range source_range;
  Source_Range other_source_range;
  union {
    Mass_Error_User_Defined User_Defined;
    Mass_Error_Circular_Dependency Circular_Dependency;
    Mass_Error_Integer_Range Integer_Range;
    Mass_Error_File_Open File_Open;
    Mass_Error_File_Too_Large File_Too_Large;
    Mass_Error_Dynamic_Library_Load Dynamic_Library_Load;
    Mass_Error_Dynamic_Library_Symbol_Not_Found Dynamic_Library_Symbol_Not_Found;
    Mass_Error_Operator_Fixity_Conflict Operator_Fixity_Conflict;
    Mass_Error_Undefined_Variable Undefined_Variable;
    Mass_Error_Redefinition Redefinition;
    Mass_Error_Unknown_Field Unknown_Field;
    Mass_Error_Invalid_Identifier Invalid_Identifier;
    Mass_Error_Type_Mismatch Type_Mismatch;
    Mass_Error_No_Matching_Overload No_Matching_Overload;
    Mass_Error_Undecidable_Overload Undecidable_Overload;
  };
} Mass_Error;
static inline const Mass_Error_User_Defined *
mass_error_as_user_defined(const Mass_Error *mass_error) {
  assert(mass_error->tag == Mass_Error_Tag_User_Defined);
  return &mass_error->User_Defined;
}
static inline const Mass_Error_Circular_Dependency *
mass_error_as_circular_dependency(const Mass_Error *mass_error) {
  assert(mass_error->tag == Mass_Error_Tag_Circular_Dependency);
  return &mass_error->Circular_Dependency;
}
static inline const Mass_Error_Integer_Range *
mass_error_as_integer_range(const Mass_Error *mass_error) {
  assert(mass_error->tag == Mass_Error_Tag_Integer_Range);
  return &mass_error->Integer_Range;
}
static inline const Mass_Error_File_Open *
mass_error_as_file_open(const Mass_Error *mass_error) {
  assert(mass_error->tag == Mass_Error_Tag_File_Open);
  return &mass_error->File_Open;
}
static inline const Mass_Error_File_Too_Large *
mass_error_as_file_too_large(const Mass_Error *mass_error) {
  assert(mass_error->tag == Mass_Error_Tag_File_Too_Large);
  return &mass_error->File_Too_Large;
}
static inline const Mass_Error_Dynamic_Library_Load *
mass_error_as_dynamic_library_load(const Mass_Error *mass_error) {
  assert(mass_error->tag == Mass_Error_Tag_Dynamic_Library_Load);
  return &mass_error->Dynamic_Library_Load;
}
static inline const Mass_Error_Dynamic_Library_Symbol_Not_Found *
mass_error_as_dynamic_library_symbol_not_found(const Mass_Error *mass_error) {
  assert(mass_error->tag == Mass_Error_Tag_Dynamic_Library_Symbol_Not_Found);
  return &mass_error->Dynamic_Library_Symbol_Not_Found;
}
static inline const Mass_Error_Operator_Fixity_Conflict *
mass_error_as_operator_fixity_conflict(const Mass_Error *mass_error) {
  assert(mass_error->tag == Mass_Error_Tag_Operator_Fixity_Conflict);
  return &mass_error->Operator_Fixity_Conflict;
}
static inline const Mass_Error_Undefined_Variable *
mass_error_as_undefined_variable(const Mass_Error *mass_error) {
  assert(mass_error->tag == Mass_Error_Tag_Undefined_Variable);
  return &mass_error->Undefined_Variable;
}
static inline const Mass_Error_Redefinition *
mass_error_as_redefinition(const Mass_Error *mass_error) {
  assert(mass_error->tag == Mass_Error_Tag_Redefinition);
  return &mass_error->Redefinition;
}
static inline const Mass_Error_Unknown_Field *
mass_error_as_unknown_field(const Mass_Error *mass_error) {
  assert(mass_error->tag == Mass_Error_Tag_Unknown_Field);
  return &mass_error->Unknown_Field;
}
static inline const Mass_Error_Invalid_Identifier *
mass_error_as_invalid_identifier(const Mass_Error *mass_error) {
  assert(mass_error->tag == Mass_Error_Tag_Invalid_Identifier);
  return &mass_error->Invalid_Identifier;
}
static inline const Mass_Error_Type_Mismatch *
mass_error_as_type_mismatch(const Mass_Error *mass_error) {
  assert(mass_error->tag == Mass_Error_Tag_Type_Mismatch);
  return &mass_error->Type_Mismatch;
}
static inline const Mass_Error_No_Matching_Overload *
mass_error_as_no_matching_overload(const Mass_Error *mass_error) {
  assert(mass_error->tag == Mass_Error_Tag_No_Matching_Overload);
  return &mass_error->No_Matching_Overload;
}
static inline const Mass_Error_Undecidable_Overload *
mass_error_as_undecidable_overload(const Mass_Error *mass_error) {
  assert(mass_error->tag == Mass_Error_Tag_Undecidable_Overload);
  return &mass_error->Undecidable_Overload;
}
typedef dyn_array_type(Mass_Error) Array_Mass_Error;
typedef enum {
  Mass_Result_Tag_Success = 0,
  Mass_Result_Tag_Error = 1,
} Mass_Result_Tag;

const char *mass_result_tag_name(Mass_Result_Tag value) {
  if (value == 0) return "Mass_Result_Success";
  if (value == 1) return "Mass_Result_Error";
  return "<unknown value>";}

typedef struct Mass_Result_Error {
  Mass_Error error;
} Mass_Result_Error;
typedef struct Mass_Result {
  Mass_Result_Tag tag;
  char _tag_padding[4];
  union {
    Mass_Result_Error Error;
  };
} Mass_Result;
static inline const Mass_Result_Error *
mass_result_as_error(const Mass_Result *mass_result) {
  assert(mass_result->tag == Mass_Result_Tag_Error);
  return &mass_result->Error;
}
typedef dyn_array_type(Mass_Result) Array_Mass_Result;
typedef struct Program {
  Array_Import_Library import_libraries;
  Array_Label_Location_Diff_Patch_Info patch_info_array;
  Array_Relocation relocations;
  Value * entry_point;
  Array_Function_Builder functions;
  Program_Memory memory;
  const Calling_Convention * default_calling_convention;
  Os os;
  u32 _os_padding;
} Program;
typedef dyn_array_type(Program) Array_Program;

typedef struct Calling_Convention {
  Register_Bitset register_volatile_bitset;
  Calling_Convention_Call_Setup_Proc call_setup_proc;
} Calling_Convention;
typedef dyn_array_type(Calling_Convention) Array_Calling_Convention;

typedef struct Mass_Trampoline {
  const Descriptor * args_descriptor;
  const Function_Info * original_info;
  Mass_Trampoline_Proc proc;
} Mass_Trampoline;
typedef dyn_array_type(Mass_Trampoline) Array_Mass_Trampoline;

hash_map_template(Struct_Field_Set, const Struct_Field *, u64, hash_pointer, const_void_pointer_equal)
hash_map_slice_template(Slice_Set, u64)
hash_map_slice_template(Symbol_Map, Symbol *)
hash_map_template(Trampoline_Map, const Function_Info *, const Mass_Trampoline *, hash_pointer, const_void_pointer_equal)
hash_map_template(Scope_Map, const Symbol *, Scope_Entry *, hash_pointer, const_void_pointer_equal)
hash_map_slice_template(Macro_Replacement_Map, Value_View)
hash_map_slice_template(Jit_Import_Library_Handle_Map, void *)
hash_map_slice_template(Imported_Module_Map, Module *)
typedef struct Jit_Counters {
  u64 functions;
  u64 imports;
  u64 relocations;
  u64 protected_ro_data_page_count;
} Jit_Counters;
typedef dyn_array_type(Jit_Counters) Array_Jit_Counters;

typedef struct Jit {
  u64 is_stack_unwinding_in_progress;
  Program * program;
  Jit_Import_Library_Handle_Map * import_library_handles;
  Jit_Counters previous_counts;
  void * platform_specific_payload;
} Jit;
typedef dyn_array_type(Jit) Array_Jit;

hash_map_template(Static_Pointer_Length_Map, const void *, u64, hash_pointer, const_void_pointer_equal)
hash_map_template(Descriptor_Pointer_To_Cache_Map, const Descriptor *, const Descriptor *, hash_pointer, const_void_pointer_equal)
hash_map_template(Intrinsic_Proc_Cache_Map, const Value *, Mass_Intrinsic_Proc, hash_pointer, const_void_pointer_equal)
typedef struct Common_Symbols {
  const Symbol * apply;
  const Symbol * fn;
  const Symbol * get;
  const Symbol * intrinsic;
  const Symbol * label;
  const Symbol * postfix_block;
  const Symbol * statement;
  const Symbol * _if;
  const Symbol * then;
  const Symbol * _while;
  const Symbol * _else;
  const Symbol * _;
  const Symbol * operator_arrow;
  const Symbol * operator_at;
  const Symbol * operator_colon;
  const Symbol * operator_colon_equal;
  const Symbol * operator_double_colon;
  const Symbol * operator_comma;
  const Symbol * operator_dot;
  const Symbol * operator_dot_star;
  const Symbol * operator_equal;
  const Symbol * operator_fat_arrow;
  const Symbol * operator_space;
  const Symbol * operator_tilde;
  const Symbol * operator_dot_dot_dot;
} Common_Symbols;
typedef dyn_array_type(Common_Symbols) Array_Common_Symbols;

typedef struct Compilation {
  Virtual_Memory_Buffer temp_buffer;
  Allocator * temp_allocator;
  Virtual_Memory_Buffer allocation_buffer;
  Allocator * allocator;
  Jit jit;
  Module compiler_module;
  Static_Pointer_Length_Map * static_pointer_length_map;
  Imported_Module_Map * module_map;
  Trampoline_Map * trampoline_map;
  Scope * root_scope;
  Program * runtime_program;
  Mass_Result * result;
  Symbol_Map * symbol_cache_map;
  Operator_Symbol_Map * prefix_operator_symbol_map;
  Operator_Symbol_Map * infix_or_suffix_operator_symbol_map;
  Descriptor_Pointer_To_Cache_Map * descriptor_pointer_to_cache_map;
  Intrinsic_Proc_Cache_Map * intrinsic_proc_cache_map;
  Common_Symbols common_symbols;
  Operator apply_operator;
} Compilation;
typedef dyn_array_type(Compilation) Array_Compilation;

typedef struct Operand_Encoding {
  Operand_Encoding_Type type;
  u32 bit_size;
} Operand_Encoding;
typedef dyn_array_type(Operand_Encoding) Array_Operand_Encoding;

typedef struct Instruction_Encoding {
  u8 op_code[4];
  Instruction_Extension_Type extension_type;
  u8 op_code_extension;
  u8 _op_code_extension_padding[3];
  Operand_Encoding operands[3];
} Instruction_Encoding;
typedef dyn_array_type(Instruction_Encoding) Array_Instruction_Encoding;

typedef struct X64_Mnemonic {
  const char * name;
  const Instruction_Encoding * encoding_list;
  u64 encoding_count;
} X64_Mnemonic;
typedef dyn_array_type(X64_Mnemonic) Array_X64_Mnemonic;

typedef dyn_array_type(_Bool) Array__Bool;

typedef dyn_array_type(char) Array_char;

typedef dyn_array_type(int) Array_int;

typedef dyn_array_type(Allocator) Array_Allocator;

typedef dyn_array_type(Virtual_Memory_Buffer) Array_Virtual_Memory_Buffer;

typedef dyn_array_type(i8) Array_i8;

typedef dyn_array_type(i16) Array_i16;

typedef dyn_array_type(i32) Array_i32;

typedef dyn_array_type(i64) Array_i64;

_Pragma("warning (pop)")

// Mass Type Reflection

static Descriptor descriptor_void;
static Descriptor descriptor_void_pointer;
static Descriptor descriptor_never;
static Descriptor descriptor_never_pointer;
static Descriptor descriptor_descriptor;
static Descriptor descriptor_descriptor_pointer;
static Descriptor descriptor_type;
static Descriptor descriptor_type_pointer;
static Descriptor descriptor_bits;
static Descriptor descriptor_array_bits;
static Descriptor descriptor_array_bits_ptr;
static Descriptor descriptor_bits_pointer;
static Descriptor descriptor_bits_pointer_pointer;
static Descriptor descriptor_source_position;
static Descriptor descriptor_array_source_position;
static Descriptor descriptor_array_source_position_ptr;
static Descriptor descriptor_source_position_pointer;
static Descriptor descriptor_source_position_pointer_pointer;
static Descriptor descriptor_source_file;
static Descriptor descriptor_array_source_file;
static Descriptor descriptor_array_source_file_ptr;
static Descriptor descriptor_source_file_pointer;
static Descriptor descriptor_source_file_pointer_pointer;
static Descriptor descriptor_source_range;
static Descriptor descriptor_array_source_range;
static Descriptor descriptor_array_source_range_ptr;
static Descriptor descriptor_source_range_pointer;
static Descriptor descriptor_source_range_pointer_pointer;
static Descriptor descriptor_mass_while;
static Descriptor descriptor_array_mass_while;
static Descriptor descriptor_array_mass_while_ptr;
static Descriptor descriptor_mass_while_pointer;
static Descriptor descriptor_mass_while_pointer_pointer;
static Descriptor descriptor_assignment;
static Descriptor descriptor_array_assignment;
static Descriptor descriptor_array_assignment_ptr;
static Descriptor descriptor_assignment_pointer;
static Descriptor descriptor_assignment_pointer_pointer;
static Descriptor descriptor_module_exports;
static Descriptor descriptor_array_module_exports;
static Descriptor descriptor_array_module_exports_ptr;
static Descriptor descriptor_array_const_module_exports_ptr;
static Descriptor descriptor_module_exports_pointer;
static Descriptor descriptor_module_exports_pointer_pointer;
static Descriptor descriptor_module;
static Descriptor descriptor_array_module;
static Descriptor descriptor_array_module_ptr;
static Descriptor descriptor_module_pointer;
static Descriptor descriptor_module_pointer_pointer;
static Descriptor descriptor_parse_error;
static Descriptor descriptor_array_parse_error;
static Descriptor descriptor_array_parse_error_ptr;
static Descriptor descriptor_parse_error_pointer;
static Descriptor descriptor_parse_error_pointer_pointer;
static Descriptor descriptor_value_view;
static Descriptor descriptor_array_value_view;
static Descriptor descriptor_array_value_view_ptr;
static Descriptor descriptor_value_view_pointer;
static Descriptor descriptor_value_view_pointer_pointer;
static Descriptor descriptor_symbol;
static Descriptor descriptor_array_symbol;
static Descriptor descriptor_array_symbol_ptr;
static Descriptor descriptor_symbol_pointer;
static Descriptor descriptor_symbol_pointer_pointer;
static Descriptor descriptor_group_paren;
static Descriptor descriptor_array_group_paren;
static Descriptor descriptor_array_group_paren_ptr;
static Descriptor descriptor_group_paren_pointer;
static Descriptor descriptor_group_paren_pointer_pointer;
static Descriptor descriptor_ast_return;
static Descriptor descriptor_array_ast_return;
static Descriptor descriptor_array_ast_return_ptr;
static Descriptor descriptor_ast_return_pointer;
static Descriptor descriptor_ast_return_pointer_pointer;
static Descriptor descriptor_ast_statement;
static Descriptor descriptor_array_ast_statement;
static Descriptor descriptor_array_ast_statement_ptr;
static Descriptor descriptor_ast_statement_pointer;
static Descriptor descriptor_ast_statement_pointer_pointer;
static Descriptor descriptor_ast_block;
static Descriptor descriptor_array_ast_block;
static Descriptor descriptor_array_ast_block_ptr;
static Descriptor descriptor_ast_block_pointer;
static Descriptor descriptor_ast_block_pointer_pointer;
static Descriptor descriptor_group_square;
static Descriptor descriptor_array_group_square;
static Descriptor descriptor_array_group_square_ptr;
static Descriptor descriptor_group_square_pointer;
static Descriptor descriptor_group_square_pointer_pointer;
static Descriptor descriptor_ast_using;
static Descriptor descriptor_array_ast_using;
static Descriptor descriptor_array_ast_using_ptr;
static Descriptor descriptor_ast_using_pointer;
static Descriptor descriptor_ast_using_pointer_pointer;
static Descriptor descriptor_section_permissions;
static Descriptor descriptor_array_section_permissions;
static Descriptor descriptor_array_section_permissions_ptr;
static Descriptor descriptor_array_const_section_permissions_ptr;
static Descriptor descriptor_section_permissions_pointer;
static Descriptor descriptor_section_permissions_pointer_pointer;
static Descriptor descriptor_section;
static Descriptor descriptor_array_section;
static Descriptor descriptor_array_section_ptr;
static Descriptor descriptor_section_pointer;
static Descriptor descriptor_section_pointer_pointer;
static Descriptor descriptor_program_memory;
static Descriptor descriptor_array_program_memory;
static Descriptor descriptor_array_program_memory_ptr;
static Descriptor descriptor_program_memory_pointer;
static Descriptor descriptor_program_memory_pointer_pointer;
static Descriptor descriptor_register;
static Descriptor descriptor_array_register;
static Descriptor descriptor_array_register_ptr;
static Descriptor descriptor_array_const_register_ptr;
static Descriptor descriptor_register_pointer;
static Descriptor descriptor_register_pointer_pointer;
static Descriptor descriptor_label;
static Descriptor descriptor_array_label;
static Descriptor descriptor_array_label_ptr;
static Descriptor descriptor_label_pointer;
static Descriptor descriptor_label_pointer_pointer;
static Descriptor descriptor_label_location_diff_patch_info;
static Descriptor descriptor_array_label_location_diff_patch_info;
static Descriptor descriptor_array_label_location_diff_patch_info_ptr;
static Descriptor descriptor_label_location_diff_patch_info_pointer;
static Descriptor descriptor_label_location_diff_patch_info_pointer_pointer;
static Descriptor descriptor_number_base;
static Descriptor descriptor_array_number_base;
static Descriptor descriptor_array_number_base_ptr;
static Descriptor descriptor_array_const_number_base_ptr;
static Descriptor descriptor_number_base_pointer;
static Descriptor descriptor_number_base_pointer_pointer;
static Descriptor descriptor_quoted;
static Descriptor descriptor_array_quoted;
static Descriptor descriptor_array_quoted_ptr;
static Descriptor descriptor_quoted_pointer;
static Descriptor descriptor_quoted_pointer_pointer;
static Descriptor descriptor_spread;
static Descriptor descriptor_array_spread;
static Descriptor descriptor_array_spread_ptr;
static Descriptor descriptor_spread_pointer;
static Descriptor descriptor_spread_pointer_pointer;
static Descriptor descriptor_named_accessor;
static Descriptor descriptor_array_named_accessor;
static Descriptor descriptor_array_named_accessor_ptr;
static Descriptor descriptor_named_accessor_pointer;
static Descriptor descriptor_named_accessor_pointer_pointer;
static Descriptor descriptor_external_symbol;
static Descriptor descriptor_array_external_symbol;
static Descriptor descriptor_array_external_symbol_ptr;
static Descriptor descriptor_external_symbol_pointer;
static Descriptor descriptor_external_symbol_pointer_pointer;
static Descriptor descriptor_import_symbol;
static Descriptor descriptor_array_import_symbol;
static Descriptor descriptor_array_import_symbol_ptr;
static Descriptor descriptor_import_symbol_pointer;
static Descriptor descriptor_import_symbol_pointer_pointer;
static Descriptor descriptor_import_library;
static Descriptor descriptor_array_import_library;
static Descriptor descriptor_array_import_library_ptr;
static Descriptor descriptor_import_library_pointer;
static Descriptor descriptor_import_library_pointer_pointer;
static Descriptor descriptor_compare_type;
static Descriptor descriptor_array_compare_type;
static Descriptor descriptor_array_compare_type_ptr;
static Descriptor descriptor_array_const_compare_type_ptr;
static Descriptor descriptor_compare_type_pointer;
static Descriptor descriptor_compare_type_pointer_pointer;
static Descriptor descriptor_stack_area;
static Descriptor descriptor_array_stack_area;
static Descriptor descriptor_array_stack_area_ptr;
static Descriptor descriptor_array_const_stack_area_ptr;
static Descriptor descriptor_stack_area_pointer;
static Descriptor descriptor_stack_area_pointer_pointer;
static Descriptor descriptor_memory_location;
static Descriptor descriptor_array_memory_location;
static Descriptor descriptor_array_memory_location_ptr;
static Descriptor descriptor_array_const_memory_location_ptr;
static Descriptor descriptor_memory_location_pointer;
static Descriptor descriptor_memory_location_pointer_pointer;
static Descriptor descriptor_storage_flags;
static Descriptor descriptor_array_storage_flags;
static Descriptor descriptor_array_storage_flags_ptr;
static Descriptor descriptor_array_const_storage_flags_ptr;
static Descriptor descriptor_storage_flags_pointer;
static Descriptor descriptor_storage_flags_pointer_pointer;
static Descriptor descriptor_storage;
static Descriptor descriptor_array_storage;
static Descriptor descriptor_array_storage_ptr;
static Descriptor descriptor_array_const_storage_ptr;
static Descriptor descriptor_storage_pointer;
static Descriptor descriptor_storage_pointer_pointer;
static Descriptor descriptor_relocation;
static Descriptor descriptor_array_relocation;
static Descriptor descriptor_array_relocation_ptr;
static Descriptor descriptor_relocation_pointer;
static Descriptor descriptor_relocation_pointer_pointer;
static Descriptor descriptor_instruction_assembly;
static Descriptor descriptor_array_instruction_assembly;
static Descriptor descriptor_array_instruction_assembly_ptr;
static Descriptor descriptor_instruction_assembly_pointer;
static Descriptor descriptor_instruction_assembly_pointer_pointer;
static Descriptor descriptor_instruction;
static Descriptor descriptor_array_instruction;
static Descriptor descriptor_array_instruction_ptr;
static Descriptor descriptor_array_const_instruction_ptr;
static Descriptor descriptor_instruction_pointer;
static Descriptor descriptor_instruction_pointer_pointer;
static Descriptor descriptor_instruction_bucket;
static Descriptor descriptor_array_instruction_bucket;
static Descriptor descriptor_array_instruction_bucket_ptr;
static Descriptor descriptor_instruction_bucket_pointer;
static Descriptor descriptor_instruction_bucket_pointer_pointer;
static Descriptor descriptor_code_block;
static Descriptor descriptor_array_code_block;
static Descriptor descriptor_array_code_block_ptr;
static Descriptor descriptor_code_block_pointer;
static Descriptor descriptor_code_block_pointer_pointer;
static Descriptor descriptor_epoch;
static Descriptor descriptor_array_epoch;
static Descriptor descriptor_array_epoch_ptr;
static Descriptor descriptor_epoch_pointer;
static Descriptor descriptor_epoch_pointer_pointer;
static Descriptor descriptor_function_layout;
static Descriptor descriptor_array_function_layout;
static Descriptor descriptor_array_function_layout_ptr;
static Descriptor descriptor_function_layout_pointer;
static Descriptor descriptor_function_layout_pointer_pointer;
static Descriptor descriptor_mass_context;
static Descriptor descriptor_array_mass_context;
static Descriptor descriptor_array_mass_context_ptr;
static Descriptor descriptor_mass_context_pointer;
static Descriptor descriptor_mass_context_pointer_pointer;
static Descriptor descriptor_parser_flags;
static Descriptor descriptor_array_parser_flags;
static Descriptor descriptor_array_parser_flags_ptr;
static Descriptor descriptor_array_const_parser_flags_ptr;
static Descriptor descriptor_parser_flags_pointer;
static Descriptor descriptor_parser_flags_pointer_pointer;
static Descriptor descriptor_parser;
static Descriptor descriptor_array_parser;
static Descriptor descriptor_array_parser_ptr;
static Descriptor descriptor_parser_pointer;
static Descriptor descriptor_parser_pointer_pointer;
static Descriptor descriptor_operator_fixity;
static Descriptor descriptor_array_operator_fixity;
static Descriptor descriptor_array_operator_fixity_ptr;
static Descriptor descriptor_array_const_operator_fixity_ptr;
static Descriptor descriptor_operator_fixity_pointer;
static Descriptor descriptor_operator_fixity_pointer_pointer;
static Descriptor descriptor_operator_associativity;
static Descriptor descriptor_array_operator_associativity;
static Descriptor descriptor_array_operator_associativity_ptr;
static Descriptor descriptor_array_const_operator_associativity_ptr;
static Descriptor descriptor_operator_associativity_pointer;
static Descriptor descriptor_operator_associativity_pointer_pointer;
static Descriptor descriptor_operator_flags;
static Descriptor descriptor_array_operator_flags;
static Descriptor descriptor_array_operator_flags_ptr;
static Descriptor descriptor_array_const_operator_flags_ptr;
static Descriptor descriptor_operator_flags_pointer;
static Descriptor descriptor_operator_flags_pointer_pointer;
static Descriptor descriptor_operator;
static Descriptor descriptor_array_operator;
static Descriptor descriptor_array_operator_ptr;
static Descriptor descriptor_array_const_operator_ptr;
static Descriptor descriptor_operator_pointer;
static Descriptor descriptor_operator_pointer_pointer;
static Descriptor descriptor_scope_entry;
static Descriptor descriptor_array_scope_entry;
static Descriptor descriptor_array_scope_entry_ptr;
static Descriptor descriptor_scope_entry_pointer;
static Descriptor descriptor_scope_entry_pointer_pointer;
MASS_DEFINE_OPAQUE_C_TYPE(operator_map, Operator_Map);
MASS_DEFINE_OPAQUE_C_TYPE(operator_symbol_map, Operator_Symbol_Map);
static Descriptor descriptor_scope;
static Descriptor descriptor_array_scope;
static Descriptor descriptor_array_scope_ptr;
static Descriptor descriptor_array_const_scope_ptr;
static Descriptor descriptor_scope_pointer;
static Descriptor descriptor_scope_pointer_pointer;
static Descriptor descriptor_overload;
static Descriptor descriptor_array_overload;
static Descriptor descriptor_array_overload_ptr;
static Descriptor descriptor_overload_pointer;
static Descriptor descriptor_overload_pointer_pointer;
static Descriptor descriptor_undecidable_match;
static Descriptor descriptor_array_undecidable_match;
static Descriptor descriptor_array_undecidable_match_ptr;
static Descriptor descriptor_undecidable_match_pointer;
static Descriptor descriptor_undecidable_match_pointer_pointer;
static Descriptor descriptor_overload_match;
static Descriptor descriptor_array_overload_match;
static Descriptor descriptor_array_overload_match_ptr;
static Descriptor descriptor_array_const_overload_match_ptr;
static Descriptor descriptor_overload_match_pointer;
static Descriptor descriptor_overload_match_pointer_pointer;
static Descriptor descriptor_overload_match_summary;
static Descriptor descriptor_array_overload_match_summary;
static Descriptor descriptor_array_overload_match_summary_ptr;
static Descriptor descriptor_overload_match_summary_pointer;
static Descriptor descriptor_overload_match_summary_pointer_pointer;
static Descriptor descriptor_overload_match_state;
static Descriptor descriptor_array_overload_match_state;
static Descriptor descriptor_array_overload_match_state_ptr;
static Descriptor descriptor_overload_match_state_pointer;
static Descriptor descriptor_overload_match_state_pointer_pointer;
static Descriptor descriptor_value_flags;
static Descriptor descriptor_array_value_flags;
static Descriptor descriptor_array_value_flags_ptr;
static Descriptor descriptor_array_const_value_flags_ptr;
static Descriptor descriptor_value_flags_pointer;
static Descriptor descriptor_value_flags_pointer_pointer;
static Descriptor descriptor_value;
static Descriptor descriptor_array_value;
static Descriptor descriptor_array_value_ptr;
static Descriptor descriptor_array_const_value_ptr;
static Descriptor descriptor_value_pointer;
static Descriptor descriptor_value_pointer_pointer;
static Descriptor descriptor_register_bitset;
static Descriptor descriptor_array_register_bitset;
static Descriptor descriptor_array_register_bitset_ptr;
static Descriptor descriptor_register_bitset_pointer;
static Descriptor descriptor_register_bitset_pointer_pointer;
static Descriptor descriptor_function_builder;
static Descriptor descriptor_array_function_builder;
static Descriptor descriptor_array_function_builder_ptr;
static Descriptor descriptor_function_builder_pointer;
static Descriptor descriptor_function_builder_pointer_pointer;
static Descriptor descriptor_expected_result;
static Descriptor descriptor_array_expected_result;
static Descriptor descriptor_array_expected_result_ptr;
static Descriptor descriptor_array_const_expected_result_ptr;
static Descriptor descriptor_expected_result_pointer;
static Descriptor descriptor_expected_result_pointer_pointer;
static Descriptor descriptor_lazy_static_value;
static Descriptor descriptor_array_lazy_static_value;
static Descriptor descriptor_array_lazy_static_value_ptr;
static Descriptor descriptor_lazy_static_value_pointer;
static Descriptor descriptor_lazy_static_value_pointer_pointer;
static Descriptor descriptor_mass_intrinsic_proc;
static Descriptor descriptor_function_parameter;
static Descriptor descriptor_array_function_parameter;
static Descriptor descriptor_array_function_parameter_ptr;
static Descriptor descriptor_array_const_function_parameter_ptr;
static Descriptor descriptor_function_parameter_pointer;
static Descriptor descriptor_function_parameter_pointer_pointer;
static Descriptor descriptor_resolved_function_parameter;
static Descriptor descriptor_array_resolved_function_parameter;
static Descriptor descriptor_array_resolved_function_parameter_ptr;
static Descriptor descriptor_array_const_resolved_function_parameter_ptr;
static Descriptor descriptor_resolved_function_parameter_pointer;
static Descriptor descriptor_resolved_function_parameter_pointer_pointer;
static Descriptor descriptor_function_info_flags;
static Descriptor descriptor_array_function_info_flags;
static Descriptor descriptor_array_function_info_flags_ptr;
static Descriptor descriptor_array_const_function_info_flags_ptr;
static Descriptor descriptor_function_info_flags_pointer;
static Descriptor descriptor_function_info_flags_pointer_pointer;
static Descriptor descriptor_function_return;
static Descriptor descriptor_array_function_return;
static Descriptor descriptor_array_function_return_ptr;
static Descriptor descriptor_array_const_function_return_ptr;
static Descriptor descriptor_function_return_pointer;
static Descriptor descriptor_function_return_pointer_pointer;
static Descriptor descriptor_function_info;
static Descriptor descriptor_array_function_info;
static Descriptor descriptor_array_function_info_ptr;
static Descriptor descriptor_function_info_pointer;
static Descriptor descriptor_function_info_pointer_pointer;
static Descriptor descriptor_function_header_flags;
static Descriptor descriptor_array_function_header_flags;
static Descriptor descriptor_array_function_header_flags_ptr;
static Descriptor descriptor_array_const_function_header_flags_ptr;
static Descriptor descriptor_function_header_flags_pointer;
static Descriptor descriptor_function_header_flags_pointer_pointer;
static Descriptor descriptor_function_specialization;
static Descriptor descriptor_array_function_specialization;
static Descriptor descriptor_array_function_specialization_ptr;
static Descriptor descriptor_function_specialization_pointer;
static Descriptor descriptor_function_specialization_pointer_pointer;
static Descriptor descriptor_function_header;
static Descriptor descriptor_array_function_header;
static Descriptor descriptor_array_function_header_ptr;
static Descriptor descriptor_function_header_pointer;
static Descriptor descriptor_function_header_pointer_pointer;
static Descriptor descriptor_function_literal;
static Descriptor descriptor_array_function_literal;
static Descriptor descriptor_array_function_literal_ptr;
static Descriptor descriptor_function_literal_pointer;
static Descriptor descriptor_function_literal_pointer_pointer;
static Descriptor descriptor_function_call_parameter_flags;
static Descriptor descriptor_array_function_call_parameter_flags;
static Descriptor descriptor_array_function_call_parameter_flags_ptr;
static Descriptor descriptor_array_const_function_call_parameter_flags_ptr;
static Descriptor descriptor_function_call_parameter_flags_pointer;
static Descriptor descriptor_function_call_parameter_flags_pointer_pointer;
static Descriptor descriptor_function_call_parameter;
static Descriptor descriptor_array_function_call_parameter;
static Descriptor descriptor_array_function_call_parameter_ptr;
static Descriptor descriptor_function_call_parameter_pointer;
static Descriptor descriptor_function_call_parameter_pointer_pointer;
static Descriptor descriptor_mass_call_encode_proc;
static Descriptor descriptor_function_call_setup;
static Descriptor descriptor_array_function_call_setup;
static Descriptor descriptor_array_function_call_setup_ptr;
static Descriptor descriptor_function_call_setup_pointer;
static Descriptor descriptor_function_call_setup_pointer_pointer;
static Descriptor descriptor_system_v_argument_class;
static Descriptor descriptor_array_system_v_argument_class;
static Descriptor descriptor_array_system_v_argument_class_ptr;
static Descriptor descriptor_array_const_system_v_argument_class_ptr;
static Descriptor descriptor_system_v_argument_class_pointer;
static Descriptor descriptor_system_v_argument_class_pointer_pointer;
static Descriptor descriptor_system_v_classification;
static Descriptor descriptor_array_system_v_classification;
static Descriptor descriptor_array_system_v_classification_ptr;
static Descriptor descriptor_system_v_classification_pointer;
static Descriptor descriptor_system_v_classification_pointer_pointer;
static Descriptor descriptor_system_v_registers;
static Descriptor descriptor_array_system_v_registers;
static Descriptor descriptor_array_system_v_registers_ptr;
static Descriptor descriptor_system_v_registers_pointer;
static Descriptor descriptor_system_v_registers_pointer_pointer;
static Descriptor descriptor_system_v_register_state;
static Descriptor descriptor_array_system_v_register_state;
static Descriptor descriptor_array_system_v_register_state_ptr;
static Descriptor descriptor_system_v_register_state_pointer;
static Descriptor descriptor_system_v_register_state_pointer_pointer;
static Descriptor descriptor_system_v_eightbyte_array;
static Descriptor descriptor_array_system_v_eightbyte_array;
static Descriptor descriptor_array_system_v_eightbyte_array_ptr;
static Descriptor descriptor_system_v_eightbyte_array_pointer;
static Descriptor descriptor_system_v_eightbyte_array_pointer_pointer;
static Descriptor descriptor_mass_function_call_lazy_payload;
static Descriptor descriptor_array_mass_function_call_lazy_payload;
static Descriptor descriptor_array_mass_function_call_lazy_payload_ptr;
static Descriptor descriptor_mass_function_call_lazy_payload_pointer;
static Descriptor descriptor_mass_function_call_lazy_payload_pointer_pointer;
static Descriptor descriptor_tuple;
static Descriptor descriptor_array_tuple;
static Descriptor descriptor_array_tuple_ptr;
static Descriptor descriptor_tuple_pointer;
static Descriptor descriptor_tuple_pointer_pointer;
static Descriptor descriptor_list_node;
static Descriptor descriptor_array_list_node;
static Descriptor descriptor_array_list_node_ptr;
static Descriptor descriptor_list_node_pointer;
static Descriptor descriptor_list_node_pointer_pointer;
static Descriptor descriptor_typed_symbol;
static Descriptor descriptor_array_typed_symbol;
static Descriptor descriptor_array_typed_symbol_ptr;
static Descriptor descriptor_typed_symbol_pointer;
static Descriptor descriptor_typed_symbol_pointer_pointer;
static Descriptor descriptor_struct_field;
static Descriptor descriptor_array_struct_field;
static Descriptor descriptor_array_struct_field_ptr;
static Descriptor descriptor_struct_field_pointer;
static Descriptor descriptor_struct_field_pointer_pointer;
static Descriptor descriptor_descriptor;
static Descriptor descriptor_array_descriptor;
static Descriptor descriptor_array_descriptor_ptr;
static Descriptor descriptor_array_const_descriptor_ptr;
static Descriptor descriptor_descriptor_pointer;
static Descriptor descriptor_descriptor_pointer_pointer;
static Descriptor descriptor_type;
static Descriptor descriptor_array_type;
static Descriptor descriptor_array_type_ptr;
static Descriptor descriptor_type_pointer;
static Descriptor descriptor_type_pointer_pointer;
static Descriptor descriptor_mass_type_constraint_proc;
static Descriptor descriptor_mass_error;
static Descriptor descriptor_array_mass_error;
static Descriptor descriptor_array_mass_error_ptr;
static Descriptor descriptor_array_const_mass_error_ptr;
static Descriptor descriptor_mass_error_pointer;
static Descriptor descriptor_mass_error_pointer_pointer;
static Descriptor descriptor_mass_result;
static Descriptor descriptor_array_mass_result;
static Descriptor descriptor_array_mass_result_ptr;
static Descriptor descriptor_array_const_mass_result_ptr;
static Descriptor descriptor_mass_result_pointer;
static Descriptor descriptor_mass_result_pointer_pointer;
static Descriptor descriptor_os;
static Descriptor descriptor_array_os;
static Descriptor descriptor_array_os_ptr;
static Descriptor descriptor_array_const_os_ptr;
static Descriptor descriptor_os_pointer;
static Descriptor descriptor_os_pointer_pointer;
static Descriptor descriptor_program;
static Descriptor descriptor_array_program;
static Descriptor descriptor_array_program_ptr;
static Descriptor descriptor_program_pointer;
static Descriptor descriptor_program_pointer_pointer;
static Descriptor descriptor_calling_convention_call_setup_proc;
static Descriptor descriptor_calling_convention;
static Descriptor descriptor_array_calling_convention;
static Descriptor descriptor_array_calling_convention_ptr;
static Descriptor descriptor_calling_convention_pointer;
static Descriptor descriptor_calling_convention_pointer_pointer;
static Descriptor descriptor_mass_trampoline_proc;
static Descriptor descriptor_mass_trampoline;
static Descriptor descriptor_array_mass_trampoline;
static Descriptor descriptor_array_mass_trampoline_ptr;
static Descriptor descriptor_mass_trampoline_pointer;
static Descriptor descriptor_mass_trampoline_pointer_pointer;
MASS_DEFINE_OPAQUE_C_TYPE(struct_field_set, Struct_Field_Set);
MASS_DEFINE_OPAQUE_C_TYPE(slice_set, Slice_Set);
MASS_DEFINE_OPAQUE_C_TYPE(symbol_map, Symbol_Map);
MASS_DEFINE_OPAQUE_C_TYPE(trampoline_map, Trampoline_Map);
MASS_DEFINE_OPAQUE_C_TYPE(scope_map, Scope_Map);
MASS_DEFINE_OPAQUE_C_TYPE(macro_replacement_map, Macro_Replacement_Map);
MASS_DEFINE_OPAQUE_C_TYPE(jit_import_library_handle_map, Jit_Import_Library_Handle_Map);
MASS_DEFINE_OPAQUE_C_TYPE(imported_module_map, Imported_Module_Map);
static Descriptor descriptor_jit_counters;
static Descriptor descriptor_array_jit_counters;
static Descriptor descriptor_array_jit_counters_ptr;
static Descriptor descriptor_jit_counters_pointer;
static Descriptor descriptor_jit_counters_pointer_pointer;
static Descriptor descriptor_jit;
static Descriptor descriptor_array_jit;
static Descriptor descriptor_array_jit_ptr;
static Descriptor descriptor_jit_pointer;
static Descriptor descriptor_jit_pointer_pointer;
MASS_DEFINE_OPAQUE_C_TYPE(static_pointer_length_map, Static_Pointer_Length_Map);
MASS_DEFINE_OPAQUE_C_TYPE(descriptor_pointer_to_cache_map, Descriptor_Pointer_To_Cache_Map);
MASS_DEFINE_OPAQUE_C_TYPE(intrinsic_proc_cache_map, Intrinsic_Proc_Cache_Map);
static Descriptor descriptor_common_symbols;
static Descriptor descriptor_array_common_symbols;
static Descriptor descriptor_array_common_symbols_ptr;
static Descriptor descriptor_common_symbols_pointer;
static Descriptor descriptor_common_symbols_pointer_pointer;
static Descriptor descriptor_compilation;
static Descriptor descriptor_array_compilation;
static Descriptor descriptor_array_compilation_ptr;
static Descriptor descriptor_compilation_pointer;
static Descriptor descriptor_compilation_pointer_pointer;
static Descriptor descriptor_lazy_value_proc;
static Descriptor descriptor_instruction_extension_type;
static Descriptor descriptor_array_instruction_extension_type;
static Descriptor descriptor_array_instruction_extension_type_ptr;
static Descriptor descriptor_array_const_instruction_extension_type_ptr;
static Descriptor descriptor_instruction_extension_type_pointer;
static Descriptor descriptor_instruction_extension_type_pointer_pointer;
static Descriptor descriptor_operand_encoding_type;
static Descriptor descriptor_array_operand_encoding_type;
static Descriptor descriptor_array_operand_encoding_type_ptr;
static Descriptor descriptor_array_const_operand_encoding_type_ptr;
static Descriptor descriptor_operand_encoding_type_pointer;
static Descriptor descriptor_operand_encoding_type_pointer_pointer;
static Descriptor descriptor_operand_encoding;
static Descriptor descriptor_array_operand_encoding;
static Descriptor descriptor_array_operand_encoding_ptr;
static Descriptor descriptor_operand_encoding_pointer;
static Descriptor descriptor_operand_encoding_pointer_pointer;
static Descriptor descriptor_instruction_encoding;
static Descriptor descriptor_array_instruction_encoding;
static Descriptor descriptor_array_instruction_encoding_ptr;
static Descriptor descriptor_instruction_encoding_pointer;
static Descriptor descriptor_instruction_encoding_pointer_pointer;
static Descriptor descriptor_x64_mnemonic;
static Descriptor descriptor_array_x64_mnemonic;
static Descriptor descriptor_array_x64_mnemonic_ptr;
static Descriptor descriptor_x64_mnemonic_pointer;
static Descriptor descriptor_x64_mnemonic_pointer_pointer;
static Descriptor descriptor_x86_64_system_v_classify;
static Descriptor descriptor_x86_64_system_v_parameter_for_classification;
static Descriptor descriptor_call_setup_fill_parameter_register_bitset;
static Descriptor descriptor_push_instruction;
static Descriptor descriptor_mass_get;
static Descriptor descriptor_mass_get_from_descriptor_module;
static Descriptor descriptor_mass_import;
static Descriptor descriptor_mass_intrinsic;
static Descriptor descriptor_mass_apply;
static Descriptor descriptor_mass_call;
static Descriptor descriptor_mass_using;
static Descriptor descriptor_mass_return;
static Descriptor descriptor_mass_operator_assignment;
static Descriptor descriptor_mass_define_inferred;
static Descriptor descriptor_mass_comma;
static Descriptor descriptor_mass_function_literal;
static Descriptor descriptor_mass_dereference;
static Descriptor descriptor_mass_array_like_get;
static Descriptor descriptor_mass_struct_get;
static Descriptor descriptor_mass_named_accessor;
static Descriptor descriptor_mass_spread;
static Descriptor descriptor_mass_typed_symbol;
static Descriptor descriptor_mass_pointer_to;
static Descriptor descriptor_mass_pointer_to_type;
static Descriptor descriptor_mass_eval;
static Descriptor descriptor_mass_inline_module;
static Descriptor descriptor_mass_c_struct;
static Descriptor descriptor_mass_exports;
static Descriptor descriptor_mass_cast;
static Descriptor descriptor_mass_zero_extend;
static Descriptor descriptor_mass_parse_type;
static Descriptor descriptor_mass_type_of;
static Descriptor descriptor_mass_size_of;
static Descriptor descriptor_mass_static_assert;
static Descriptor descriptor_allocator_allocate_bytes;
static Descriptor descriptor_mass_constraint_integer_type;
static Descriptor descriptor_mass_constraint_float_type;
static Descriptor descriptor_mass_constraint_pointer_type;
static Descriptor descriptor_mass_constraint_struct_type;
static Descriptor descriptor_mass_constraint_fixed_array_type;
static Descriptor descriptor_mass_constraint_function_instance_type;
static Descriptor descriptor_mass_tuple_length;
static Descriptor descriptor_mass_tuple_get;
static Descriptor descriptor_descriptor_pointer_to;
static Descriptor descriptor_scope_make_imperative;
static Descriptor descriptor_scope_make_declarative;
static Descriptor descriptor_mass_ensure_symbol;
static Descriptor descriptor_scope_define_value;
static Descriptor descriptor_same_type;
static Descriptor descriptor_register_acquire;
static Descriptor descriptor_register_acquire_temp;
static Descriptor descriptor_register_release;
static Descriptor descriptor_storage_register;
static Descriptor descriptor_storage_register_temp;
static Descriptor descriptor_storage_release_if_temporary;
static Descriptor descriptor_mass_expected_result_exact;
static Descriptor descriptor_mass_expected_result_exact_type;
static Descriptor descriptor_value_force;
static Descriptor descriptor_mass_module_get_impl;
static Descriptor descriptor_mass_forward_call_to_alias;
static Descriptor descriptor_mass_i64_logical_shift_left;
static Descriptor descriptor_mass_i64_logical_shift_right;
static Descriptor descriptor_mass_i64_bitwise_and;
static Descriptor descriptor_mass_i64_bitwise_or;
static Descriptor descriptor_mass_i64_add;
static Descriptor descriptor_mass_i64_subtract;
static Descriptor descriptor_mass_i64_signed_multiply;
static Descriptor descriptor_mass_i64_unsigned_multiply;
static Descriptor descriptor_mass_i64_signed_divide;
static Descriptor descriptor_mass_i64_unsigned_divide;
static Descriptor descriptor_mass_i64_signed_remainder;
static Descriptor descriptor_mass_i64_unsigned_remainder;
static Descriptor descriptor_mass_i64_signed_less;
static Descriptor descriptor_mass_i64_unsigned_less;
static Descriptor descriptor_mass_i64_signed_less_equal;
static Descriptor descriptor_mass_i64_unsigned_less_equal;
static Descriptor descriptor_mass_i64_signed_greater;
static Descriptor descriptor_mass_i64_unsigned_greater;
static Descriptor descriptor_mass_i64_signed_greater_equal;
static Descriptor descriptor_mass_i64_unsigned_greater_equal;
static Descriptor descriptor_slice_equal;
static Descriptor descriptor_value_make;
static Descriptor descriptor_descriptor_function_instance;
static Descriptor descriptor_mass_result_is_error;
static Descriptor descriptor_calling_convention_x86_64_system_v_call_setup_proc;
static Descriptor descriptor_mass_function_info_init_for_header_and_maybe_body;
static Descriptor descriptor_call_function_overload;
static Descriptor descriptor_mass_integer_add;
static Descriptor descriptor_mass_integer_subtract;
static Descriptor descriptor_mass_integer_multiply;
static Descriptor descriptor_mass_integer_divide;
static Descriptor descriptor_mass_integer_remainder;
static Descriptor descriptor_mass_integer_less;
static Descriptor descriptor_mass_integer_greater;
static Descriptor descriptor_mass_integer_less_equal;
static Descriptor descriptor_mass_integer_greater_equal;
static Descriptor descriptor_mass_integer_equal;
static Descriptor descriptor_mass_integer_not_equal;
static Descriptor descriptor_mass_generic_equal;
static Descriptor descriptor_mass_generic_not_equal;
static Descriptor descriptor__bool;
static Descriptor descriptor_array__bool;
static Descriptor descriptor_array__bool_ptr;
static Descriptor descriptor_array_const__bool_ptr;
static Descriptor descriptor__bool_pointer;
static Descriptor descriptor__bool_pointer_pointer;
static Descriptor descriptor_char;
static Descriptor descriptor_array_char;
static Descriptor descriptor_array_char_ptr;
static Descriptor descriptor_array_const_char_ptr;
static Descriptor descriptor_char_pointer;
static Descriptor descriptor_char_pointer_pointer;
static Descriptor descriptor_int;
static Descriptor descriptor_array_int;
static Descriptor descriptor_array_int_ptr;
static Descriptor descriptor_array_const_int_ptr;
static Descriptor descriptor_int_pointer;
static Descriptor descriptor_int_pointer_pointer;
static Descriptor descriptor_allocator;
static Descriptor descriptor_array_allocator;
static Descriptor descriptor_array_allocator_ptr;
static Descriptor descriptor_array_const_allocator_ptr;
static Descriptor descriptor_allocator_pointer;
static Descriptor descriptor_allocator_pointer_pointer;
static Descriptor descriptor_virtual_memory_buffer;
static Descriptor descriptor_array_virtual_memory_buffer;
static Descriptor descriptor_array_virtual_memory_buffer_ptr;
static Descriptor descriptor_array_const_virtual_memory_buffer_ptr;
static Descriptor descriptor_virtual_memory_buffer_pointer;
static Descriptor descriptor_virtual_memory_buffer_pointer_pointer;
static Descriptor descriptor_i8;
static Descriptor descriptor_array_i8;
static Descriptor descriptor_array_i8_ptr;
static Descriptor descriptor_array_const_i8_ptr;
static Descriptor descriptor_i8_pointer;
static Descriptor descriptor_i8_pointer_pointer;
static Descriptor descriptor_i16;
static Descriptor descriptor_array_i16;
static Descriptor descriptor_array_i16_ptr;
static Descriptor descriptor_array_const_i16_ptr;
static Descriptor descriptor_i16_pointer;
static Descriptor descriptor_i16_pointer_pointer;
static Descriptor descriptor_i32;
static Descriptor descriptor_array_i32;
static Descriptor descriptor_array_i32_ptr;
static Descriptor descriptor_array_const_i32_ptr;
static Descriptor descriptor_i32_pointer;
static Descriptor descriptor_i32_pointer_pointer;
static Descriptor descriptor_i64;
static Descriptor descriptor_array_i64;
static Descriptor descriptor_array_i64_ptr;
static Descriptor descriptor_array_const_i64_ptr;
static Descriptor descriptor_i64_pointer;
static Descriptor descriptor_i64_pointer_pointer;
static Descriptor descriptor_u8;
static Descriptor descriptor_array_u8;
static Descriptor descriptor_array_u8_ptr;
static Descriptor descriptor_array_const_u8_ptr;
static Descriptor descriptor_u8_pointer;
static Descriptor descriptor_u8_pointer_pointer;
static Descriptor descriptor_u16;
static Descriptor descriptor_array_u16;
static Descriptor descriptor_array_u16_ptr;
static Descriptor descriptor_array_const_u16_ptr;
static Descriptor descriptor_u16_pointer;
static Descriptor descriptor_u16_pointer_pointer;
static Descriptor descriptor_u32;
static Descriptor descriptor_array_u32;
static Descriptor descriptor_array_u32_ptr;
static Descriptor descriptor_array_const_u32_ptr;
static Descriptor descriptor_u32_pointer;
static Descriptor descriptor_u32_pointer_pointer;
static Descriptor descriptor_u64;
static Descriptor descriptor_array_u64;
static Descriptor descriptor_array_u64_ptr;
static Descriptor descriptor_array_const_u64_ptr;
static Descriptor descriptor_u64_pointer;
static Descriptor descriptor_u64_pointer_pointer;
static Descriptor descriptor_s8;
static Descriptor descriptor_array_s8;
static Descriptor descriptor_array_s8_ptr;
static Descriptor descriptor_array_const_s8_ptr;
static Descriptor descriptor_s8_pointer;
static Descriptor descriptor_s8_pointer_pointer;
static Descriptor descriptor_s16;
static Descriptor descriptor_array_s16;
static Descriptor descriptor_array_s16_ptr;
static Descriptor descriptor_array_const_s16_ptr;
static Descriptor descriptor_s16_pointer;
static Descriptor descriptor_s16_pointer_pointer;
static Descriptor descriptor_s32;
static Descriptor descriptor_array_s32;
static Descriptor descriptor_array_s32_ptr;
static Descriptor descriptor_array_const_s32_ptr;
static Descriptor descriptor_s32_pointer;
static Descriptor descriptor_s32_pointer_pointer;
static Descriptor descriptor_s64;
static Descriptor descriptor_array_s64;
static Descriptor descriptor_array_s64_ptr;
static Descriptor descriptor_array_const_s64_ptr;
static Descriptor descriptor_s64_pointer;
static Descriptor descriptor_s64_pointer_pointer;
static Descriptor descriptor_f32;
static Descriptor descriptor_array_f32;
static Descriptor descriptor_array_f32_ptr;
static Descriptor descriptor_array_const_f32_ptr;
static Descriptor descriptor_f32_pointer;
static Descriptor descriptor_f32_pointer_pointer;
static Descriptor descriptor_f64;
static Descriptor descriptor_array_f64;
static Descriptor descriptor_array_f64_ptr;
static Descriptor descriptor_array_const_f64_ptr;
static Descriptor descriptor_f64_pointer;
static Descriptor descriptor_f64_pointer_pointer;
static Descriptor descriptor_range_u8;
static Descriptor descriptor_array_range_u8;
static Descriptor descriptor_array_range_u8_ptr;
static Descriptor descriptor_range_u8_pointer;
static Descriptor descriptor_range_u8_pointer_pointer;
static Descriptor descriptor_range_u16;
static Descriptor descriptor_array_range_u16;
static Descriptor descriptor_array_range_u16_ptr;
static Descriptor descriptor_range_u16_pointer;
static Descriptor descriptor_range_u16_pointer_pointer;
static Descriptor descriptor_range_u32;
static Descriptor descriptor_array_range_u32;
static Descriptor descriptor_array_range_u32_ptr;
static Descriptor descriptor_range_u32_pointer;
static Descriptor descriptor_range_u32_pointer_pointer;
static Descriptor descriptor_range_u64;
static Descriptor descriptor_array_range_u64;
static Descriptor descriptor_array_range_u64_ptr;
static Descriptor descriptor_range_u64_pointer;
static Descriptor descriptor_range_u64_pointer_pointer;
static Descriptor descriptor_range_s8;
static Descriptor descriptor_array_range_s8;
static Descriptor descriptor_array_range_s8_ptr;
static Descriptor descriptor_range_s8_pointer;
static Descriptor descriptor_range_s8_pointer_pointer;
static Descriptor descriptor_range_s16;
static Descriptor descriptor_array_range_s16;
static Descriptor descriptor_array_range_s16_ptr;
static Descriptor descriptor_range_s16_pointer;
static Descriptor descriptor_range_s16_pointer_pointer;
static Descriptor descriptor_range_s32;
static Descriptor descriptor_array_range_s32;
static Descriptor descriptor_array_range_s32_ptr;
static Descriptor descriptor_range_s32_pointer;
static Descriptor descriptor_range_s32_pointer_pointer;
static Descriptor descriptor_range_s64;
static Descriptor descriptor_array_range_s64;
static Descriptor descriptor_array_range_s64_ptr;
static Descriptor descriptor_range_s64_pointer;
static Descriptor descriptor_range_s64_pointer_pointer;
static Descriptor descriptor_range_f32;
static Descriptor descriptor_array_range_f32;
static Descriptor descriptor_array_range_f32_ptr;
static Descriptor descriptor_range_f32_pointer;
static Descriptor descriptor_range_f32_pointer_pointer;
static Descriptor descriptor_range_f64;
static Descriptor descriptor_array_range_f64;
static Descriptor descriptor_array_range_f64_ptr;
static Descriptor descriptor_range_f64_pointer;
static Descriptor descriptor_range_f64_pointer_pointer;
static Descriptor descriptor_slice;
static Descriptor descriptor_array_slice;
static Descriptor descriptor_array_slice_ptr;
static Descriptor descriptor_slice_pointer;
static Descriptor descriptor_slice_pointer_pointer;
static Descriptor descriptor_dyn_array_internal;
static Descriptor descriptor_array_dyn_array_internal;
static Descriptor descriptor_array_dyn_array_internal_ptr;
static Descriptor descriptor_dyn_array_internal_pointer;
static Descriptor descriptor_dyn_array_internal_pointer_pointer;
static Descriptor descriptor_storage_3 = MASS_DESCRIPTOR_STATIC_ARRAY(Storage, 3, &descriptor_storage);
static Descriptor descriptor_i8_15 = MASS_DESCRIPTOR_STATIC_ARRAY(u8, 15, &descriptor_i8);
static Descriptor descriptor_instruction_15 = MASS_DESCRIPTOR_STATIC_ARRAY(Instruction, 15, &descriptor_instruction);
static Descriptor descriptor_i8_16 = MASS_DESCRIPTOR_STATIC_ARRAY(u8, 16, &descriptor_i8);
static Descriptor descriptor_i8_7 = MASS_DESCRIPTOR_STATIC_ARRAY(u8, 7, &descriptor_i8);
static Descriptor descriptor_system_v_argument_class_8 = MASS_DESCRIPTOR_STATIC_ARRAY(SYSTEM_V_ARGUMENT_CLASS, 8, &descriptor_system_v_argument_class);
static Descriptor descriptor_i8_4 = MASS_DESCRIPTOR_STATIC_ARRAY(u8, 4, &descriptor_i8);
static Descriptor descriptor_i8_3 = MASS_DESCRIPTOR_STATIC_ARRAY(u8, 3, &descriptor_i8);
static Descriptor descriptor_operand_encoding_3 = MASS_DESCRIPTOR_STATIC_ARRAY(Operand_Encoding, 3, &descriptor_operand_encoding);
static Descriptor descriptor_i8_0 = MASS_DESCRIPTOR_STATIC_ARRAY(s8, 0, &descriptor_i8);
MASS_DEFINE_STRUCT_DESCRIPTOR(bits, Bits,
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("as_u64"),
    .offset = offsetof(Bits, as_u64),
  },
);
MASS_DEFINE_TYPE_VALUE(bits);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_bits_ptr, bits_pointer, Array_Bits_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_bits, bits, Array_Bits);
DEFINE_VALUE_IS_AS_HELPERS(Bits, bits);
DEFINE_VALUE_IS_AS_HELPERS(Bits *, bits_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(source_position, Source_Position,
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("line"),
    .offset = offsetof(Source_Position, line),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("column"),
    .offset = offsetof(Source_Position, column),
  },
);
MASS_DEFINE_TYPE_VALUE(source_position);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_source_position_ptr, source_position_pointer, Array_Source_Position_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_source_position, source_position, Array_Source_Position);
DEFINE_VALUE_IS_AS_HELPERS(Source_Position, source_position);
DEFINE_VALUE_IS_AS_HELPERS(Source_Position *, source_position_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(source_file, Source_File,
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("path"),
    .offset = offsetof(Source_File, path),
  },
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("text"),
    .offset = offsetof(Source_File, text),
  },
);
MASS_DEFINE_TYPE_VALUE(source_file);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_source_file_ptr, source_file_pointer, Array_Source_File_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_source_file, source_file, Array_Source_File);
DEFINE_VALUE_IS_AS_HELPERS(Source_File, source_file);
DEFINE_VALUE_IS_AS_HELPERS(Source_File *, source_file_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(source_range, Source_Range,
  {
    .descriptor = &descriptor_source_file_pointer,
    .name = slice_literal_fields("file"),
    .offset = offsetof(Source_Range, file),
  },
  {
    .descriptor = &descriptor_range_u32,
    .name = slice_literal_fields("offsets"),
    .offset = offsetof(Source_Range, offsets),
  },
);
MASS_DEFINE_TYPE_VALUE(source_range);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_source_range_ptr, source_range_pointer, Array_Source_Range_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_source_range, source_range, Array_Source_Range);
DEFINE_VALUE_IS_AS_HELPERS(Source_Range, source_range);
DEFINE_VALUE_IS_AS_HELPERS(Source_Range *, source_range_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_while, Mass_While,
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("condition"),
    .offset = offsetof(Mass_While, condition),
  },
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("body"),
    .offset = offsetof(Mass_While, body),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_while);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_mass_while_ptr, mass_while_pointer, Array_Mass_While_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_mass_while, mass_while, Array_Mass_While);
DEFINE_VALUE_IS_AS_HELPERS(Mass_While, mass_while);
DEFINE_VALUE_IS_AS_HELPERS(Mass_While *, mass_while_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(assignment, Assignment,
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("target"),
    .offset = offsetof(Assignment, target),
  },
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("source"),
    .offset = offsetof(Assignment, source),
  },
);
MASS_DEFINE_TYPE_VALUE(assignment);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_assignment_ptr, assignment_pointer, Array_Assignment_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_assignment, assignment, Array_Assignment);
DEFINE_VALUE_IS_AS_HELPERS(Assignment, assignment);
DEFINE_VALUE_IS_AS_HELPERS(Assignment *, assignment_pointer);
/*union struct start */
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_module_exports_ptr, module_exports_pointer, Array_Module_Exports_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_module_exports, module_exports, Array_Module_Exports);
MASS_DEFINE_OPAQUE_C_TYPE(module_exports_tag, Module_Exports_Tag)
static C_Enum_Item module_exports_tag_items[] = {
{ .name = slice_literal_fields("Not_Specified"), .value = 0 },
{ .name = slice_literal_fields("All"), .value = 1 },
{ .name = slice_literal_fields("Selective"), .value = 2 },
};
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(module_exports_not_specified, 0);
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(module_exports_all, 0);
MASS_DEFINE_STRUCT_DESCRIPTOR(module_exports_selective, Module_Exports_Selective,
  {
    .descriptor = &descriptor_tuple_pointer,
    .name = slice_literal_fields("tuple"),
    .offset = offsetof(Module_Exports_Selective, tuple),
  },
);
MASS_DEFINE_TYPE_VALUE(module_exports_selective);
MASS_DEFINE_STRUCT_DESCRIPTOR(module_exports, Module_Exports,
  {
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_module_exports_tag,
    .offset = offsetof(Module_Exports, tag),
  },
  {
    .descriptor = &descriptor_scope_pointer,
    .name = slice_literal_fields("scope"),
    .offset = offsetof(Module_Exports, scope),
  },
  {
    .descriptor = &descriptor_source_range,
    .name = slice_literal_fields("source_range"),
    .offset = offsetof(Module_Exports, source_range),
  },
  {
    .name = slice_literal_fields("Selective"),
    .descriptor = &descriptor_module_exports_selective,
    .offset = offsetof(Module_Exports, Selective),
  },
  {
    .name = slice_literal_fields("Not_Specified"),
    .descriptor = &descriptor_module_exports_not_specified,
    .offset = offsetof(Module_Exports, Selective)/*:EmptyVariantOffset*/,
  },
  {
    .name = slice_literal_fields("All"),
    .descriptor = &descriptor_module_exports_all,
    .offset = offsetof(Module_Exports, Selective)/*:EmptyVariantOffset*/,
  },
);
MASS_DEFINE_TYPE_VALUE(module_exports);
DEFINE_VALUE_IS_AS_HELPERS(Module_Exports, module_exports);
DEFINE_VALUE_IS_AS_HELPERS(Module_Exports *, module_exports_pointer);
/*union struct end*/
MASS_DEFINE_STRUCT_DESCRIPTOR(module, Module,
  {
    .descriptor = &descriptor_source_range,
    .name = slice_literal_fields("source_range"),
    .offset = offsetof(Module, source_range),
  },
  {
    .descriptor = &descriptor_scope_pointer,
    .name = slice_literal_fields("own_scope"),
    .offset = offsetof(Module, own_scope),
  },
  {
    .descriptor = &descriptor_module_exports,
    .name = slice_literal_fields("exports"),
    .offset = offsetof(Module, exports),
  },
);
MASS_DEFINE_TYPE_VALUE(module);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_module_ptr, module_pointer, Array_Module_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_module, module, Array_Module);
DEFINE_VALUE_IS_AS_HELPERS(Module, module);
DEFINE_VALUE_IS_AS_HELPERS(Module *, module_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(parse_error, Parse_Error,
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("message"),
    .offset = offsetof(Parse_Error, message),
  },
  {
    .descriptor = &descriptor_source_range,
    .name = slice_literal_fields("source_range"),
    .offset = offsetof(Parse_Error, source_range),
  },
);
MASS_DEFINE_TYPE_VALUE(parse_error);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_parse_error_ptr, parse_error_pointer, Array_Parse_Error_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_parse_error, parse_error, Array_Parse_Error);
DEFINE_VALUE_IS_AS_HELPERS(Parse_Error, parse_error);
DEFINE_VALUE_IS_AS_HELPERS(Parse_Error *, parse_error_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(value_view, Value_View,
  {
    .descriptor = &descriptor_value_pointer_pointer,
    .name = slice_literal_fields("values"),
    .offset = offsetof(Value_View, values),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("length"),
    .offset = offsetof(Value_View, length),
  },
  {
    .descriptor = &descriptor_source_range,
    .name = slice_literal_fields("source_range"),
    .offset = offsetof(Value_View, source_range),
  },
);
MASS_DEFINE_TYPE_VALUE(value_view);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_value_view_ptr, value_view_pointer, Array_Value_View_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_value_view, value_view, Array_Value_View);
DEFINE_VALUE_IS_AS_HELPERS(Value_View, value_view);
DEFINE_VALUE_IS_AS_HELPERS(Value_View *, value_view_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(symbol, Symbol,
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("name"),
    .offset = offsetof(Symbol, name),
  },
);
MASS_DEFINE_TYPE_VALUE(symbol);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_symbol_ptr, symbol_pointer, Array_Symbol_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_symbol, symbol, Array_Symbol);
DEFINE_VALUE_IS_AS_HELPERS(Symbol, symbol);
DEFINE_VALUE_IS_AS_HELPERS(Symbol *, symbol_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(group_paren, Group_Paren,
  {
    .descriptor = &descriptor_value_view,
    .name = slice_literal_fields("children"),
    .offset = offsetof(Group_Paren, children),
  },
);
MASS_DEFINE_TYPE_VALUE(group_paren);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_group_paren_ptr, group_paren_pointer, Array_Group_Paren_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_group_paren, group_paren, Array_Group_Paren);
DEFINE_VALUE_IS_AS_HELPERS(Group_Paren, group_paren);
DEFINE_VALUE_IS_AS_HELPERS(Group_Paren *, group_paren_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(ast_return, Ast_Return,
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("value"),
    .offset = offsetof(Ast_Return, value),
  },
);
MASS_DEFINE_TYPE_VALUE(ast_return);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_ast_return_ptr, ast_return_pointer, Array_Ast_Return_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_ast_return, ast_return, Array_Ast_Return);
DEFINE_VALUE_IS_AS_HELPERS(Ast_Return, ast_return);
DEFINE_VALUE_IS_AS_HELPERS(Ast_Return *, ast_return_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(ast_statement, Ast_Statement,
  {
    .descriptor = &descriptor_value_view,
    .name = slice_literal_fields("children"),
    .offset = offsetof(Ast_Statement, children),
  },
  {
    .descriptor = &descriptor_ast_statement_pointer,
    .name = slice_literal_fields("next"),
    .offset = offsetof(Ast_Statement, next),
  },
);
MASS_DEFINE_TYPE_VALUE(ast_statement);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_ast_statement_ptr, ast_statement_pointer, Array_Ast_Statement_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_ast_statement, ast_statement, Array_Ast_Statement);
DEFINE_VALUE_IS_AS_HELPERS(Ast_Statement, ast_statement);
DEFINE_VALUE_IS_AS_HELPERS(Ast_Statement *, ast_statement_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(ast_block, Ast_Block,
  {
    .descriptor = &descriptor_ast_statement_pointer,
    .name = slice_literal_fields("first_statement"),
    .offset = offsetof(Ast_Block, first_statement),
  },
  {
    .descriptor = &descriptor_ast_statement_pointer,
    .name = slice_literal_fields("last_statement"),
    .offset = offsetof(Ast_Block, last_statement),
  },
);
MASS_DEFINE_TYPE_VALUE(ast_block);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_ast_block_ptr, ast_block_pointer, Array_Ast_Block_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_ast_block, ast_block, Array_Ast_Block);
DEFINE_VALUE_IS_AS_HELPERS(Ast_Block, ast_block);
DEFINE_VALUE_IS_AS_HELPERS(Ast_Block *, ast_block_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(group_square, Group_Square,
  {
    .descriptor = &descriptor_value_view,
    .name = slice_literal_fields("children"),
    .offset = offsetof(Group_Square, children),
  },
);
MASS_DEFINE_TYPE_VALUE(group_square);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_group_square_ptr, group_square_pointer, Array_Group_Square_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_group_square, group_square, Array_Group_Square);
DEFINE_VALUE_IS_AS_HELPERS(Group_Square, group_square);
DEFINE_VALUE_IS_AS_HELPERS(Group_Square *, group_square_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(ast_using, Ast_Using,
  {
    .descriptor = &descriptor_module_pointer,
    .name = slice_literal_fields("module"),
    .offset = offsetof(Ast_Using, module),
  },
);
MASS_DEFINE_TYPE_VALUE(ast_using);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_ast_using_ptr, ast_using_pointer, Array_Ast_Using_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_ast_using, ast_using, Array_Ast_Using);
DEFINE_VALUE_IS_AS_HELPERS(Ast_Using, ast_using);
DEFINE_VALUE_IS_AS_HELPERS(Ast_Using *, ast_using_pointer);
MASS_DEFINE_OPAQUE_C_TYPE(section_permissions, Section_Permissions)
static C_Enum_Item section_permissions_items[] = {
{ .name = slice_literal_fields("Read"), .value = 1 },
{ .name = slice_literal_fields("Write"), .value = 2 },
{ .name = slice_literal_fields("Execute"), .value = 4 },
};
DEFINE_VALUE_IS_AS_HELPERS(Section_Permissions, section_permissions);
DEFINE_VALUE_IS_AS_HELPERS(Section_Permissions *, section_permissions_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(section, Section,
  {
    .descriptor = &descriptor_virtual_memory_buffer,
    .name = slice_literal_fields("buffer"),
    .offset = offsetof(Section, buffer),
  },
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("name"),
    .offset = offsetof(Section, name),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("base_rva"),
    .offset = offsetof(Section, base_rva),
  },
  {
    .descriptor = &descriptor_section_permissions,
    .name = slice_literal_fields("permissions"),
    .offset = offsetof(Section, permissions),
  },
);
MASS_DEFINE_TYPE_VALUE(section);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_section_ptr, section_pointer, Array_Section_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_section, section, Array_Section);
DEFINE_VALUE_IS_AS_HELPERS(Section, section);
DEFINE_VALUE_IS_AS_HELPERS(Section *, section_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(program_memory, Program_Memory,
  {
    .descriptor = &descriptor_virtual_memory_buffer,
    .name = slice_literal_fields("buffer"),
    .offset = offsetof(Program_Memory, buffer),
  },
  {
    .descriptor = &descriptor_section,
    .name = slice_literal_fields("rw_data"),
    .offset = offsetof(Program_Memory, rw_data),
  },
  {
    .descriptor = &descriptor_section,
    .name = slice_literal_fields("code"),
    .offset = offsetof(Program_Memory, code),
  },
  {
    .descriptor = &descriptor_section,
    .name = slice_literal_fields("ro_data"),
    .offset = offsetof(Program_Memory, ro_data),
  },
);
MASS_DEFINE_TYPE_VALUE(program_memory);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_program_memory_ptr, program_memory_pointer, Array_Program_Memory_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_program_memory, program_memory, Array_Program_Memory);
DEFINE_VALUE_IS_AS_HELPERS(Program_Memory, program_memory);
DEFINE_VALUE_IS_AS_HELPERS(Program_Memory *, program_memory_pointer);
MASS_DEFINE_OPAQUE_C_TYPE(register, Register)
static C_Enum_Item register_items[] = {
{ .name = slice_literal_fields("A"), .value = 0 },
{ .name = slice_literal_fields("C"), .value = 1 },
{ .name = slice_literal_fields("D"), .value = 2 },
{ .name = slice_literal_fields("B"), .value = 3 },
{ .name = slice_literal_fields("SP"), .value = 4 },
{ .name = slice_literal_fields("BP"), .value = 5 },
{ .name = slice_literal_fields("SI"), .value = 6 },
{ .name = slice_literal_fields("DI"), .value = 7 },
{ .name = slice_literal_fields("R8"), .value = 8 },
{ .name = slice_literal_fields("R9"), .value = 9 },
{ .name = slice_literal_fields("R10"), .value = 10 },
{ .name = slice_literal_fields("R11"), .value = 11 },
{ .name = slice_literal_fields("R12"), .value = 12 },
{ .name = slice_literal_fields("R13"), .value = 13 },
{ .name = slice_literal_fields("R14"), .value = 14 },
{ .name = slice_literal_fields("R15"), .value = 15 },
{ .name = slice_literal_fields("Xmm0"), .value = 16 },
{ .name = slice_literal_fields("Xmm1"), .value = 17 },
{ .name = slice_literal_fields("Xmm2"), .value = 18 },
{ .name = slice_literal_fields("Xmm3"), .value = 19 },
{ .name = slice_literal_fields("Xmm4"), .value = 20 },
{ .name = slice_literal_fields("Xmm5"), .value = 21 },
{ .name = slice_literal_fields("Xmm6"), .value = 22 },
{ .name = slice_literal_fields("Xmm7"), .value = 23 },
{ .name = slice_literal_fields("Xmm8"), .value = 24 },
{ .name = slice_literal_fields("Xmm9"), .value = 25 },
{ .name = slice_literal_fields("Xmm10"), .value = 26 },
{ .name = slice_literal_fields("Xmm11"), .value = 27 },
{ .name = slice_literal_fields("Xmm12"), .value = 28 },
{ .name = slice_literal_fields("Xmm13"), .value = 29 },
{ .name = slice_literal_fields("Xmm14"), .value = 30 },
{ .name = slice_literal_fields("Xmm15"), .value = 31 },
};
DEFINE_VALUE_IS_AS_HELPERS(Register, register);
DEFINE_VALUE_IS_AS_HELPERS(Register *, register_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(label, Label,
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("resolved"),
    .offset = offsetof(Label, resolved),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("offset_in_section"),
    .offset = offsetof(Label, offset_in_section),
  },
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("name"),
    .offset = offsetof(Label, name),
  },
  {
    .descriptor = &descriptor_section_pointer,
    .name = slice_literal_fields("section"),
    .offset = offsetof(Label, section),
  },
  {
    .descriptor = &descriptor_program_pointer,
    .name = slice_literal_fields("program"),
    .offset = offsetof(Label, program),
  },
);
MASS_DEFINE_TYPE_VALUE(label);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_label_ptr, label_pointer, Array_Label_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_label, label, Array_Label);
DEFINE_VALUE_IS_AS_HELPERS(Label, label);
DEFINE_VALUE_IS_AS_HELPERS(Label *, label_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(label_location_diff_patch_info, Label_Location_Diff_Patch_Info,
  {
    .descriptor = &descriptor_label_pointer,
    .name = slice_literal_fields("target"),
    .offset = offsetof(Label_Location_Diff_Patch_Info, target),
  },
  {
    .descriptor = &descriptor_label,
    .name = slice_literal_fields("from"),
    .offset = offsetof(Label_Location_Diff_Patch_Info, from),
  },
  {
    .descriptor = &descriptor_void_pointer,
    .name = slice_literal_fields("patch32_at"),
    .offset = offsetof(Label_Location_Diff_Patch_Info, patch32_at),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("offset_from_label"),
    .offset = offsetof(Label_Location_Diff_Patch_Info, offset_from_label),
  },
);
MASS_DEFINE_TYPE_VALUE(label_location_diff_patch_info);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_label_location_diff_patch_info_ptr, label_location_diff_patch_info_pointer, Array_Label_Location_Diff_Patch_Info_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_label_location_diff_patch_info, label_location_diff_patch_info, Array_Label_Location_Diff_Patch_Info);
DEFINE_VALUE_IS_AS_HELPERS(Label_Location_Diff_Patch_Info, label_location_diff_patch_info);
DEFINE_VALUE_IS_AS_HELPERS(Label_Location_Diff_Patch_Info *, label_location_diff_patch_info_pointer);
MASS_DEFINE_OPAQUE_C_TYPE(number_base, Number_Base)
static C_Enum_Item number_base_items[] = {
{ .name = slice_literal_fields("2"), .value = 2 },
{ .name = slice_literal_fields("10"), .value = 10 },
{ .name = slice_literal_fields("16"), .value = 16 },
};
DEFINE_VALUE_IS_AS_HELPERS(Number_Base, number_base);
DEFINE_VALUE_IS_AS_HELPERS(Number_Base *, number_base_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(quoted, Quoted,
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("value"),
    .offset = offsetof(Quoted, value),
  },
);
MASS_DEFINE_TYPE_VALUE(quoted);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_quoted_ptr, quoted_pointer, Array_Quoted_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_quoted, quoted, Array_Quoted);
DEFINE_VALUE_IS_AS_HELPERS(Quoted, quoted);
DEFINE_VALUE_IS_AS_HELPERS(Quoted *, quoted_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(spread, Spread,
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("value"),
    .offset = offsetof(Spread, value),
  },
);
MASS_DEFINE_TYPE_VALUE(spread);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_spread_ptr, spread_pointer, Array_Spread_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_spread, spread, Array_Spread);
DEFINE_VALUE_IS_AS_HELPERS(Spread, spread);
DEFINE_VALUE_IS_AS_HELPERS(Spread *, spread_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(named_accessor, Named_Accessor,
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("symbol"),
    .offset = offsetof(Named_Accessor, symbol),
  },
);
MASS_DEFINE_TYPE_VALUE(named_accessor);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_named_accessor_ptr, named_accessor_pointer, Array_Named_Accessor_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_named_accessor, named_accessor, Array_Named_Accessor);
DEFINE_VALUE_IS_AS_HELPERS(Named_Accessor, named_accessor);
DEFINE_VALUE_IS_AS_HELPERS(Named_Accessor *, named_accessor_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(external_symbol, External_Symbol,
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("library_name"),
    .offset = offsetof(External_Symbol, library_name),
  },
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("symbol_name"),
    .offset = offsetof(External_Symbol, symbol_name),
  },
);
MASS_DEFINE_TYPE_VALUE(external_symbol);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_external_symbol_ptr, external_symbol_pointer, Array_External_Symbol_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_external_symbol, external_symbol, Array_External_Symbol);
DEFINE_VALUE_IS_AS_HELPERS(External_Symbol, external_symbol);
DEFINE_VALUE_IS_AS_HELPERS(External_Symbol *, external_symbol_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(import_symbol, Import_Symbol,
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("name"),
    .offset = offsetof(Import_Symbol, name),
  },
  {
    .descriptor = &descriptor_label_pointer,
    .name = slice_literal_fields("label32"),
    .offset = offsetof(Import_Symbol, label32),
  },
);
MASS_DEFINE_TYPE_VALUE(import_symbol);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_import_symbol_ptr, import_symbol_pointer, Array_Import_Symbol_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_import_symbol, import_symbol, Array_Import_Symbol);
DEFINE_VALUE_IS_AS_HELPERS(Import_Symbol, import_symbol);
DEFINE_VALUE_IS_AS_HELPERS(Import_Symbol *, import_symbol_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(import_library, Import_Library,
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("name"),
    .offset = offsetof(Import_Library, name),
  },
  {
    .descriptor = &descriptor_array_import_symbol,
    .name = slice_literal_fields("symbols"),
    .offset = offsetof(Import_Library, symbols),
  },
);
MASS_DEFINE_TYPE_VALUE(import_library);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_import_library_ptr, import_library_pointer, Array_Import_Library_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_import_library, import_library, Array_Import_Library);
DEFINE_VALUE_IS_AS_HELPERS(Import_Library, import_library);
DEFINE_VALUE_IS_AS_HELPERS(Import_Library *, import_library_pointer);
MASS_DEFINE_OPAQUE_C_TYPE(compare_type, Compare_Type)
static C_Enum_Item compare_type_items[] = {
{ .name = slice_literal_fields("Equal"), .value = 1 },
{ .name = slice_literal_fields("Not_Equal"), .value = 2 },
{ .name = slice_literal_fields("Unsigned_Below"), .value = 3 },
{ .name = slice_literal_fields("Unsigned_Below_Equal"), .value = 4 },
{ .name = slice_literal_fields("Unsigned_Above"), .value = 5 },
{ .name = slice_literal_fields("Unsigned_Above_Equal"), .value = 6 },
{ .name = slice_literal_fields("Signed_Less"), .value = 7 },
{ .name = slice_literal_fields("Signed_Less_Equal"), .value = 8 },
{ .name = slice_literal_fields("Signed_Greater"), .value = 9 },
{ .name = slice_literal_fields("Signed_Greater_Equal"), .value = 10 },
};
DEFINE_VALUE_IS_AS_HELPERS(Compare_Type, compare_type);
DEFINE_VALUE_IS_AS_HELPERS(Compare_Type *, compare_type_pointer);
MASS_DEFINE_OPAQUE_C_TYPE(stack_area, Stack_Area)
static C_Enum_Item stack_area_items[] = {
{ .name = slice_literal_fields("Local"), .value = 0 },
{ .name = slice_literal_fields("Received_Argument"), .value = 1 },
{ .name = slice_literal_fields("Call_Target_Argument"), .value = 2 },
};
DEFINE_VALUE_IS_AS_HELPERS(Stack_Area, stack_area);
DEFINE_VALUE_IS_AS_HELPERS(Stack_Area *, stack_area_pointer);
/*union struct start */
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_memory_location_ptr, memory_location_pointer, Array_Memory_Location_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_memory_location, memory_location, Array_Memory_Location);
MASS_DEFINE_OPAQUE_C_TYPE(memory_location_tag, Memory_Location_Tag)
static C_Enum_Item memory_location_tag_items[] = {
{ .name = slice_literal_fields("Instruction_Pointer_Relative"), .value = 0 },
{ .name = slice_literal_fields("Indirect"), .value = 1 },
{ .name = slice_literal_fields("Stack"), .value = 2 },
};
MASS_DEFINE_STRUCT_DESCRIPTOR(memory_location_instruction_pointer_relative, Memory_Location_Instruction_Pointer_Relative,
  {
    .descriptor = &descriptor_label_pointer,
    .name = slice_literal_fields("label"),
    .offset = offsetof(Memory_Location_Instruction_Pointer_Relative, label),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("offset"),
    .offset = offsetof(Memory_Location_Instruction_Pointer_Relative, offset),
  },
);
MASS_DEFINE_TYPE_VALUE(memory_location_instruction_pointer_relative);
MASS_DEFINE_STRUCT_DESCRIPTOR(memory_location_indirect, Memory_Location_Indirect,
  {
    .descriptor = &descriptor_register,
    .name = slice_literal_fields("base_register"),
    .offset = offsetof(Memory_Location_Indirect, base_register),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("offset"),
    .offset = offsetof(Memory_Location_Indirect, offset),
  },
);
MASS_DEFINE_TYPE_VALUE(memory_location_indirect);
MASS_DEFINE_STRUCT_DESCRIPTOR(memory_location_stack, Memory_Location_Stack,
  {
    .descriptor = &descriptor_stack_area,
    .name = slice_literal_fields("area"),
    .offset = offsetof(Memory_Location_Stack, area),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("offset"),
    .offset = offsetof(Memory_Location_Stack, offset),
  },
);
MASS_DEFINE_TYPE_VALUE(memory_location_stack);
MASS_DEFINE_STRUCT_DESCRIPTOR(memory_location, Memory_Location,
  {
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_memory_location_tag,
    .offset = offsetof(Memory_Location, tag),
  },
  {
    .name = slice_literal_fields("Instruction_Pointer_Relative"),
    .descriptor = &descriptor_memory_location_instruction_pointer_relative,
    .offset = offsetof(Memory_Location, Instruction_Pointer_Relative),
  },
  {
    .name = slice_literal_fields("Indirect"),
    .descriptor = &descriptor_memory_location_indirect,
    .offset = offsetof(Memory_Location, Indirect),
  },
  {
    .name = slice_literal_fields("Stack"),
    .descriptor = &descriptor_memory_location_stack,
    .offset = offsetof(Memory_Location, Stack),
  },
);
MASS_DEFINE_TYPE_VALUE(memory_location);
DEFINE_VALUE_IS_AS_HELPERS(Memory_Location, memory_location);
DEFINE_VALUE_IS_AS_HELPERS(Memory_Location *, memory_location_pointer);
/*union struct end*/
MASS_DEFINE_OPAQUE_C_TYPE(storage_flags, Storage_Flags)
static C_Enum_Item storage_flags_items[] = {
{ .name = slice_literal_fields("None"), .value = 0 },
{ .name = slice_literal_fields("Temporary"), .value = 1 },
};
DEFINE_VALUE_IS_AS_HELPERS(Storage_Flags, storage_flags);
DEFINE_VALUE_IS_AS_HELPERS(Storage_Flags *, storage_flags_pointer);
/*union struct start */
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_storage_ptr, storage_pointer, Array_Storage_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_storage, storage, Array_Storage);
MASS_DEFINE_OPAQUE_C_TYPE(storage_tag, Storage_Tag)
static C_Enum_Item storage_tag_items[] = {
{ .name = slice_literal_fields("Immediate"), .value = 0 },
{ .name = slice_literal_fields("Eflags"), .value = 1 },
{ .name = slice_literal_fields("Register"), .value = 2 },
{ .name = slice_literal_fields("Xmm"), .value = 3 },
{ .name = slice_literal_fields("Static"), .value = 4 },
{ .name = slice_literal_fields("Memory"), .value = 5 },
{ .name = slice_literal_fields("Disjoint"), .value = 6 },
};
MASS_DEFINE_STRUCT_DESCRIPTOR(storage_immediate, Storage_Immediate,
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("bits"),
    .offset = offsetof(Storage_Immediate, bits),
  },
);
MASS_DEFINE_TYPE_VALUE(storage_immediate);
MASS_DEFINE_STRUCT_DESCRIPTOR(storage_eflags, Storage_Eflags,
  {
    .descriptor = &descriptor_compare_type,
    .name = slice_literal_fields("compare_type"),
    .offset = offsetof(Storage_Eflags, compare_type),
  },
);
MASS_DEFINE_TYPE_VALUE(storage_eflags);
MASS_DEFINE_STRUCT_DESCRIPTOR(storage_register, Storage_Register,
  {
    .descriptor = &descriptor_register,
    .name = slice_literal_fields("index"),
    .offset = offsetof(Storage_Register, index),
  },
  {
    .descriptor = &descriptor_i16,
    .name = slice_literal_fields("packed"),
    .offset = offsetof(Storage_Register, packed),
  },
  {
    .descriptor = &descriptor_i16,
    .name = slice_literal_fields("offset_in_bits"),
    .offset = offsetof(Storage_Register, offset_in_bits),
  },
);
MASS_DEFINE_TYPE_VALUE(storage_register);
MASS_DEFINE_STRUCT_DESCRIPTOR(storage_xmm, Storage_Xmm,
  {
    .descriptor = &descriptor_register,
    .name = slice_literal_fields("index"),
    .offset = offsetof(Storage_Xmm, index),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("offset"),
    .offset = offsetof(Storage_Xmm, offset),
  },
);
MASS_DEFINE_TYPE_VALUE(storage_xmm);
MASS_DEFINE_STRUCT_DESCRIPTOR(storage_static, Storage_Static,
  {
    .descriptor = &descriptor_void_pointer,
    .name = slice_literal_fields("pointer"),
    .offset = offsetof(Storage_Static, pointer),
  },
);
MASS_DEFINE_TYPE_VALUE(storage_static);
MASS_DEFINE_STRUCT_DESCRIPTOR(storage_memory, Storage_Memory,
  {
    .descriptor = &descriptor_memory_location,
    .name = slice_literal_fields("location"),
    .offset = offsetof(Storage_Memory, location),
  },
);
MASS_DEFINE_TYPE_VALUE(storage_memory);
MASS_DEFINE_STRUCT_DESCRIPTOR(storage_disjoint, Storage_Disjoint,
  {
    .descriptor = &descriptor_array_storage_ptr,
    .name = slice_literal_fields("pieces"),
    .offset = offsetof(Storage_Disjoint, pieces),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("packed"),
    .offset = offsetof(Storage_Disjoint, packed),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("offset_in_bits"),
    .offset = offsetof(Storage_Disjoint, offset_in_bits),
  },
);
MASS_DEFINE_TYPE_VALUE(storage_disjoint);
MASS_DEFINE_STRUCT_DESCRIPTOR(storage, Storage,
  {
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_storage_tag,
    .offset = offsetof(Storage, tag),
  },
  {
    .descriptor = &descriptor_storage_flags,
    .name = slice_literal_fields("flags"),
    .offset = offsetof(Storage, flags),
  },
  {
    .descriptor = &descriptor_bits,
    .name = slice_literal_fields("bit_size"),
    .offset = offsetof(Storage, bit_size),
  },
  {
    .name = slice_literal_fields("Immediate"),
    .descriptor = &descriptor_storage_immediate,
    .offset = offsetof(Storage, Immediate),
  },
  {
    .name = slice_literal_fields("Eflags"),
    .descriptor = &descriptor_storage_eflags,
    .offset = offsetof(Storage, Eflags),
  },
  {
    .name = slice_literal_fields("Register"),
    .descriptor = &descriptor_storage_register,
    .offset = offsetof(Storage, Register),
  },
  {
    .name = slice_literal_fields("Xmm"),
    .descriptor = &descriptor_storage_xmm,
    .offset = offsetof(Storage, Xmm),
  },
  {
    .name = slice_literal_fields("Static"),
    .descriptor = &descriptor_storage_static,
    .offset = offsetof(Storage, Static),
  },
  {
    .name = slice_literal_fields("Memory"),
    .descriptor = &descriptor_storage_memory,
    .offset = offsetof(Storage, Memory),
  },
  {
    .name = slice_literal_fields("Disjoint"),
    .descriptor = &descriptor_storage_disjoint,
    .offset = offsetof(Storage, Disjoint),
  },
);
MASS_DEFINE_TYPE_VALUE(storage);
DEFINE_VALUE_IS_AS_HELPERS(Storage, storage);
DEFINE_VALUE_IS_AS_HELPERS(Storage *, storage_pointer);
/*union struct end*/
MASS_DEFINE_STRUCT_DESCRIPTOR(relocation, Relocation,
  {
    .descriptor = &descriptor_storage,
    .name = slice_literal_fields("patch_at"),
    .offset = offsetof(Relocation, patch_at),
  },
  {
    .descriptor = &descriptor_storage,
    .name = slice_literal_fields("address_of"),
    .offset = offsetof(Relocation, address_of),
  },
);
MASS_DEFINE_TYPE_VALUE(relocation);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_relocation_ptr, relocation_pointer, Array_Relocation_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_relocation, relocation, Array_Relocation);
DEFINE_VALUE_IS_AS_HELPERS(Relocation, relocation);
DEFINE_VALUE_IS_AS_HELPERS(Relocation *, relocation_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(instruction_assembly, Instruction_Assembly,
  {
    .descriptor = &descriptor_x64_mnemonic_pointer,
    .name = slice_literal_fields("mnemonic"),
    .offset = offsetof(Instruction_Assembly, mnemonic),
  },
  {
    .descriptor = &descriptor_storage_3,
    .name = slice_literal_fields("operands"),
    .offset = offsetof(Instruction_Assembly, operands),
  },
);
MASS_DEFINE_TYPE_VALUE(instruction_assembly);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_instruction_assembly_ptr, instruction_assembly_pointer, Array_Instruction_Assembly_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_instruction_assembly, instruction_assembly, Array_Instruction_Assembly);
DEFINE_VALUE_IS_AS_HELPERS(Instruction_Assembly, instruction_assembly);
DEFINE_VALUE_IS_AS_HELPERS(Instruction_Assembly *, instruction_assembly_pointer);
/*union struct start */
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_instruction_ptr, instruction_pointer, Array_Instruction_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_instruction, instruction, Array_Instruction);
MASS_DEFINE_OPAQUE_C_TYPE(instruction_tag, Instruction_Tag)
static C_Enum_Item instruction_tag_items[] = {
{ .name = slice_literal_fields("Label"), .value = 0 },
{ .name = slice_literal_fields("Bytes"), .value = 1 },
{ .name = slice_literal_fields("Label_Patch"), .value = 2 },
{ .name = slice_literal_fields("Stack_Patch"), .value = 3 },
{ .name = slice_literal_fields("Location"), .value = 4 },
};
MASS_DEFINE_STRUCT_DESCRIPTOR(instruction_label, Instruction_Label,
  {
    .descriptor = &descriptor_label_pointer,
    .name = slice_literal_fields("pointer"),
    .offset = offsetof(Instruction_Label, pointer),
  },
);
MASS_DEFINE_TYPE_VALUE(instruction_label);
MASS_DEFINE_STRUCT_DESCRIPTOR(instruction_bytes, Instruction_Bytes,
  {
    .descriptor = &descriptor_i8_15,
    .name = slice_literal_fields("memory"),
    .offset = offsetof(Instruction_Bytes, memory),
  },
  {
    .descriptor = &descriptor_i8,
    .name = slice_literal_fields("length"),
    .offset = offsetof(Instruction_Bytes, length),
  },
);
MASS_DEFINE_TYPE_VALUE(instruction_bytes);
MASS_DEFINE_STRUCT_DESCRIPTOR(instruction_label_patch, Instruction_Label_Patch,
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("offset_in_instruction"),
    .offset = offsetof(Instruction_Label_Patch, offset_in_instruction),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("offset_from_label"),
    .offset = offsetof(Instruction_Label_Patch, offset_from_label),
  },
  {
    .descriptor = &descriptor_label_pointer,
    .name = slice_literal_fields("label"),
    .offset = offsetof(Instruction_Label_Patch, label),
  },
);
MASS_DEFINE_TYPE_VALUE(instruction_label_patch);
MASS_DEFINE_STRUCT_DESCRIPTOR(instruction_stack_patch, Instruction_Stack_Patch,
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("mod_r_m_offset_in_previous_instruction"),
    .offset = offsetof(Instruction_Stack_Patch, mod_r_m_offset_in_previous_instruction),
  },
  {
    .descriptor = &descriptor_stack_area,
    .name = slice_literal_fields("stack_area"),
    .offset = offsetof(Instruction_Stack_Patch, stack_area),
  },
);
MASS_DEFINE_TYPE_VALUE(instruction_stack_patch);
MASS_DEFINE_STRUCT_DESCRIPTOR(instruction_location, Instruction_Location,
  {
    .descriptor = &descriptor_source_range,
    .name = slice_literal_fields("source_range"),
    .offset = offsetof(Instruction_Location, source_range),
  },
);
MASS_DEFINE_TYPE_VALUE(instruction_location);
MASS_DEFINE_STRUCT_DESCRIPTOR(instruction, Instruction,
  {
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_instruction_tag,
    .offset = offsetof(Instruction, tag),
  },
  {
    .descriptor = &descriptor_scope_pointer,
    .name = slice_literal_fields("scope"),
    .offset = offsetof(Instruction, scope),
  },
  {
    .name = slice_literal_fields("Label"),
    .descriptor = &descriptor_instruction_label,
    .offset = offsetof(Instruction, Label),
  },
  {
    .name = slice_literal_fields("Bytes"),
    .descriptor = &descriptor_instruction_bytes,
    .offset = offsetof(Instruction, Bytes),
  },
  {
    .name = slice_literal_fields("Label_Patch"),
    .descriptor = &descriptor_instruction_label_patch,
    .offset = offsetof(Instruction, Label_Patch),
  },
  {
    .name = slice_literal_fields("Stack_Patch"),
    .descriptor = &descriptor_instruction_stack_patch,
    .offset = offsetof(Instruction, Stack_Patch),
  },
  {
    .name = slice_literal_fields("Location"),
    .descriptor = &descriptor_instruction_location,
    .offset = offsetof(Instruction, Location),
  },
);
MASS_DEFINE_TYPE_VALUE(instruction);
DEFINE_VALUE_IS_AS_HELPERS(Instruction, instruction);
DEFINE_VALUE_IS_AS_HELPERS(Instruction *, instruction_pointer);
/*union struct end*/
MASS_DEFINE_STRUCT_DESCRIPTOR(instruction_bucket, Instruction_Bucket,
  {
    .descriptor = &descriptor_instruction_15,
    .name = slice_literal_fields("items"),
    .offset = offsetof(Instruction_Bucket, items),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("length"),
    .offset = offsetof(Instruction_Bucket, length),
  },
  {
    .descriptor = &descriptor_instruction_bucket_pointer,
    .name = slice_literal_fields("next"),
    .offset = offsetof(Instruction_Bucket, next),
  },
);
MASS_DEFINE_TYPE_VALUE(instruction_bucket);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_instruction_bucket_ptr, instruction_bucket_pointer, Array_Instruction_Bucket_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_instruction_bucket, instruction_bucket, Array_Instruction_Bucket);
DEFINE_VALUE_IS_AS_HELPERS(Instruction_Bucket, instruction_bucket);
DEFINE_VALUE_IS_AS_HELPERS(Instruction_Bucket *, instruction_bucket_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(code_block, Code_Block,
  {
    .descriptor = &descriptor_allocator_pointer,
    .name = slice_literal_fields("allocator"),
    .offset = offsetof(Code_Block, allocator),
  },
  {
    .descriptor = &descriptor_label_pointer,
    .name = slice_literal_fields("start_label"),
    .offset = offsetof(Code_Block, start_label),
  },
  {
    .descriptor = &descriptor_label_pointer,
    .name = slice_literal_fields("end_label"),
    .offset = offsetof(Code_Block, end_label),
  },
  {
    .descriptor = &descriptor_instruction_bucket_pointer,
    .name = slice_literal_fields("first_bucket"),
    .offset = offsetof(Code_Block, first_bucket),
  },
  {
    .descriptor = &descriptor_instruction_bucket_pointer,
    .name = slice_literal_fields("last_bucket"),
    .offset = offsetof(Code_Block, last_bucket),
  },
);
MASS_DEFINE_TYPE_VALUE(code_block);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_code_block_ptr, code_block_pointer, Array_Code_Block_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_code_block, code_block, Array_Code_Block);
DEFINE_VALUE_IS_AS_HELPERS(Code_Block, code_block);
DEFINE_VALUE_IS_AS_HELPERS(Code_Block *, code_block_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(epoch, Epoch,
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("as_u64"),
    .offset = offsetof(Epoch, as_u64),
  },
);
MASS_DEFINE_TYPE_VALUE(epoch);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_epoch_ptr, epoch_pointer, Array_Epoch_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_epoch, epoch, Array_Epoch);
DEFINE_VALUE_IS_AS_HELPERS(Epoch, epoch);
DEFINE_VALUE_IS_AS_HELPERS(Epoch *, epoch_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(function_layout, Function_Layout,
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("stack_reserve"),
    .offset = offsetof(Function_Layout, stack_reserve),
  },
  {
    .descriptor = &descriptor_i8,
    .name = slice_literal_fields("stack_allocation_offset_in_prolog"),
    .offset = offsetof(Function_Layout, stack_allocation_offset_in_prolog),
  },
  {
    .descriptor = &descriptor_i8,
    .name = slice_literal_fields("size_of_prolog"),
    .offset = offsetof(Function_Layout, size_of_prolog),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("begin_rva"),
    .offset = offsetof(Function_Layout, begin_rva),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("end_rva"),
    .offset = offsetof(Function_Layout, end_rva),
  },
  {
    .descriptor = &descriptor_i8_16,
    .name = slice_literal_fields("volatile_register_push_offsets"),
    .offset = offsetof(Function_Layout, volatile_register_push_offsets),
  },
);
MASS_DEFINE_TYPE_VALUE(function_layout);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_function_layout_ptr, function_layout_pointer, Array_Function_Layout_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_function_layout, function_layout, Array_Function_Layout);
DEFINE_VALUE_IS_AS_HELPERS(Function_Layout, function_layout);
DEFINE_VALUE_IS_AS_HELPERS(Function_Layout *, function_layout_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_context, Mass_Context,
  {
    .descriptor = &descriptor_allocator_pointer,
    .name = slice_literal_fields("allocator"),
    .offset = offsetof(Mass_Context, allocator),
  },
  {
    .descriptor = &descriptor_allocator_pointer,
    .name = slice_literal_fields("temp_allocator"),
    .offset = offsetof(Mass_Context, temp_allocator),
  },
  {
    .descriptor = &descriptor_compilation_pointer,
    .name = slice_literal_fields("compilation"),
    .offset = offsetof(Mass_Context, compilation),
  },
  {
    .descriptor = &descriptor_program_pointer,
    .name = slice_literal_fields("program"),
    .offset = offsetof(Mass_Context, program),
  },
  {
    .descriptor = &descriptor_mass_result_pointer,
    .name = slice_literal_fields("result"),
    .offset = offsetof(Mass_Context, result),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_context);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_mass_context_ptr, mass_context_pointer, Array_Mass_Context_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_mass_context, mass_context, Array_Mass_Context);
DEFINE_VALUE_IS_AS_HELPERS(Mass_Context, mass_context);
DEFINE_VALUE_IS_AS_HELPERS(Mass_Context *, mass_context_pointer);
MASS_DEFINE_OPAQUE_C_TYPE(parser_flags, Parser_Flags)
static C_Enum_Item parser_flags_items[] = {
{ .name = slice_literal_fields("None"), .value = 0 },
{ .name = slice_literal_fields("Global"), .value = 1 },
{ .name = slice_literal_fields("Type_Only"), .value = 2 },
};
DEFINE_VALUE_IS_AS_HELPERS(Parser_Flags, parser_flags);
DEFINE_VALUE_IS_AS_HELPERS(Parser_Flags *, parser_flags_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(parser, Parser,
  {
    .descriptor = &descriptor_parser_flags,
    .name = slice_literal_fields("flags"),
    .offset = offsetof(Parser, flags),
  },
  {
    .descriptor = &descriptor_epoch,
    .name = slice_literal_fields("epoch"),
    .offset = offsetof(Parser, epoch),
  },
  {
    .descriptor = &descriptor_scope_pointer,
    .name = slice_literal_fields("scope"),
    .offset = offsetof(Parser, scope),
  },
  {
    .descriptor = &descriptor_module_pointer,
    .name = slice_literal_fields("module"),
    .offset = offsetof(Parser, module),
  },
  {
    .descriptor = &descriptor_descriptor_pointer_pointer,
    .name = slice_literal_fields("return_descriptor_pointer"),
    .offset = offsetof(Parser, return_descriptor_pointer),
  },
);
MASS_DEFINE_TYPE_VALUE(parser);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_parser_ptr, parser_pointer, Array_Parser_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_parser, parser, Array_Parser);
DEFINE_VALUE_IS_AS_HELPERS(Parser, parser);
DEFINE_VALUE_IS_AS_HELPERS(Parser *, parser_pointer);
MASS_DEFINE_OPAQUE_C_TYPE(operator_fixity, Operator_Fixity)
static C_Enum_Item operator_fixity_items[] = {
{ .name = slice_literal_fields("Infix"), .value = 1 },
{ .name = slice_literal_fields("Prefix"), .value = 2 },
{ .name = slice_literal_fields("Postfix"), .value = 4 },
};
DEFINE_VALUE_IS_AS_HELPERS(Operator_Fixity, operator_fixity);
DEFINE_VALUE_IS_AS_HELPERS(Operator_Fixity *, operator_fixity_pointer);
MASS_DEFINE_OPAQUE_C_TYPE(operator_associativity, Operator_Associativity)
static C_Enum_Item operator_associativity_items[] = {
{ .name = slice_literal_fields("Left"), .value = 0 },
{ .name = slice_literal_fields("Right"), .value = 1 },
};
DEFINE_VALUE_IS_AS_HELPERS(Operator_Associativity, operator_associativity);
DEFINE_VALUE_IS_AS_HELPERS(Operator_Associativity *, operator_associativity_pointer);
MASS_DEFINE_OPAQUE_C_TYPE(operator_flags, Operator_Flags)
static C_Enum_Item operator_flags_items[] = {
{ .name = slice_literal_fields("None"), .value = 0 },
{ .name = slice_literal_fields("Optional_Rhs"), .value = 1 },
};
DEFINE_VALUE_IS_AS_HELPERS(Operator_Flags, operator_flags);
DEFINE_VALUE_IS_AS_HELPERS(Operator_Flags *, operator_flags_pointer);
/*union struct start */
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_operator_ptr, operator_pointer, Array_Operator_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_operator, operator, Array_Operator);
MASS_DEFINE_OPAQUE_C_TYPE(operator_tag, Operator_Tag)
static C_Enum_Item operator_tag_items[] = {
{ .name = slice_literal_fields("Alias"), .value = 0 },
{ .name = slice_literal_fields("Intrinsic"), .value = 1 },
};
MASS_DEFINE_STRUCT_DESCRIPTOR(operator_alias, Operator_Alias,
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("symbol"),
    .offset = offsetof(Operator_Alias, symbol),
  },
);
MASS_DEFINE_TYPE_VALUE(operator_alias);
MASS_DEFINE_STRUCT_DESCRIPTOR(operator_intrinsic, Operator_Intrinsic,
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("body"),
    .offset = offsetof(Operator_Intrinsic, body),
  },
);
MASS_DEFINE_TYPE_VALUE(operator_intrinsic);
MASS_DEFINE_STRUCT_DESCRIPTOR(operator, Operator,
  {
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_operator_tag,
    .offset = offsetof(Operator, tag),
  },
  {
    .descriptor = &descriptor_operator_flags,
    .name = slice_literal_fields("flags"),
    .offset = offsetof(Operator, flags),
  },
  {
    .descriptor = &descriptor_operator_fixity,
    .name = slice_literal_fields("fixity"),
    .offset = offsetof(Operator, fixity),
  },
  {
    .descriptor = &descriptor_operator_associativity,
    .name = slice_literal_fields("associativity"),
    .offset = offsetof(Operator, associativity),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("precedence"),
    .offset = offsetof(Operator, precedence),
  },
  {
    .name = slice_literal_fields("Alias"),
    .descriptor = &descriptor_operator_alias,
    .offset = offsetof(Operator, Alias),
  },
  {
    .name = slice_literal_fields("Intrinsic"),
    .descriptor = &descriptor_operator_intrinsic,
    .offset = offsetof(Operator, Intrinsic),
  },
);
MASS_DEFINE_TYPE_VALUE(operator);
DEFINE_VALUE_IS_AS_HELPERS(Operator, operator);
DEFINE_VALUE_IS_AS_HELPERS(Operator *, operator_pointer);
/*union struct end*/
MASS_DEFINE_STRUCT_DESCRIPTOR(scope_entry, Scope_Entry,
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("value"),
    .offset = offsetof(Scope_Entry, value),
  },
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("name"),
    .offset = offsetof(Scope_Entry, name),
  },
  {
    .descriptor = &descriptor_epoch,
    .name = slice_literal_fields("epoch"),
    .offset = offsetof(Scope_Entry, epoch),
  },
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("latest_forced_value"),
    .offset = offsetof(Scope_Entry, latest_forced_value),
  },
  {
    .descriptor = &descriptor_source_range,
    .name = slice_literal_fields("source_range"),
    .offset = offsetof(Scope_Entry, source_range),
  },
);
MASS_DEFINE_TYPE_VALUE(scope_entry);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_scope_entry_ptr, scope_entry_pointer, Array_Scope_Entry_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_scope_entry, scope_entry, Array_Scope_Entry);
DEFINE_VALUE_IS_AS_HELPERS(Scope_Entry, scope_entry);
DEFINE_VALUE_IS_AS_HELPERS(Scope_Entry *, scope_entry_pointer);
/*union struct start */
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_scope_ptr, scope_pointer, Array_Scope_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_scope, scope, Array_Scope);
MASS_DEFINE_OPAQUE_C_TYPE(scope_tag, Scope_Tag)
static C_Enum_Item scope_tag_items[] = {
{ .name = slice_literal_fields("Imperative"), .value = 0 },
{ .name = slice_literal_fields("Declarative"), .value = 1 },
};
MASS_DEFINE_STRUCT_DESCRIPTOR(scope_imperative, Scope_Imperative,
  {
    .descriptor = &descriptor_scope_entry,
    .name = slice_literal_fields("entry"),
    .offset = offsetof(Scope_Imperative, entry),
  },
);
MASS_DEFINE_TYPE_VALUE(scope_imperative);
MASS_DEFINE_STRUCT_DESCRIPTOR(scope_declarative, Scope_Declarative,
  {
    .descriptor = &descriptor_scope_map_pointer,
    .name = slice_literal_fields("map"),
    .offset = offsetof(Scope_Declarative, map),
  },
);
MASS_DEFINE_TYPE_VALUE(scope_declarative);
MASS_DEFINE_STRUCT_DESCRIPTOR(scope, Scope,
  {
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_scope_tag,
    .offset = offsetof(Scope, tag),
  },
  {
    .descriptor = &descriptor_allocator_pointer,
    .name = slice_literal_fields("allocator"),
    .offset = offsetof(Scope, allocator),
  },
  {
    .descriptor = &descriptor_scope_pointer,
    .name = slice_literal_fields("parent"),
    .offset = offsetof(Scope, parent),
  },
  {
    .name = slice_literal_fields("Imperative"),
    .descriptor = &descriptor_scope_imperative,
    .offset = offsetof(Scope, Imperative),
  },
  {
    .name = slice_literal_fields("Declarative"),
    .descriptor = &descriptor_scope_declarative,
    .offset = offsetof(Scope, Declarative),
  },
);
MASS_DEFINE_TYPE_VALUE(scope);
DEFINE_VALUE_IS_AS_HELPERS(Scope, scope);
DEFINE_VALUE_IS_AS_HELPERS(Scope *, scope_pointer);
/*union struct end*/
MASS_DEFINE_STRUCT_DESCRIPTOR(overload, Overload,
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("value"),
    .offset = offsetof(Overload, value),
  },
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("next"),
    .offset = offsetof(Overload, next),
  },
);
MASS_DEFINE_TYPE_VALUE(overload);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_overload_ptr, overload_pointer, Array_Overload_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_overload, overload, Array_Overload);
DEFINE_VALUE_IS_AS_HELPERS(Overload, overload);
DEFINE_VALUE_IS_AS_HELPERS(Overload *, overload_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(undecidable_match, Undecidable_Match,
  {
    .descriptor = &descriptor_function_info_pointer,
    .name = slice_literal_fields("info"),
    .offset = offsetof(Undecidable_Match, info),
  },
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("value"),
    .offset = offsetof(Undecidable_Match, value),
  },
);
MASS_DEFINE_TYPE_VALUE(undecidable_match);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_undecidable_match_ptr, undecidable_match_pointer, Array_Undecidable_Match_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_undecidable_match, undecidable_match, Array_Undecidable_Match);
DEFINE_VALUE_IS_AS_HELPERS(Undecidable_Match, undecidable_match);
DEFINE_VALUE_IS_AS_HELPERS(Undecidable_Match *, undecidable_match_pointer);
/*union struct start */
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_overload_match_ptr, overload_match_pointer, Array_Overload_Match_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_overload_match, overload_match, Array_Overload_Match);
MASS_DEFINE_OPAQUE_C_TYPE(overload_match_tag, Overload_Match_Tag)
static C_Enum_Item overload_match_tag_items[] = {
{ .name = slice_literal_fields("No_Match"), .value = 0 },
{ .name = slice_literal_fields("Undecidable"), .value = 1 },
{ .name = slice_literal_fields("Found"), .value = 2 },
};
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(overload_match_no_match, 0);
MASS_DEFINE_STRUCT_DESCRIPTOR(overload_match_undecidable, Overload_Match_Undecidable,
  {
    .descriptor = &descriptor_array_undecidable_match,
    .name = slice_literal_fields("matches"),
    .offset = offsetof(Overload_Match_Undecidable, matches),
  },
);
MASS_DEFINE_TYPE_VALUE(overload_match_undecidable);
MASS_DEFINE_STRUCT_DESCRIPTOR(overload_match_found, Overload_Match_Found,
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("value"),
    .offset = offsetof(Overload_Match_Found, value),
  },
  {
    .descriptor = &descriptor_function_info_pointer,
    .name = slice_literal_fields("info"),
    .offset = offsetof(Overload_Match_Found, info),
  },
);
MASS_DEFINE_TYPE_VALUE(overload_match_found);
MASS_DEFINE_STRUCT_DESCRIPTOR(overload_match, Overload_Match,
  {
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_overload_match_tag,
    .offset = offsetof(Overload_Match, tag),
  },
  {
    .name = slice_literal_fields("Undecidable"),
    .descriptor = &descriptor_overload_match_undecidable,
    .offset = offsetof(Overload_Match, Undecidable),
  },
  {
    .name = slice_literal_fields("Found"),
    .descriptor = &descriptor_overload_match_found,
    .offset = offsetof(Overload_Match, Found),
  },
  {
    .name = slice_literal_fields("No_Match"),
    .descriptor = &descriptor_overload_match_no_match,
    .offset = offsetof(Overload_Match, Found)/*:EmptyVariantOffset*/,
  },
);
MASS_DEFINE_TYPE_VALUE(overload_match);
DEFINE_VALUE_IS_AS_HELPERS(Overload_Match, overload_match);
DEFINE_VALUE_IS_AS_HELPERS(Overload_Match *, overload_match_pointer);
/*union struct end*/
MASS_DEFINE_STRUCT_DESCRIPTOR(overload_match_summary, Overload_Match_Summary,
  {
    .descriptor = &descriptor_i16,
    .name = slice_literal_fields("inverted_generic_count"),
    .offset = offsetof(Overload_Match_Summary, inverted_generic_count),
  },
  {
    .descriptor = &descriptor_i16,
    .name = slice_literal_fields("inverted_cast_count"),
    .offset = offsetof(Overload_Match_Summary, inverted_cast_count),
  },
  {
    .descriptor = &descriptor_i16,
    .name = slice_literal_fields("exact_count"),
    .offset = offsetof(Overload_Match_Summary, exact_count),
  },
  {
    .descriptor = &descriptor__bool,
    .name = slice_literal_fields("compile_time"),
    .offset = offsetof(Overload_Match_Summary, compile_time),
  },
  {
    .descriptor = &descriptor__bool,
    .name = slice_literal_fields("matched"),
    .offset = offsetof(Overload_Match_Summary, matched),
  },
);
MASS_DEFINE_TYPE_VALUE(overload_match_summary);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_overload_match_summary_ptr, overload_match_summary_pointer, Array_Overload_Match_Summary_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_overload_match_summary, overload_match_summary, Array_Overload_Match_Summary);
DEFINE_VALUE_IS_AS_HELPERS(Overload_Match_Summary, overload_match_summary);
DEFINE_VALUE_IS_AS_HELPERS(Overload_Match_Summary *, overload_match_summary_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(overload_match_state, Overload_Match_State,
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("value"),
    .offset = offsetof(Overload_Match_State, value),
  },
  {
    .descriptor = &descriptor_function_info_pointer,
    .name = slice_literal_fields("info"),
    .offset = offsetof(Overload_Match_State, info),
  },
  {
    .descriptor = &descriptor_overload_match_summary,
    .name = slice_literal_fields("summary"),
    .offset = offsetof(Overload_Match_State, summary),
  },
);
MASS_DEFINE_TYPE_VALUE(overload_match_state);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_overload_match_state_ptr, overload_match_state_pointer, Array_Overload_Match_State_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_overload_match_state, overload_match_state, Array_Overload_Match_State);
DEFINE_VALUE_IS_AS_HELPERS(Overload_Match_State, overload_match_state);
DEFINE_VALUE_IS_AS_HELPERS(Overload_Match_State *, overload_match_state_pointer);
MASS_DEFINE_OPAQUE_C_TYPE(value_flags, Value_Flags)
static C_Enum_Item value_flags_items[] = {
{ .name = slice_literal_fields("None"), .value = 0 },
{ .name = slice_literal_fields("Constant"), .value = 1 },
};
DEFINE_VALUE_IS_AS_HELPERS(Value_Flags, value_flags);
DEFINE_VALUE_IS_AS_HELPERS(Value_Flags *, value_flags_pointer);
/*union struct start */
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_value_ptr, value_pointer, Array_Value_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_value, value, Array_Value);
MASS_DEFINE_OPAQUE_C_TYPE(value_tag, Value_Tag)
static C_Enum_Item value_tag_items[] = {
{ .name = slice_literal_fields("Lazy"), .value = 0 },
{ .name = slice_literal_fields("Forced"), .value = 1 },
};
MASS_DEFINE_STRUCT_DESCRIPTOR(value_lazy, Value_Lazy,
  {
    .descriptor = &descriptor__bool,
    .name = slice_literal_fields("is_factory"),
    .offset = offsetof(Value_Lazy, is_factory),
  },
  {
    .descriptor = &descriptor_epoch,
    .name = slice_literal_fields("epoch"),
    .offset = offsetof(Value_Lazy, epoch),
  },
  {
    .descriptor = &descriptor_void_pointer,
    .name = slice_literal_fields("payload"),
    .offset = offsetof(Value_Lazy, payload),
  },
  {
    .descriptor = &descriptor_scope_pointer,
    .name = slice_literal_fields("scope"),
    .offset = offsetof(Value_Lazy, scope),
  },
  {
    .descriptor = &descriptor_lazy_value_proc,
    .name = slice_literal_fields("proc"),
    .offset = offsetof(Value_Lazy, proc),
  },
);
MASS_DEFINE_TYPE_VALUE(value_lazy);
MASS_DEFINE_STRUCT_DESCRIPTOR(value_forced, Value_Forced,
  {
    .descriptor = &descriptor_storage,
    .name = slice_literal_fields("storage"),
    .offset = offsetof(Value_Forced, storage),
  },
);
MASS_DEFINE_TYPE_VALUE(value_forced);
MASS_DEFINE_STRUCT_DESCRIPTOR(value, Value,
  {
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_value_tag,
    .offset = offsetof(Value, tag),
  },
  {
    .descriptor = &descriptor_value_flags,
    .name = slice_literal_fields("flags"),
    .offset = offsetof(Value, flags),
  },
  {
    .descriptor = &descriptor_descriptor_pointer,
    .name = slice_literal_fields("descriptor"),
    .offset = offsetof(Value, descriptor),
  },
  {
    .descriptor = &descriptor_source_range,
    .name = slice_literal_fields("source_range"),
    .offset = offsetof(Value, source_range),
  },
  {
    .name = slice_literal_fields("Lazy"),
    .descriptor = &descriptor_value_lazy,
    .offset = offsetof(Value, Lazy),
  },
  {
    .name = slice_literal_fields("Forced"),
    .descriptor = &descriptor_value_forced,
    .offset = offsetof(Value, Forced),
  },
);
MASS_DEFINE_TYPE_VALUE(value);
DEFINE_VALUE_IS_AS_HELPERS(Value, value);
DEFINE_VALUE_IS_AS_HELPERS(Value *, value_pointer);
/*union struct end*/
MASS_DEFINE_STRUCT_DESCRIPTOR(register_bitset, Register_Bitset,
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("bits"),
    .offset = offsetof(Register_Bitset, bits),
  },
);
MASS_DEFINE_TYPE_VALUE(register_bitset);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_register_bitset_ptr, register_bitset_pointer, Array_Register_Bitset_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_register_bitset, register_bitset, Array_Register_Bitset);
DEFINE_VALUE_IS_AS_HELPERS(Register_Bitset, register_bitset);
DEFINE_VALUE_IS_AS_HELPERS(Register_Bitset *, register_bitset_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(function_builder, Function_Builder,
  {
    .descriptor = &descriptor_epoch,
    .name = slice_literal_fields("epoch"),
    .offset = offsetof(Function_Builder, epoch),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("stack_reserve"),
    .offset = offsetof(Function_Builder, stack_reserve),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("max_call_parameters_stack_size"),
    .offset = offsetof(Function_Builder, max_call_parameters_stack_size),
  },
  {
    .descriptor = &descriptor_value,
    .name = slice_literal_fields("return_value"),
    .offset = offsetof(Function_Builder, return_value),
  },
  {
    .descriptor = &descriptor_code_block,
    .name = slice_literal_fields("code_block"),
    .offset = offsetof(Function_Builder, code_block),
  },
  {
    .descriptor = &descriptor_register_bitset,
    .name = slice_literal_fields("register_used_bitset"),
    .offset = offsetof(Function_Builder, register_used_bitset),
  },
  {
    .descriptor = &descriptor_register_bitset,
    .name = slice_literal_fields("register_volatile_bitset"),
    .offset = offsetof(Function_Builder, register_volatile_bitset),
  },
  {
    .descriptor = &descriptor_register_bitset,
    .name = slice_literal_fields("register_occupied_bitset"),
    .offset = offsetof(Function_Builder, register_occupied_bitset),
  },
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("source"),
    .offset = offsetof(Function_Builder, source),
  },
  {
    .descriptor = &descriptor_function_info_pointer,
    .name = slice_literal_fields("function"),
    .offset = offsetof(Function_Builder, function),
  },
);
MASS_DEFINE_TYPE_VALUE(function_builder);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_function_builder_ptr, function_builder_pointer, Array_Function_Builder_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_function_builder, function_builder, Array_Function_Builder);
DEFINE_VALUE_IS_AS_HELPERS(Function_Builder, function_builder);
DEFINE_VALUE_IS_AS_HELPERS(Function_Builder *, function_builder_pointer);
/*union struct start */
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_expected_result_ptr, expected_result_pointer, Array_Expected_Result_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_expected_result, expected_result, Array_Expected_Result);
MASS_DEFINE_OPAQUE_C_TYPE(expected_result_tag, Expected_Result_Tag)
static C_Enum_Item expected_result_tag_items[] = {
{ .name = slice_literal_fields("Exact"), .value = 0 },
{ .name = slice_literal_fields("Flexible"), .value = 1 },
};
MASS_DEFINE_STRUCT_DESCRIPTOR(expected_result_exact, Expected_Result_Exact,
  {
    .descriptor = &descriptor_descriptor_pointer,
    .name = slice_literal_fields("descriptor"),
    .offset = offsetof(Expected_Result_Exact, descriptor),
  },
  {
    .descriptor = &descriptor_storage,
    .name = slice_literal_fields("storage"),
    .offset = offsetof(Expected_Result_Exact, storage),
  },
);
MASS_DEFINE_TYPE_VALUE(expected_result_exact);
MASS_DEFINE_STRUCT_DESCRIPTOR(expected_result_flexible, Expected_Result_Flexible,
  {
    .descriptor = &descriptor_descriptor_pointer,
    .name = slice_literal_fields("descriptor"),
    .offset = offsetof(Expected_Result_Flexible, descriptor),
  },
);
MASS_DEFINE_TYPE_VALUE(expected_result_flexible);
MASS_DEFINE_STRUCT_DESCRIPTOR(expected_result, Expected_Result,
  {
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_expected_result_tag,
    .offset = offsetof(Expected_Result, tag),
  },
  {
    .name = slice_literal_fields("Exact"),
    .descriptor = &descriptor_expected_result_exact,
    .offset = offsetof(Expected_Result, Exact),
  },
  {
    .name = slice_literal_fields("Flexible"),
    .descriptor = &descriptor_expected_result_flexible,
    .offset = offsetof(Expected_Result, Flexible),
  },
);
MASS_DEFINE_TYPE_VALUE(expected_result);
DEFINE_VALUE_IS_AS_HELPERS(Expected_Result, expected_result);
DEFINE_VALUE_IS_AS_HELPERS(Expected_Result *, expected_result_pointer);
/*union struct end*/
MASS_DEFINE_STRUCT_DESCRIPTOR(lazy_static_value, Lazy_Static_Value,
  {
    .descriptor = &descriptor_mass_context,
    .name = slice_literal_fields("context"),
    .offset = offsetof(Lazy_Static_Value, context),
  },
  {
    .descriptor = &descriptor_parser,
    .name = slice_literal_fields("parser"),
    .offset = offsetof(Lazy_Static_Value, parser),
  },
  {
    .descriptor = &descriptor_value_view,
    .name = slice_literal_fields("expression"),
    .offset = offsetof(Lazy_Static_Value, expression),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("resolving"),
    .offset = offsetof(Lazy_Static_Value, resolving),
  },
);
MASS_DEFINE_TYPE_VALUE(lazy_static_value);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_lazy_static_value_ptr, lazy_static_value_pointer, Array_Lazy_Static_Value_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_lazy_static_value, lazy_static_value, Array_Lazy_Static_Value);
DEFINE_VALUE_IS_AS_HELPERS(Lazy_Static_Value, lazy_static_value);
DEFINE_VALUE_IS_AS_HELPERS(Lazy_Static_Value *, lazy_static_value_pointer);
MASS_DEFINE_FUNCTION_DESCRIPTOR(
  mass_intrinsic_proc,
  &descriptor_value_pointer,
  {
    .tag = Resolved_Function_Parameter_Tag_Unknown,
    .descriptor = &descriptor_mass_context_pointer,
  },
  {
    .tag = Resolved_Function_Parameter_Tag_Unknown,
    .descriptor = &descriptor_parser_pointer,
  },
  {
    .tag = Resolved_Function_Parameter_Tag_Unknown,
    .descriptor = &descriptor_value_view,
  }
)
/*union struct start */
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_function_parameter_ptr, function_parameter_pointer, Array_Function_Parameter_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_function_parameter, function_parameter, Array_Function_Parameter);
MASS_DEFINE_OPAQUE_C_TYPE(function_parameter_tag, Function_Parameter_Tag)
static C_Enum_Item function_parameter_tag_items[] = {
{ .name = slice_literal_fields("Runtime"), .value = 0 },
{ .name = slice_literal_fields("Generic"), .value = 1 },
{ .name = slice_literal_fields("Exact_Static"), .value = 2 },
};
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(function_parameter_runtime, 0);
MASS_DEFINE_STRUCT_DESCRIPTOR(function_parameter_generic, Function_Parameter_Generic,
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("is_static"),
    .offset = offsetof(Function_Parameter_Generic, is_static),
  },
  {
    .descriptor = &descriptor_mass_type_constraint_proc,
    .name = slice_literal_fields("maybe_type_constraint"),
    .offset = offsetof(Function_Parameter_Generic, maybe_type_constraint),
  },
);
MASS_DEFINE_TYPE_VALUE(function_parameter_generic);
MASS_DEFINE_STRUCT_DESCRIPTOR(function_parameter_exact_static, Function_Parameter_Exact_Static,
  {
    .descriptor = &descriptor_storage,
    .name = slice_literal_fields("storage"),
    .offset = offsetof(Function_Parameter_Exact_Static, storage),
  },
);
MASS_DEFINE_TYPE_VALUE(function_parameter_exact_static);
MASS_DEFINE_STRUCT_DESCRIPTOR(function_parameter, Function_Parameter,
  {
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_function_parameter_tag,
    .offset = offsetof(Function_Parameter, tag),
  },
  {
    .descriptor = &descriptor_descriptor_pointer,
    .name = slice_literal_fields("descriptor"),
    .offset = offsetof(Function_Parameter, descriptor),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("symbol"),
    .offset = offsetof(Function_Parameter, symbol),
  },
  {
    .descriptor = &descriptor_source_range,
    .name = slice_literal_fields("source_range"),
    .offset = offsetof(Function_Parameter, source_range),
  },
  {
    .descriptor = &descriptor_value_view,
    .name = slice_literal_fields("maybe_type_expression"),
    .offset = offsetof(Function_Parameter, maybe_type_expression),
  },
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("maybe_default_value"),
    .offset = offsetof(Function_Parameter, maybe_default_value),
  },
  {
    .name = slice_literal_fields("Generic"),
    .descriptor = &descriptor_function_parameter_generic,
    .offset = offsetof(Function_Parameter, Generic),
  },
  {
    .name = slice_literal_fields("Exact_Static"),
    .descriptor = &descriptor_function_parameter_exact_static,
    .offset = offsetof(Function_Parameter, Exact_Static),
  },
  {
    .name = slice_literal_fields("Runtime"),
    .descriptor = &descriptor_function_parameter_runtime,
    .offset = offsetof(Function_Parameter, Exact_Static)/*:EmptyVariantOffset*/,
  },
);
MASS_DEFINE_TYPE_VALUE(function_parameter);
DEFINE_VALUE_IS_AS_HELPERS(Function_Parameter, function_parameter);
DEFINE_VALUE_IS_AS_HELPERS(Function_Parameter *, function_parameter_pointer);
/*union struct end*/
/*union struct start */
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_resolved_function_parameter_ptr, resolved_function_parameter_pointer, Array_Resolved_Function_Parameter_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_resolved_function_parameter, resolved_function_parameter, Array_Resolved_Function_Parameter);
MASS_DEFINE_OPAQUE_C_TYPE(resolved_function_parameter_tag, Resolved_Function_Parameter_Tag)
static C_Enum_Item resolved_function_parameter_tag_items[] = {
{ .name = slice_literal_fields("Unknown"), .value = 0 },
{ .name = slice_literal_fields("Known"), .value = 1 },
};
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(resolved_function_parameter_unknown, 0);
MASS_DEFINE_STRUCT_DESCRIPTOR(resolved_function_parameter_known, Resolved_Function_Parameter_Known,
  {
    .descriptor = &descriptor_storage,
    .name = slice_literal_fields("storage"),
    .offset = offsetof(Resolved_Function_Parameter_Known, storage),
  },
);
MASS_DEFINE_TYPE_VALUE(resolved_function_parameter_known);
MASS_DEFINE_STRUCT_DESCRIPTOR(resolved_function_parameter, Resolved_Function_Parameter,
  {
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_resolved_function_parameter_tag,
    .offset = offsetof(Resolved_Function_Parameter, tag),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("was_generic"),
    .offset = offsetof(Resolved_Function_Parameter, was_generic),
  },
  {
    .descriptor = &descriptor_descriptor_pointer,
    .name = slice_literal_fields("descriptor"),
    .offset = offsetof(Resolved_Function_Parameter, descriptor),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("symbol"),
    .offset = offsetof(Resolved_Function_Parameter, symbol),
  },
  {
    .descriptor = &descriptor_source_range,
    .name = slice_literal_fields("source_range"),
    .offset = offsetof(Resolved_Function_Parameter, source_range),
  },
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("maybe_default_value"),
    .offset = offsetof(Resolved_Function_Parameter, maybe_default_value),
  },
  {
    .name = slice_literal_fields("Known"),
    .descriptor = &descriptor_resolved_function_parameter_known,
    .offset = offsetof(Resolved_Function_Parameter, Known),
  },
  {
    .name = slice_literal_fields("Unknown"),
    .descriptor = &descriptor_resolved_function_parameter_unknown,
    .offset = offsetof(Resolved_Function_Parameter, Known)/*:EmptyVariantOffset*/,
  },
);
MASS_DEFINE_TYPE_VALUE(resolved_function_parameter);
DEFINE_VALUE_IS_AS_HELPERS(Resolved_Function_Parameter, resolved_function_parameter);
DEFINE_VALUE_IS_AS_HELPERS(Resolved_Function_Parameter *, resolved_function_parameter_pointer);
/*union struct end*/
MASS_DEFINE_OPAQUE_C_TYPE(function_info_flags, Function_Info_Flags)
static C_Enum_Item function_info_flags_items[] = {
{ .name = slice_literal_fields("None"), .value = 0 },
{ .name = slice_literal_fields("Compile_Time"), .value = 2 },
{ .name = slice_literal_fields("Intrinsic"), .value = 4 },
};
DEFINE_VALUE_IS_AS_HELPERS(Function_Info_Flags, function_info_flags);
DEFINE_VALUE_IS_AS_HELPERS(Function_Info_Flags *, function_info_flags_pointer);
/*union struct start */
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_function_return_ptr, function_return_pointer, Array_Function_Return_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_function_return, function_return, Array_Function_Return);
MASS_DEFINE_OPAQUE_C_TYPE(function_return_tag, Function_Return_Tag)
static C_Enum_Item function_return_tag_items[] = {
{ .name = slice_literal_fields("Inferred"), .value = 0 },
{ .name = slice_literal_fields("Generic"), .value = 1 },
{ .name = slice_literal_fields("Exact"), .value = 2 },
};
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(function_return_inferred, 0);
MASS_DEFINE_STRUCT_DESCRIPTOR(function_return_generic, Function_Return_Generic,
  {
    .descriptor = &descriptor_value_view,
    .name = slice_literal_fields("type_expression"),
    .offset = offsetof(Function_Return_Generic, type_expression),
  },
);
MASS_DEFINE_TYPE_VALUE(function_return_generic);
MASS_DEFINE_STRUCT_DESCRIPTOR(function_return_exact, Function_Return_Exact,
  {
    .descriptor = &descriptor_descriptor_pointer,
    .name = slice_literal_fields("descriptor"),
    .offset = offsetof(Function_Return_Exact, descriptor),
  },
);
MASS_DEFINE_TYPE_VALUE(function_return_exact);
MASS_DEFINE_STRUCT_DESCRIPTOR(function_return, Function_Return,
  {
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_function_return_tag,
    .offset = offsetof(Function_Return, tag),
  },
  {
    .descriptor = &descriptor_source_range,
    .name = slice_literal_fields("source_range"),
    .offset = offsetof(Function_Return, source_range),
  },
  {
    .name = slice_literal_fields("Generic"),
    .descriptor = &descriptor_function_return_generic,
    .offset = offsetof(Function_Return, Generic),
  },
  {
    .name = slice_literal_fields("Exact"),
    .descriptor = &descriptor_function_return_exact,
    .offset = offsetof(Function_Return, Exact),
  },
  {
    .name = slice_literal_fields("Inferred"),
    .descriptor = &descriptor_function_return_inferred,
    .offset = offsetof(Function_Return, Exact)/*:EmptyVariantOffset*/,
  },
);
MASS_DEFINE_TYPE_VALUE(function_return);
DEFINE_VALUE_IS_AS_HELPERS(Function_Return, function_return);
DEFINE_VALUE_IS_AS_HELPERS(Function_Return *, function_return_pointer);
/*union struct end*/
MASS_DEFINE_STRUCT_DESCRIPTOR(function_info, Function_Info,
  {
    .descriptor = &descriptor_function_info_flags,
    .name = slice_literal_fields("flags"),
    .offset = offsetof(Function_Info, flags),
  },
  {
    .descriptor = &descriptor_array_resolved_function_parameter,
    .name = slice_literal_fields("parameters"),
    .offset = offsetof(Function_Info, parameters),
  },
  {
    .descriptor = &descriptor_descriptor_pointer,
    .name = slice_literal_fields("return_descriptor"),
    .offset = offsetof(Function_Info, return_descriptor),
  },
);
MASS_DEFINE_TYPE_VALUE(function_info);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_function_info_ptr, function_info_pointer, Array_Function_Info_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_function_info, function_info, Array_Function_Info);
DEFINE_VALUE_IS_AS_HELPERS(Function_Info, function_info);
DEFINE_VALUE_IS_AS_HELPERS(Function_Info *, function_info_pointer);
MASS_DEFINE_OPAQUE_C_TYPE(function_header_flags, Function_Header_Flags)
static C_Enum_Item function_header_flags_items[] = {
{ .name = slice_literal_fields("None"), .value = 0 },
{ .name = slice_literal_fields("Intrinsic"), .value = 2 },
{ .name = slice_literal_fields("Compile_Time"), .value = 4 },
};
DEFINE_VALUE_IS_AS_HELPERS(Function_Header_Flags, function_header_flags);
DEFINE_VALUE_IS_AS_HELPERS(Function_Header_Flags *, function_header_flags_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(function_specialization, Function_Specialization,
  {
    .descriptor = &descriptor_array_function_parameter,
    .name = slice_literal_fields("parameters"),
    .offset = offsetof(Function_Specialization, parameters),
  },
  {
    .descriptor = &descriptor_function_info_pointer,
    .name = slice_literal_fields("info"),
    .offset = offsetof(Function_Specialization, info),
  },
);
MASS_DEFINE_TYPE_VALUE(function_specialization);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_function_specialization_ptr, function_specialization_pointer, Array_Function_Specialization_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_function_specialization, function_specialization, Array_Function_Specialization);
DEFINE_VALUE_IS_AS_HELPERS(Function_Specialization, function_specialization);
DEFINE_VALUE_IS_AS_HELPERS(Function_Specialization *, function_specialization_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(function_header, Function_Header,
  {
    .descriptor = &descriptor_function_header_flags,
    .name = slice_literal_fields("flags"),
    .offset = offsetof(Function_Header, flags),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("generic_parameter_count"),
    .offset = offsetof(Function_Header, generic_parameter_count),
  },
  {
    .descriptor = &descriptor_array_function_parameter,
    .name = slice_literal_fields("parameters"),
    .offset = offsetof(Function_Header, parameters),
  },
  {
    .descriptor = &descriptor_function_return,
    .name = slice_literal_fields("returns"),
    .offset = offsetof(Function_Header, returns),
  },
);
MASS_DEFINE_TYPE_VALUE(function_header);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_function_header_ptr, function_header_pointer, Array_Function_Header_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_function_header, function_header, Array_Function_Header);
DEFINE_VALUE_IS_AS_HELPERS(Function_Header, function_header);
DEFINE_VALUE_IS_AS_HELPERS(Function_Header *, function_header_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(function_literal, Function_Literal,
  {
    .descriptor = &descriptor_function_header,
    .name = slice_literal_fields("header"),
    .offset = offsetof(Function_Literal, header),
  },
  {
    .descriptor = &descriptor_scope_pointer,
    .name = slice_literal_fields("own_scope"),
    .offset = offsetof(Function_Literal, own_scope),
  },
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("body"),
    .offset = offsetof(Function_Literal, body),
  },
  {
    .descriptor = &descriptor_i64_pointer,
    .name = slice_literal_fields("overload_lock_count"),
    .offset = offsetof(Function_Literal, overload_lock_count),
  },
  {
    .descriptor = &descriptor_array_value_ptr,
    .name = slice_literal_fields("instances"),
    .offset = offsetof(Function_Literal, instances),
  },
  {
    .descriptor = &descriptor_array_function_specialization,
    .name = slice_literal_fields("specializations"),
    .offset = offsetof(Function_Literal, specializations),
  },
);
MASS_DEFINE_TYPE_VALUE(function_literal);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_function_literal_ptr, function_literal_pointer, Array_Function_Literal_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_function_literal, function_literal, Array_Function_Literal);
DEFINE_VALUE_IS_AS_HELPERS(Function_Literal, function_literal);
DEFINE_VALUE_IS_AS_HELPERS(Function_Literal *, function_literal_pointer);
MASS_DEFINE_OPAQUE_C_TYPE(function_call_parameter_flags, Function_Call_Parameter_Flags)
static C_Enum_Item function_call_parameter_flags_items[] = {
{ .name = slice_literal_fields("None"), .value = 0 },
{ .name = slice_literal_fields("Uninitialized"), .value = 1 },
{ .name = slice_literal_fields("Implicit_Pointer"), .value = 2 },
};
DEFINE_VALUE_IS_AS_HELPERS(Function_Call_Parameter_Flags, function_call_parameter_flags);
DEFINE_VALUE_IS_AS_HELPERS(Function_Call_Parameter_Flags *, function_call_parameter_flags_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(function_call_parameter, Function_Call_Parameter,
  {
    .descriptor = &descriptor_function_call_parameter_flags,
    .name = slice_literal_fields("flags"),
    .offset = offsetof(Function_Call_Parameter, flags),
  },
  {
    .descriptor = &descriptor_descriptor_pointer,
    .name = slice_literal_fields("descriptor"),
    .offset = offsetof(Function_Call_Parameter, descriptor),
  },
  {
    .descriptor = &descriptor_storage,
    .name = slice_literal_fields("storage"),
    .offset = offsetof(Function_Call_Parameter, storage),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("original_index"),
    .offset = offsetof(Function_Call_Parameter, original_index),
  },
);
MASS_DEFINE_TYPE_VALUE(function_call_parameter);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_function_call_parameter_ptr, function_call_parameter_pointer, Array_Function_Call_Parameter_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_function_call_parameter, function_call_parameter, Array_Function_Call_Parameter);
DEFINE_VALUE_IS_AS_HELPERS(Function_Call_Parameter, function_call_parameter);
DEFINE_VALUE_IS_AS_HELPERS(Function_Call_Parameter *, function_call_parameter_pointer);
MASS_DEFINE_FUNCTION_DESCRIPTOR(
  mass_call_encode_proc,
  &descriptor_void,
  {
    .tag = Resolved_Function_Parameter_Tag_Unknown,
    .descriptor = &descriptor_function_builder_pointer,
  },
  {
    .tag = Resolved_Function_Parameter_Tag_Unknown,
    .descriptor = &descriptor_storage,
  },
  {
    .tag = Resolved_Function_Parameter_Tag_Unknown,
    .descriptor = &descriptor_source_range_pointer,
  },
  {
    .tag = Resolved_Function_Parameter_Tag_Unknown,
    .descriptor = &descriptor_scope_pointer,
  }
)
MASS_DEFINE_STRUCT_DESCRIPTOR(function_call_setup, Function_Call_Setup,
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("parameters_stack_size"),
    .offset = offsetof(Function_Call_Setup, parameters_stack_size),
  },
  {
    .descriptor = &descriptor_mass_call_encode_proc,
    .name = slice_literal_fields("call_encode_proc"),
    .offset = offsetof(Function_Call_Setup, call_encode_proc),
  },
  {
    .descriptor = &descriptor_calling_convention_pointer,
    .name = slice_literal_fields("calling_convention"),
    .offset = offsetof(Function_Call_Setup, calling_convention),
  },
  {
    .descriptor = &descriptor_array_function_call_parameter,
    .name = slice_literal_fields("parameters"),
    .offset = offsetof(Function_Call_Setup, parameters),
  },
  {
    .descriptor = &descriptor_register_bitset,
    .name = slice_literal_fields("parameter_registers_bitset"),
    .offset = offsetof(Function_Call_Setup, parameter_registers_bitset),
  },
  {
    .descriptor = &descriptor_storage,
    .name = slice_literal_fields("caller_return"),
    .offset = offsetof(Function_Call_Setup, caller_return),
  },
  {
    .descriptor = &descriptor_storage,
    .name = slice_literal_fields("callee_return"),
    .offset = offsetof(Function_Call_Setup, callee_return),
  },
);
MASS_DEFINE_TYPE_VALUE(function_call_setup);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_function_call_setup_ptr, function_call_setup_pointer, Array_Function_Call_Setup_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_function_call_setup, function_call_setup, Array_Function_Call_Setup);
DEFINE_VALUE_IS_AS_HELPERS(Function_Call_Setup, function_call_setup);
DEFINE_VALUE_IS_AS_HELPERS(Function_Call_Setup *, function_call_setup_pointer);
MASS_DEFINE_OPAQUE_C_TYPE(system_v_argument_class, SYSTEM_V_ARGUMENT_CLASS)
static C_Enum_Item system_v_argument_class_items[] = {
{ .name = slice_literal_fields("NO_CLASS"), .value = 0 },
{ .name = slice_literal_fields("INTEGER"), .value = 1 },
{ .name = slice_literal_fields("SSE"), .value = 2 },
{ .name = slice_literal_fields("SSEUP"), .value = 3 },
{ .name = slice_literal_fields("X87"), .value = 4 },
{ .name = slice_literal_fields("X87UP"), .value = 5 },
{ .name = slice_literal_fields("COMPLEX_X87"), .value = 6 },
{ .name = slice_literal_fields("MEMORY"), .value = 7 },
};
DEFINE_VALUE_IS_AS_HELPERS(SYSTEM_V_ARGUMENT_CLASS, system_v_argument_class);
DEFINE_VALUE_IS_AS_HELPERS(SYSTEM_V_ARGUMENT_CLASS *, system_v_argument_class_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(system_v_classification, System_V_Classification,
  {
    .descriptor = &descriptor_system_v_argument_class,
    .name = slice_literal_fields("class"),
    .offset = offsetof(System_V_Classification, class),
  },
  {
    .descriptor = &descriptor_descriptor_pointer,
    .name = slice_literal_fields("descriptor"),
    .offset = offsetof(System_V_Classification, descriptor),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("eightbyte_count"),
    .offset = offsetof(System_V_Classification, eightbyte_count),
  },
);
MASS_DEFINE_TYPE_VALUE(system_v_classification);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_system_v_classification_ptr, system_v_classification_pointer, Array_System_V_Classification_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_system_v_classification, system_v_classification, Array_System_V_Classification);
DEFINE_VALUE_IS_AS_HELPERS(System_V_Classification, system_v_classification);
DEFINE_VALUE_IS_AS_HELPERS(System_V_Classification *, system_v_classification_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(system_v_registers, System_V_Registers,
  {
    .descriptor = &descriptor_register_pointer,
    .name = slice_literal_fields("items"),
    .offset = offsetof(System_V_Registers, items),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("count"),
    .offset = offsetof(System_V_Registers, count),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("index"),
    .offset = offsetof(System_V_Registers, index),
  },
);
MASS_DEFINE_TYPE_VALUE(system_v_registers);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_system_v_registers_ptr, system_v_registers_pointer, Array_System_V_Registers_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_system_v_registers, system_v_registers, Array_System_V_Registers);
DEFINE_VALUE_IS_AS_HELPERS(System_V_Registers, system_v_registers);
DEFINE_VALUE_IS_AS_HELPERS(System_V_Registers *, system_v_registers_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(system_v_register_state, System_V_Register_State,
  {
    .descriptor = &descriptor_system_v_registers,
    .name = slice_literal_fields("general"),
    .offset = offsetof(System_V_Register_State, general),
  },
  {
    .descriptor = &descriptor_system_v_registers,
    .name = slice_literal_fields("vector"),
    .offset = offsetof(System_V_Register_State, vector),
  },
);
MASS_DEFINE_TYPE_VALUE(system_v_register_state);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_system_v_register_state_ptr, system_v_register_state_pointer, Array_System_V_Register_State_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_system_v_register_state, system_v_register_state, Array_System_V_Register_State);
DEFINE_VALUE_IS_AS_HELPERS(System_V_Register_State, system_v_register_state);
DEFINE_VALUE_IS_AS_HELPERS(System_V_Register_State *, system_v_register_state_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(system_v_eightbyte_array, System_V_Eightbyte_Array,
  {
    .descriptor = &descriptor_system_v_argument_class_8,
    .name = slice_literal_fields("classes"),
    .offset = offsetof(System_V_Eightbyte_Array, classes),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("count"),
    .offset = offsetof(System_V_Eightbyte_Array, count),
  },
);
MASS_DEFINE_TYPE_VALUE(system_v_eightbyte_array);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_system_v_eightbyte_array_ptr, system_v_eightbyte_array_pointer, Array_System_V_Eightbyte_Array_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_system_v_eightbyte_array, system_v_eightbyte_array, Array_System_V_Eightbyte_Array);
DEFINE_VALUE_IS_AS_HELPERS(System_V_Eightbyte_Array, system_v_eightbyte_array);
DEFINE_VALUE_IS_AS_HELPERS(System_V_Eightbyte_Array *, system_v_eightbyte_array_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_function_call_lazy_payload, Mass_Function_Call_Lazy_Payload,
  {
    .descriptor = &descriptor_value_view,
    .name = slice_literal_fields("args"),
    .offset = offsetof(Mass_Function_Call_Lazy_Payload, args),
  },
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("overload"),
    .offset = offsetof(Mass_Function_Call_Lazy_Payload, overload),
  },
  {
    .descriptor = &descriptor_function_info_pointer,
    .name = slice_literal_fields("info"),
    .offset = offsetof(Mass_Function_Call_Lazy_Payload, info),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_function_call_lazy_payload);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_mass_function_call_lazy_payload_ptr, mass_function_call_lazy_payload_pointer, Array_Mass_Function_Call_Lazy_Payload_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_mass_function_call_lazy_payload, mass_function_call_lazy_payload, Array_Mass_Function_Call_Lazy_Payload);
DEFINE_VALUE_IS_AS_HELPERS(Mass_Function_Call_Lazy_Payload, mass_function_call_lazy_payload);
DEFINE_VALUE_IS_AS_HELPERS(Mass_Function_Call_Lazy_Payload *, mass_function_call_lazy_payload_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(tuple, Tuple,
  {
    .descriptor = &descriptor_epoch,
    .name = slice_literal_fields("epoch"),
    .offset = offsetof(Tuple, epoch),
  },
  {
    .descriptor = &descriptor_scope_pointer,
    .name = slice_literal_fields("scope_where_it_was_created"),
    .offset = offsetof(Tuple, scope_where_it_was_created),
  },
  {
    .descriptor = &descriptor_array_value_ptr,
    .name = slice_literal_fields("items"),
    .offset = offsetof(Tuple, items),
  },
);
MASS_DEFINE_TYPE_VALUE(tuple);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_tuple_ptr, tuple_pointer, Array_Tuple_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_tuple, tuple, Array_Tuple);
DEFINE_VALUE_IS_AS_HELPERS(Tuple, tuple);
DEFINE_VALUE_IS_AS_HELPERS(Tuple *, tuple_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(list_node, List_Node,
  {
    .descriptor = &descriptor_list_node_pointer,
    .name = slice_literal_fields("maybe_previous"),
    .offset = offsetof(List_Node, maybe_previous),
  },
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("value"),
    .offset = offsetof(List_Node, value),
  },
);
MASS_DEFINE_TYPE_VALUE(list_node);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_list_node_ptr, list_node_pointer, Array_List_Node_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_list_node, list_node, Array_List_Node);
DEFINE_VALUE_IS_AS_HELPERS(List_Node, list_node);
DEFINE_VALUE_IS_AS_HELPERS(List_Node *, list_node_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(typed_symbol, Typed_Symbol,
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("symbol"),
    .offset = offsetof(Typed_Symbol, symbol),
  },
  {
    .descriptor = &descriptor_descriptor_pointer,
    .name = slice_literal_fields("descriptor"),
    .offset = offsetof(Typed_Symbol, descriptor),
  },
);
MASS_DEFINE_TYPE_VALUE(typed_symbol);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_typed_symbol_ptr, typed_symbol_pointer, Array_Typed_Symbol_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_typed_symbol, typed_symbol, Array_Typed_Symbol);
DEFINE_VALUE_IS_AS_HELPERS(Typed_Symbol, typed_symbol);
DEFINE_VALUE_IS_AS_HELPERS(Typed_Symbol *, typed_symbol_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(struct_field, Struct_Field,
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("name"),
    .offset = offsetof(Struct_Field, name),
  },
  {
    .descriptor = &descriptor_descriptor_pointer,
    .name = slice_literal_fields("descriptor"),
    .offset = offsetof(Struct_Field, descriptor),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("offset"),
    .offset = offsetof(Struct_Field, offset),
  },
);
MASS_DEFINE_TYPE_VALUE(struct_field);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_struct_field_ptr, struct_field_pointer, Array_Struct_Field_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_struct_field, struct_field, Array_Struct_Field);
DEFINE_VALUE_IS_AS_HELPERS(Struct_Field, struct_field);
DEFINE_VALUE_IS_AS_HELPERS(Struct_Field *, struct_field_pointer);
/*union struct start */
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_descriptor_ptr, descriptor_pointer, Array_Descriptor_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_descriptor, descriptor, Array_Descriptor);
MASS_DEFINE_OPAQUE_C_TYPE(descriptor_tag, Descriptor_Tag)
static C_Enum_Item descriptor_tag_items[] = {
{ .name = slice_literal_fields("Void"), .value = 0 },
{ .name = slice_literal_fields("Never"), .value = 1 },
{ .name = slice_literal_fields("Raw"), .value = 2 },
{ .name = slice_literal_fields("Float"), .value = 3 },
{ .name = slice_literal_fields("Integer"), .value = 4 },
{ .name = slice_literal_fields("Function_Instance"), .value = 5 },
{ .name = slice_literal_fields("Fixed_Array"), .value = 6 },
{ .name = slice_literal_fields("Struct"), .value = 7 },
{ .name = slice_literal_fields("Pointer_To"), .value = 8 },
};
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(descriptor_void, 0);
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(descriptor_never, 0);
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(descriptor_raw, 0);
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(descriptor_float, 0);
MASS_DEFINE_STRUCT_DESCRIPTOR(descriptor_integer, Descriptor_Integer,
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("is_signed"),
    .offset = offsetof(Descriptor_Integer, is_signed),
  },
);
MASS_DEFINE_TYPE_VALUE(descriptor_integer);
MASS_DEFINE_STRUCT_DESCRIPTOR(descriptor_function_instance, Descriptor_Function_Instance,
  {
    .descriptor = &descriptor_function_info_pointer,
    .name = slice_literal_fields("info"),
    .offset = offsetof(Descriptor_Function_Instance, info),
  },
  {
    .descriptor = &descriptor_function_call_setup,
    .name = slice_literal_fields("call_setup"),
    .offset = offsetof(Descriptor_Function_Instance, call_setup),
  },
  {
    .descriptor = &descriptor_program_pointer,
    .name = slice_literal_fields("program"),
    .offset = offsetof(Descriptor_Function_Instance, program),
  },
);
MASS_DEFINE_TYPE_VALUE(descriptor_function_instance);
MASS_DEFINE_STRUCT_DESCRIPTOR(descriptor_fixed_array, Descriptor_Fixed_Array,
  {
    .descriptor = &descriptor_descriptor_pointer,
    .name = slice_literal_fields("item"),
    .offset = offsetof(Descriptor_Fixed_Array, item),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("length"),
    .offset = offsetof(Descriptor_Fixed_Array, length),
  },
);
MASS_DEFINE_TYPE_VALUE(descriptor_fixed_array);
MASS_DEFINE_STRUCT_DESCRIPTOR(descriptor_struct, Descriptor_Struct,
  {
    .descriptor = &descriptor_array_struct_field,
    .name = slice_literal_fields("fields"),
    .offset = offsetof(Descriptor_Struct, fields),
  },
);
MASS_DEFINE_TYPE_VALUE(descriptor_struct);
MASS_DEFINE_STRUCT_DESCRIPTOR(descriptor_pointer_to, Descriptor_Pointer_To,
  {
    .descriptor = &descriptor_descriptor_pointer,
    .name = slice_literal_fields("descriptor"),
    .offset = offsetof(Descriptor_Pointer_To, descriptor),
  },
);
MASS_DEFINE_TYPE_VALUE(descriptor_pointer_to);
MASS_DEFINE_STRUCT_DESCRIPTOR(descriptor, Descriptor,
  {
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_descriptor_tag,
    .offset = offsetof(Descriptor, tag),
  },
  {
    .descriptor = &descriptor_module_pointer,
    .name = slice_literal_fields("own_module"),
    .offset = offsetof(Descriptor, own_module),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("brand"),
    .offset = offsetof(Descriptor, brand),
  },
  {
    .descriptor = &descriptor_bits,
    .name = slice_literal_fields("bit_size"),
    .offset = offsetof(Descriptor, bit_size),
  },
  {
    .descriptor = &descriptor_bits,
    .name = slice_literal_fields("bit_alignment"),
    .offset = offsetof(Descriptor, bit_alignment),
  },
  {
    .name = slice_literal_fields("Integer"),
    .descriptor = &descriptor_descriptor_integer,
    .offset = offsetof(Descriptor, Integer),
  },
  {
    .name = slice_literal_fields("Function_Instance"),
    .descriptor = &descriptor_descriptor_function_instance,
    .offset = offsetof(Descriptor, Function_Instance),
  },
  {
    .name = slice_literal_fields("Fixed_Array"),
    .descriptor = &descriptor_descriptor_fixed_array,
    .offset = offsetof(Descriptor, Fixed_Array),
  },
  {
    .name = slice_literal_fields("Struct"),
    .descriptor = &descriptor_descriptor_struct,
    .offset = offsetof(Descriptor, Struct),
  },
  {
    .name = slice_literal_fields("Pointer_To"),
    .descriptor = &descriptor_descriptor_pointer_to,
    .offset = offsetof(Descriptor, Pointer_To),
  },
  {
    .name = slice_literal_fields("Void"),
    .descriptor = &descriptor_descriptor_void,
    .offset = offsetof(Descriptor, Pointer_To)/*:EmptyVariantOffset*/,
  },
  {
    .name = slice_literal_fields("Never"),
    .descriptor = &descriptor_descriptor_never,
    .offset = offsetof(Descriptor, Pointer_To)/*:EmptyVariantOffset*/,
  },
  {
    .name = slice_literal_fields("Raw"),
    .descriptor = &descriptor_descriptor_raw,
    .offset = offsetof(Descriptor, Pointer_To)/*:EmptyVariantOffset*/,
  },
  {
    .name = slice_literal_fields("Float"),
    .descriptor = &descriptor_descriptor_float,
    .offset = offsetof(Descriptor, Pointer_To)/*:EmptyVariantOffset*/,
  },
);
MASS_DEFINE_TYPE_VALUE(descriptor);
DEFINE_VALUE_IS_AS_HELPERS(Descriptor, descriptor);
DEFINE_VALUE_IS_AS_HELPERS(Descriptor *, descriptor_pointer);
/*union struct end*/
MASS_DEFINE_STRUCT_DESCRIPTOR(type, Type,
  {
    .descriptor = &descriptor_descriptor_pointer,
    .name = slice_literal_fields("descriptor"),
    .offset = offsetof(Type, descriptor),
  },
);
MASS_DEFINE_TYPE_VALUE(type);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_type_ptr, type_pointer, Array_Type_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_type, type, Array_Type);
DEFINE_VALUE_IS_AS_HELPERS(Type, type);
DEFINE_VALUE_IS_AS_HELPERS(Type *, type_pointer);
MASS_DEFINE_FUNCTION_DESCRIPTOR(
  mass_type_constraint_proc,
  &descriptor__bool,
  {
    .tag = Resolved_Function_Parameter_Tag_Unknown,
    .descriptor = &descriptor_type,
  }
)
/*union struct start */
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_mass_error_ptr, mass_error_pointer, Array_Mass_Error_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_mass_error, mass_error, Array_Mass_Error);
MASS_DEFINE_OPAQUE_C_TYPE(mass_error_tag, Mass_Error_Tag)
static C_Enum_Item mass_error_tag_items[] = {
{ .name = slice_literal_fields("Unimplemented"), .value = 0 },
{ .name = slice_literal_fields("Unreachable_Statement"), .value = 1 },
{ .name = slice_literal_fields("Parse"), .value = 2 },
{ .name = slice_literal_fields("Assignment_To_Constant"), .value = 3 },
{ .name = slice_literal_fields("User_Defined"), .value = 4 },
{ .name = slice_literal_fields("Circular_Dependency"), .value = 5 },
{ .name = slice_literal_fields("Non_Trailing_Default_Argument"), .value = 6 },
{ .name = slice_literal_fields("Expected_Static"), .value = 7 },
{ .name = slice_literal_fields("Tokenizer"), .value = 8 },
{ .name = slice_literal_fields("Integer_Range"), .value = 9 },
{ .name = slice_literal_fields("File_Open"), .value = 10 },
{ .name = slice_literal_fields("File_Too_Large"), .value = 11 },
{ .name = slice_literal_fields("Dynamic_Library_Load"), .value = 12 },
{ .name = slice_literal_fields("Dynamic_Library_Symbol_Not_Found"), .value = 13 },
{ .name = slice_literal_fields("Operator_Fixity_Conflict"), .value = 14 },
{ .name = slice_literal_fields("Undefined_Variable"), .value = 15 },
{ .name = slice_literal_fields("Redefinition"), .value = 16 },
{ .name = slice_literal_fields("Unknown_Field"), .value = 17 },
{ .name = slice_literal_fields("Invalid_Identifier"), .value = 18 },
{ .name = slice_literal_fields("Type_Mismatch"), .value = 19 },
{ .name = slice_literal_fields("Epoch_Mismatch"), .value = 20 },
{ .name = slice_literal_fields("No_Matching_Overload"), .value = 21 },
{ .name = slice_literal_fields("Undecidable_Overload"), .value = 22 },
{ .name = slice_literal_fields("Non_Function_Overload"), .value = 23 },
{ .name = slice_literal_fields("No_Runtime_Use"), .value = 24 },
{ .name = slice_literal_fields("Recursive_Intrinsic_Use"), .value = 25 },
};
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(mass_error_unimplemented, 0);
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(mass_error_unreachable_statement, 0);
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(mass_error_parse, 0);
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(mass_error_assignment_to_constant, 0);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_error_user_defined, Mass_Error_User_Defined,
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("name"),
    .offset = offsetof(Mass_Error_User_Defined, name),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_user_defined);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_error_circular_dependency, Mass_Error_Circular_Dependency,
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("name"),
    .offset = offsetof(Mass_Error_Circular_Dependency, name),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_circular_dependency);
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(mass_error_non_trailing_default_argument, 0);
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(mass_error_expected_static, 0);
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(mass_error_tokenizer, 0);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_error_integer_range, Mass_Error_Integer_Range,
  {
    .descriptor = &descriptor_descriptor_pointer,
    .name = slice_literal_fields("descriptor"),
    .offset = offsetof(Mass_Error_Integer_Range, descriptor),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_integer_range);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_error_file_open, Mass_Error_File_Open,
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("path"),
    .offset = offsetof(Mass_Error_File_Open, path),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_file_open);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_error_file_too_large, Mass_Error_File_Too_Large,
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("path"),
    .offset = offsetof(Mass_Error_File_Too_Large, path),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_file_too_large);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_error_dynamic_library_load, Mass_Error_Dynamic_Library_Load,
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("library_name"),
    .offset = offsetof(Mass_Error_Dynamic_Library_Load, library_name),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_dynamic_library_load);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_error_dynamic_library_symbol_not_found, Mass_Error_Dynamic_Library_Symbol_Not_Found,
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("library_name"),
    .offset = offsetof(Mass_Error_Dynamic_Library_Symbol_Not_Found, library_name),
  },
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("symbol_name"),
    .offset = offsetof(Mass_Error_Dynamic_Library_Symbol_Not_Found, symbol_name),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_dynamic_library_symbol_not_found);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_error_operator_fixity_conflict, Mass_Error_Operator_Fixity_Conflict,
  {
    .descriptor = &descriptor_operator_fixity,
    .name = slice_literal_fields("fixity"),
    .offset = offsetof(Mass_Error_Operator_Fixity_Conflict, fixity),
  },
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("symbol"),
    .offset = offsetof(Mass_Error_Operator_Fixity_Conflict, symbol),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_operator_fixity_conflict);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_error_undefined_variable, Mass_Error_Undefined_Variable,
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("name"),
    .offset = offsetof(Mass_Error_Undefined_Variable, name),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("is_operator"),
    .offset = offsetof(Mass_Error_Undefined_Variable, is_operator),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_undefined_variable);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_error_redefinition, Mass_Error_Redefinition,
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("name"),
    .offset = offsetof(Mass_Error_Redefinition, name),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_redefinition);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_error_unknown_field, Mass_Error_Unknown_Field,
  {
    .descriptor = &descriptor_descriptor_pointer,
    .name = slice_literal_fields("type"),
    .offset = offsetof(Mass_Error_Unknown_Field, type),
  },
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("name"),
    .offset = offsetof(Mass_Error_Unknown_Field, name),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_unknown_field);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_error_invalid_identifier, Mass_Error_Invalid_Identifier,
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("id"),
    .offset = offsetof(Mass_Error_Invalid_Identifier, id),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_invalid_identifier);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_error_type_mismatch, Mass_Error_Type_Mismatch,
  {
    .descriptor = &descriptor_descriptor_pointer,
    .name = slice_literal_fields("expected"),
    .offset = offsetof(Mass_Error_Type_Mismatch, expected),
  },
  {
    .descriptor = &descriptor_descriptor_pointer,
    .name = slice_literal_fields("actual"),
    .offset = offsetof(Mass_Error_Type_Mismatch, actual),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_type_mismatch);
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(mass_error_epoch_mismatch, 0);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_error_no_matching_overload, Mass_Error_No_Matching_Overload,
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("target"),
    .offset = offsetof(Mass_Error_No_Matching_Overload, target),
  },
  {
    .descriptor = &descriptor_array_resolved_function_parameter,
    .name = slice_literal_fields("arguments"),
    .offset = offsetof(Mass_Error_No_Matching_Overload, arguments),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_no_matching_overload);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_error_undecidable_overload, Mass_Error_Undecidable_Overload,
  {
    .descriptor = &descriptor_array_undecidable_match,
    .name = slice_literal_fields("matches"),
    .offset = offsetof(Mass_Error_Undecidable_Overload, matches),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_undecidable_overload);
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(mass_error_non_function_overload, 0);
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(mass_error_no_runtime_use, 0);
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(mass_error_recursive_intrinsic_use, 0);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_error, Mass_Error,
  {
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_mass_error_tag,
    .offset = offsetof(Mass_Error, tag),
  },
  {
    .descriptor = &descriptor_slice,
    .name = slice_literal_fields("detailed_message"),
    .offset = offsetof(Mass_Error, detailed_message),
  },
  {
    .descriptor = &descriptor_source_range,
    .name = slice_literal_fields("source_range"),
    .offset = offsetof(Mass_Error, source_range),
  },
  {
    .descriptor = &descriptor_source_range,
    .name = slice_literal_fields("other_source_range"),
    .offset = offsetof(Mass_Error, other_source_range),
  },
  {
    .name = slice_literal_fields("User_Defined"),
    .descriptor = &descriptor_mass_error_user_defined,
    .offset = offsetof(Mass_Error, User_Defined),
  },
  {
    .name = slice_literal_fields("Circular_Dependency"),
    .descriptor = &descriptor_mass_error_circular_dependency,
    .offset = offsetof(Mass_Error, Circular_Dependency),
  },
  {
    .name = slice_literal_fields("Integer_Range"),
    .descriptor = &descriptor_mass_error_integer_range,
    .offset = offsetof(Mass_Error, Integer_Range),
  },
  {
    .name = slice_literal_fields("File_Open"),
    .descriptor = &descriptor_mass_error_file_open,
    .offset = offsetof(Mass_Error, File_Open),
  },
  {
    .name = slice_literal_fields("File_Too_Large"),
    .descriptor = &descriptor_mass_error_file_too_large,
    .offset = offsetof(Mass_Error, File_Too_Large),
  },
  {
    .name = slice_literal_fields("Dynamic_Library_Load"),
    .descriptor = &descriptor_mass_error_dynamic_library_load,
    .offset = offsetof(Mass_Error, Dynamic_Library_Load),
  },
  {
    .name = slice_literal_fields("Dynamic_Library_Symbol_Not_Found"),
    .descriptor = &descriptor_mass_error_dynamic_library_symbol_not_found,
    .offset = offsetof(Mass_Error, Dynamic_Library_Symbol_Not_Found),
  },
  {
    .name = slice_literal_fields("Operator_Fixity_Conflict"),
    .descriptor = &descriptor_mass_error_operator_fixity_conflict,
    .offset = offsetof(Mass_Error, Operator_Fixity_Conflict),
  },
  {
    .name = slice_literal_fields("Undefined_Variable"),
    .descriptor = &descriptor_mass_error_undefined_variable,
    .offset = offsetof(Mass_Error, Undefined_Variable),
  },
  {
    .name = slice_literal_fields("Redefinition"),
    .descriptor = &descriptor_mass_error_redefinition,
    .offset = offsetof(Mass_Error, Redefinition),
  },
  {
    .name = slice_literal_fields("Unknown_Field"),
    .descriptor = &descriptor_mass_error_unknown_field,
    .offset = offsetof(Mass_Error, Unknown_Field),
  },
  {
    .name = slice_literal_fields("Invalid_Identifier"),
    .descriptor = &descriptor_mass_error_invalid_identifier,
    .offset = offsetof(Mass_Error, Invalid_Identifier),
  },
  {
    .name = slice_literal_fields("Type_Mismatch"),
    .descriptor = &descriptor_mass_error_type_mismatch,
    .offset = offsetof(Mass_Error, Type_Mismatch),
  },
  {
    .name = slice_literal_fields("No_Matching_Overload"),
    .descriptor = &descriptor_mass_error_no_matching_overload,
    .offset = offsetof(Mass_Error, No_Matching_Overload),
  },
  {
    .name = slice_literal_fields("Undecidable_Overload"),
    .descriptor = &descriptor_mass_error_undecidable_overload,
    .offset = offsetof(Mass_Error, Undecidable_Overload),
  },
  {
    .name = slice_literal_fields("Unimplemented"),
    .descriptor = &descriptor_mass_error_unimplemented,
    .offset = offsetof(Mass_Error, Undecidable_Overload)/*:EmptyVariantOffset*/,
  },
  {
    .name = slice_literal_fields("Unreachable_Statement"),
    .descriptor = &descriptor_mass_error_unreachable_statement,
    .offset = offsetof(Mass_Error, Undecidable_Overload)/*:EmptyVariantOffset*/,
  },
  {
    .name = slice_literal_fields("Parse"),
    .descriptor = &descriptor_mass_error_parse,
    .offset = offsetof(Mass_Error, Undecidable_Overload)/*:EmptyVariantOffset*/,
  },
  {
    .name = slice_literal_fields("Assignment_To_Constant"),
    .descriptor = &descriptor_mass_error_assignment_to_constant,
    .offset = offsetof(Mass_Error, Undecidable_Overload)/*:EmptyVariantOffset*/,
  },
  {
    .name = slice_literal_fields("Non_Trailing_Default_Argument"),
    .descriptor = &descriptor_mass_error_non_trailing_default_argument,
    .offset = offsetof(Mass_Error, Undecidable_Overload)/*:EmptyVariantOffset*/,
  },
  {
    .name = slice_literal_fields("Expected_Static"),
    .descriptor = &descriptor_mass_error_expected_static,
    .offset = offsetof(Mass_Error, Undecidable_Overload)/*:EmptyVariantOffset*/,
  },
  {
    .name = slice_literal_fields("Tokenizer"),
    .descriptor = &descriptor_mass_error_tokenizer,
    .offset = offsetof(Mass_Error, Undecidable_Overload)/*:EmptyVariantOffset*/,
  },
  {
    .name = slice_literal_fields("Epoch_Mismatch"),
    .descriptor = &descriptor_mass_error_epoch_mismatch,
    .offset = offsetof(Mass_Error, Undecidable_Overload)/*:EmptyVariantOffset*/,
  },
  {
    .name = slice_literal_fields("Non_Function_Overload"),
    .descriptor = &descriptor_mass_error_non_function_overload,
    .offset = offsetof(Mass_Error, Undecidable_Overload)/*:EmptyVariantOffset*/,
  },
  {
    .name = slice_literal_fields("No_Runtime_Use"),
    .descriptor = &descriptor_mass_error_no_runtime_use,
    .offset = offsetof(Mass_Error, Undecidable_Overload)/*:EmptyVariantOffset*/,
  },
  {
    .name = slice_literal_fields("Recursive_Intrinsic_Use"),
    .descriptor = &descriptor_mass_error_recursive_intrinsic_use,
    .offset = offsetof(Mass_Error, Undecidable_Overload)/*:EmptyVariantOffset*/,
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error);
DEFINE_VALUE_IS_AS_HELPERS(Mass_Error, mass_error);
DEFINE_VALUE_IS_AS_HELPERS(Mass_Error *, mass_error_pointer);
/*union struct end*/
/*union struct start */
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_mass_result_ptr, mass_result_pointer, Array_Mass_Result_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_mass_result, mass_result, Array_Mass_Result);
MASS_DEFINE_OPAQUE_C_TYPE(mass_result_tag, Mass_Result_Tag)
static C_Enum_Item mass_result_tag_items[] = {
{ .name = slice_literal_fields("Success"), .value = 0 },
{ .name = slice_literal_fields("Error"), .value = 1 },
};
MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(mass_result_success, 0);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_result_error, Mass_Result_Error,
  {
    .descriptor = &descriptor_mass_error,
    .name = slice_literal_fields("error"),
    .offset = offsetof(Mass_Result_Error, error),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_result_error);
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_result, Mass_Result,
  {
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_mass_result_tag,
    .offset = offsetof(Mass_Result, tag),
  },
  {
    .name = slice_literal_fields("Error"),
    .descriptor = &descriptor_mass_result_error,
    .offset = offsetof(Mass_Result, Error),
  },
  {
    .name = slice_literal_fields("Success"),
    .descriptor = &descriptor_mass_result_success,
    .offset = offsetof(Mass_Result, Error)/*:EmptyVariantOffset*/,
  },
);
MASS_DEFINE_TYPE_VALUE(mass_result);
DEFINE_VALUE_IS_AS_HELPERS(Mass_Result, mass_result);
DEFINE_VALUE_IS_AS_HELPERS(Mass_Result *, mass_result_pointer);
/*union struct end*/
MASS_DEFINE_OPAQUE_C_TYPE(os, Os)
static C_Enum_Item os_items[] = {
{ .name = slice_literal_fields("Windows"), .value = 1 },
{ .name = slice_literal_fields("Linux"), .value = 2 },
{ .name = slice_literal_fields("Mac"), .value = 3 },
};
DEFINE_VALUE_IS_AS_HELPERS(Os, os);
DEFINE_VALUE_IS_AS_HELPERS(Os *, os_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(program, Program,
  {
    .descriptor = &descriptor_array_import_library,
    .name = slice_literal_fields("import_libraries"),
    .offset = offsetof(Program, import_libraries),
  },
  {
    .descriptor = &descriptor_array_label_location_diff_patch_info,
    .name = slice_literal_fields("patch_info_array"),
    .offset = offsetof(Program, patch_info_array),
  },
  {
    .descriptor = &descriptor_array_relocation,
    .name = slice_literal_fields("relocations"),
    .offset = offsetof(Program, relocations),
  },
  {
    .descriptor = &descriptor_value_pointer,
    .name = slice_literal_fields("entry_point"),
    .offset = offsetof(Program, entry_point),
  },
  {
    .descriptor = &descriptor_array_function_builder,
    .name = slice_literal_fields("functions"),
    .offset = offsetof(Program, functions),
  },
  {
    .descriptor = &descriptor_program_memory,
    .name = slice_literal_fields("memory"),
    .offset = offsetof(Program, memory),
  },
  {
    .descriptor = &descriptor_calling_convention_pointer,
    .name = slice_literal_fields("default_calling_convention"),
    .offset = offsetof(Program, default_calling_convention),
  },
  {
    .descriptor = &descriptor_os,
    .name = slice_literal_fields("os"),
    .offset = offsetof(Program, os),
  },
);
MASS_DEFINE_TYPE_VALUE(program);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_program_ptr, program_pointer, Array_Program_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_program, program, Array_Program);
DEFINE_VALUE_IS_AS_HELPERS(Program, program);
DEFINE_VALUE_IS_AS_HELPERS(Program *, program_pointer);
MASS_DEFINE_FUNCTION_DESCRIPTOR(
  calling_convention_call_setup_proc,
  &descriptor_function_call_setup,
  {
    .tag = Resolved_Function_Parameter_Tag_Unknown,
    .descriptor = &descriptor_allocator_pointer,
  },
  {
    .tag = Resolved_Function_Parameter_Tag_Unknown,
    .descriptor = &descriptor_function_info_pointer,
  }
)
MASS_DEFINE_STRUCT_DESCRIPTOR(calling_convention, Calling_Convention,
  {
    .descriptor = &descriptor_register_bitset,
    .name = slice_literal_fields("register_volatile_bitset"),
    .offset = offsetof(Calling_Convention, register_volatile_bitset),
  },
  {
    .descriptor = &descriptor_calling_convention_call_setup_proc,
    .name = slice_literal_fields("call_setup_proc"),
    .offset = offsetof(Calling_Convention, call_setup_proc),
  },
);
MASS_DEFINE_TYPE_VALUE(calling_convention);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_calling_convention_ptr, calling_convention_pointer, Array_Calling_Convention_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_calling_convention, calling_convention, Array_Calling_Convention);
DEFINE_VALUE_IS_AS_HELPERS(Calling_Convention, calling_convention);
DEFINE_VALUE_IS_AS_HELPERS(Calling_Convention *, calling_convention_pointer);
MASS_DEFINE_FUNCTION_DESCRIPTOR(
  mass_trampoline_proc,
  &descriptor_void,
  {
    .tag = Resolved_Function_Parameter_Tag_Unknown,
    .descriptor = &descriptor_void_pointer,
  },
  {
    .tag = Resolved_Function_Parameter_Tag_Unknown,
    .descriptor = &descriptor_void_pointer,
  }
)
MASS_DEFINE_STRUCT_DESCRIPTOR(mass_trampoline, Mass_Trampoline,
  {
    .descriptor = &descriptor_descriptor_pointer,
    .name = slice_literal_fields("args_descriptor"),
    .offset = offsetof(Mass_Trampoline, args_descriptor),
  },
  {
    .descriptor = &descriptor_function_info_pointer,
    .name = slice_literal_fields("original_info"),
    .offset = offsetof(Mass_Trampoline, original_info),
  },
  {
    .descriptor = &descriptor_mass_trampoline_proc,
    .name = slice_literal_fields("proc"),
    .offset = offsetof(Mass_Trampoline, proc),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_trampoline);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_mass_trampoline_ptr, mass_trampoline_pointer, Array_Mass_Trampoline_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_mass_trampoline, mass_trampoline, Array_Mass_Trampoline);
DEFINE_VALUE_IS_AS_HELPERS(Mass_Trampoline, mass_trampoline);
DEFINE_VALUE_IS_AS_HELPERS(Mass_Trampoline *, mass_trampoline_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(jit_counters, Jit_Counters,
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("functions"),
    .offset = offsetof(Jit_Counters, functions),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("imports"),
    .offset = offsetof(Jit_Counters, imports),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("relocations"),
    .offset = offsetof(Jit_Counters, relocations),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("protected_ro_data_page_count"),
    .offset = offsetof(Jit_Counters, protected_ro_data_page_count),
  },
);
MASS_DEFINE_TYPE_VALUE(jit_counters);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_jit_counters_ptr, jit_counters_pointer, Array_Jit_Counters_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_jit_counters, jit_counters, Array_Jit_Counters);
DEFINE_VALUE_IS_AS_HELPERS(Jit_Counters, jit_counters);
DEFINE_VALUE_IS_AS_HELPERS(Jit_Counters *, jit_counters_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(jit, Jit,
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("is_stack_unwinding_in_progress"),
    .offset = offsetof(Jit, is_stack_unwinding_in_progress),
  },
  {
    .descriptor = &descriptor_program_pointer,
    .name = slice_literal_fields("program"),
    .offset = offsetof(Jit, program),
  },
  {
    .descriptor = &descriptor_jit_import_library_handle_map_pointer,
    .name = slice_literal_fields("import_library_handles"),
    .offset = offsetof(Jit, import_library_handles),
  },
  {
    .descriptor = &descriptor_jit_counters,
    .name = slice_literal_fields("previous_counts"),
    .offset = offsetof(Jit, previous_counts),
  },
  {
    .descriptor = &descriptor_void_pointer,
    .name = slice_literal_fields("platform_specific_payload"),
    .offset = offsetof(Jit, platform_specific_payload),
  },
);
MASS_DEFINE_TYPE_VALUE(jit);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_jit_ptr, jit_pointer, Array_Jit_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_jit, jit, Array_Jit);
DEFINE_VALUE_IS_AS_HELPERS(Jit, jit);
DEFINE_VALUE_IS_AS_HELPERS(Jit *, jit_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(common_symbols, Common_Symbols,
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("apply"),
    .offset = offsetof(Common_Symbols, apply),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("fn"),
    .offset = offsetof(Common_Symbols, fn),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("get"),
    .offset = offsetof(Common_Symbols, get),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("intrinsic"),
    .offset = offsetof(Common_Symbols, intrinsic),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("label"),
    .offset = offsetof(Common_Symbols, label),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("postfix_block"),
    .offset = offsetof(Common_Symbols, postfix_block),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("statement"),
    .offset = offsetof(Common_Symbols, statement),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("_if"),
    .offset = offsetof(Common_Symbols, _if),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("then"),
    .offset = offsetof(Common_Symbols, then),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("_while"),
    .offset = offsetof(Common_Symbols, _while),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("_else"),
    .offset = offsetof(Common_Symbols, _else),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("_"),
    .offset = offsetof(Common_Symbols, _),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("operator_arrow"),
    .offset = offsetof(Common_Symbols, operator_arrow),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("operator_at"),
    .offset = offsetof(Common_Symbols, operator_at),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("operator_colon"),
    .offset = offsetof(Common_Symbols, operator_colon),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("operator_colon_equal"),
    .offset = offsetof(Common_Symbols, operator_colon_equal),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("operator_double_colon"),
    .offset = offsetof(Common_Symbols, operator_double_colon),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("operator_comma"),
    .offset = offsetof(Common_Symbols, operator_comma),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("operator_dot"),
    .offset = offsetof(Common_Symbols, operator_dot),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("operator_dot_star"),
    .offset = offsetof(Common_Symbols, operator_dot_star),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("operator_equal"),
    .offset = offsetof(Common_Symbols, operator_equal),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("operator_fat_arrow"),
    .offset = offsetof(Common_Symbols, operator_fat_arrow),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("operator_space"),
    .offset = offsetof(Common_Symbols, operator_space),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("operator_tilde"),
    .offset = offsetof(Common_Symbols, operator_tilde),
  },
  {
    .descriptor = &descriptor_symbol_pointer,
    .name = slice_literal_fields("operator_dot_dot_dot"),
    .offset = offsetof(Common_Symbols, operator_dot_dot_dot),
  },
);
MASS_DEFINE_TYPE_VALUE(common_symbols);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_common_symbols_ptr, common_symbols_pointer, Array_Common_Symbols_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_common_symbols, common_symbols, Array_Common_Symbols);
DEFINE_VALUE_IS_AS_HELPERS(Common_Symbols, common_symbols);
DEFINE_VALUE_IS_AS_HELPERS(Common_Symbols *, common_symbols_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(compilation, Compilation,
  {
    .descriptor = &descriptor_virtual_memory_buffer,
    .name = slice_literal_fields("temp_buffer"),
    .offset = offsetof(Compilation, temp_buffer),
  },
  {
    .descriptor = &descriptor_allocator_pointer,
    .name = slice_literal_fields("temp_allocator"),
    .offset = offsetof(Compilation, temp_allocator),
  },
  {
    .descriptor = &descriptor_virtual_memory_buffer,
    .name = slice_literal_fields("allocation_buffer"),
    .offset = offsetof(Compilation, allocation_buffer),
  },
  {
    .descriptor = &descriptor_allocator_pointer,
    .name = slice_literal_fields("allocator"),
    .offset = offsetof(Compilation, allocator),
  },
  {
    .descriptor = &descriptor_jit,
    .name = slice_literal_fields("jit"),
    .offset = offsetof(Compilation, jit),
  },
  {
    .descriptor = &descriptor_module,
    .name = slice_literal_fields("compiler_module"),
    .offset = offsetof(Compilation, compiler_module),
  },
  {
    .descriptor = &descriptor_static_pointer_length_map_pointer,
    .name = slice_literal_fields("static_pointer_length_map"),
    .offset = offsetof(Compilation, static_pointer_length_map),
  },
  {
    .descriptor = &descriptor_imported_module_map_pointer,
    .name = slice_literal_fields("module_map"),
    .offset = offsetof(Compilation, module_map),
  },
  {
    .descriptor = &descriptor_trampoline_map_pointer,
    .name = slice_literal_fields("trampoline_map"),
    .offset = offsetof(Compilation, trampoline_map),
  },
  {
    .descriptor = &descriptor_scope_pointer,
    .name = slice_literal_fields("root_scope"),
    .offset = offsetof(Compilation, root_scope),
  },
  {
    .descriptor = &descriptor_program_pointer,
    .name = slice_literal_fields("runtime_program"),
    .offset = offsetof(Compilation, runtime_program),
  },
  {
    .descriptor = &descriptor_mass_result_pointer,
    .name = slice_literal_fields("result"),
    .offset = offsetof(Compilation, result),
  },
  {
    .descriptor = &descriptor_symbol_map_pointer,
    .name = slice_literal_fields("symbol_cache_map"),
    .offset = offsetof(Compilation, symbol_cache_map),
  },
  {
    .descriptor = &descriptor_operator_symbol_map_pointer,
    .name = slice_literal_fields("prefix_operator_symbol_map"),
    .offset = offsetof(Compilation, prefix_operator_symbol_map),
  },
  {
    .descriptor = &descriptor_operator_symbol_map_pointer,
    .name = slice_literal_fields("infix_or_suffix_operator_symbol_map"),
    .offset = offsetof(Compilation, infix_or_suffix_operator_symbol_map),
  },
  {
    .descriptor = &descriptor_descriptor_pointer_to_cache_map_pointer,
    .name = slice_literal_fields("descriptor_pointer_to_cache_map"),
    .offset = offsetof(Compilation, descriptor_pointer_to_cache_map),
  },
  {
    .descriptor = &descriptor_intrinsic_proc_cache_map_pointer,
    .name = slice_literal_fields("intrinsic_proc_cache_map"),
    .offset = offsetof(Compilation, intrinsic_proc_cache_map),
  },
  {
    .descriptor = &descriptor_common_symbols,
    .name = slice_literal_fields("common_symbols"),
    .offset = offsetof(Compilation, common_symbols),
  },
  {
    .descriptor = &descriptor_operator,
    .name = slice_literal_fields("apply_operator"),
    .offset = offsetof(Compilation, apply_operator),
  },
);
MASS_DEFINE_TYPE_VALUE(compilation);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_compilation_ptr, compilation_pointer, Array_Compilation_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_compilation, compilation, Array_Compilation);
DEFINE_VALUE_IS_AS_HELPERS(Compilation, compilation);
DEFINE_VALUE_IS_AS_HELPERS(Compilation *, compilation_pointer);
MASS_DEFINE_FUNCTION_DESCRIPTOR(
  lazy_value_proc,
  &descriptor_value_pointer,
  {
    .tag = Resolved_Function_Parameter_Tag_Unknown,
    .descriptor = &descriptor_mass_context_pointer,
  },
  {
    .tag = Resolved_Function_Parameter_Tag_Unknown,
    .descriptor = &descriptor_function_builder_pointer,
  },
  {
    .tag = Resolved_Function_Parameter_Tag_Unknown,
    .descriptor = &descriptor_expected_result_pointer,
  },
  {
    .tag = Resolved_Function_Parameter_Tag_Unknown,
    .descriptor = &descriptor_scope_pointer,
  },
  {
    .tag = Resolved_Function_Parameter_Tag_Unknown,
    .descriptor = &descriptor_source_range_pointer,
  },
  {
    .tag = Resolved_Function_Parameter_Tag_Unknown,
    .descriptor = &descriptor_void_pointer,
  }
)
MASS_DEFINE_OPAQUE_C_TYPE(instruction_extension_type, Instruction_Extension_Type)
static C_Enum_Item instruction_extension_type_items[] = {
{ .name = slice_literal_fields("None"), .value = 0 },
{ .name = slice_literal_fields("Register"), .value = 1 },
{ .name = slice_literal_fields("Op_Code"), .value = 2 },
{ .name = slice_literal_fields("Plus_Register"), .value = 3 },
};
DEFINE_VALUE_IS_AS_HELPERS(Instruction_Extension_Type, instruction_extension_type);
DEFINE_VALUE_IS_AS_HELPERS(Instruction_Extension_Type *, instruction_extension_type_pointer);
MASS_DEFINE_OPAQUE_C_TYPE(operand_encoding_type, Operand_Encoding_Type)
static C_Enum_Item operand_encoding_type_items[] = {
{ .name = slice_literal_fields("None"), .value = 0 },
{ .name = slice_literal_fields("Register"), .value = 2 },
{ .name = slice_literal_fields("Register_A"), .value = 3 },
{ .name = slice_literal_fields("Register_Or_Memory"), .value = 4 },
{ .name = slice_literal_fields("Xmm"), .value = 5 },
{ .name = slice_literal_fields("Memory"), .value = 7 },
{ .name = slice_literal_fields("Immediate"), .value = 8 },
};
DEFINE_VALUE_IS_AS_HELPERS(Operand_Encoding_Type, operand_encoding_type);
DEFINE_VALUE_IS_AS_HELPERS(Operand_Encoding_Type *, operand_encoding_type_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(operand_encoding, Operand_Encoding,
  {
    .descriptor = &descriptor_operand_encoding_type,
    .name = slice_literal_fields("type"),
    .offset = offsetof(Operand_Encoding, type),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("bit_size"),
    .offset = offsetof(Operand_Encoding, bit_size),
  },
);
MASS_DEFINE_TYPE_VALUE(operand_encoding);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_operand_encoding_ptr, operand_encoding_pointer, Array_Operand_Encoding_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_operand_encoding, operand_encoding, Array_Operand_Encoding);
DEFINE_VALUE_IS_AS_HELPERS(Operand_Encoding, operand_encoding);
DEFINE_VALUE_IS_AS_HELPERS(Operand_Encoding *, operand_encoding_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(instruction_encoding, Instruction_Encoding,
  {
    .descriptor = &descriptor_i8_4,
    .name = slice_literal_fields("op_code"),
    .offset = offsetof(Instruction_Encoding, op_code),
  },
  {
    .descriptor = &descriptor_instruction_extension_type,
    .name = slice_literal_fields("extension_type"),
    .offset = offsetof(Instruction_Encoding, extension_type),
  },
  {
    .descriptor = &descriptor_i8,
    .name = slice_literal_fields("op_code_extension"),
    .offset = offsetof(Instruction_Encoding, op_code_extension),
  },
  {
    .descriptor = &descriptor_operand_encoding_3,
    .name = slice_literal_fields("operands"),
    .offset = offsetof(Instruction_Encoding, operands),
  },
);
MASS_DEFINE_TYPE_VALUE(instruction_encoding);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_instruction_encoding_ptr, instruction_encoding_pointer, Array_Instruction_Encoding_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_instruction_encoding, instruction_encoding, Array_Instruction_Encoding);
DEFINE_VALUE_IS_AS_HELPERS(Instruction_Encoding, instruction_encoding);
DEFINE_VALUE_IS_AS_HELPERS(Instruction_Encoding *, instruction_encoding_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(x64_mnemonic, X64_Mnemonic,
  {
    .descriptor = &descriptor_char_pointer,
    .name = slice_literal_fields("name"),
    .offset = offsetof(X64_Mnemonic, name),
  },
  {
    .descriptor = &descriptor_instruction_encoding_pointer,
    .name = slice_literal_fields("encoding_list"),
    .offset = offsetof(X64_Mnemonic, encoding_list),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("encoding_count"),
    .offset = offsetof(X64_Mnemonic, encoding_count),
  },
);
MASS_DEFINE_TYPE_VALUE(x64_mnemonic);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_x64_mnemonic_ptr, x64_mnemonic_pointer, Array_X64_Mnemonic_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_x64_mnemonic, x64_mnemonic, Array_X64_Mnemonic);
DEFINE_VALUE_IS_AS_HELPERS(X64_Mnemonic, x64_mnemonic);
DEFINE_VALUE_IS_AS_HELPERS(X64_Mnemonic *, x64_mnemonic_pointer);
MASS_DEFINE_OPAQUE_C_TYPE(_bool, _Bool)
MASS_DEFINE_C_DYN_ARRAY_TYPE(array__bool_ptr, _bool_pointer, Array__Bool_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array__bool, _bool, Array__Bool);
DEFINE_VALUE_IS_AS_HELPERS(_Bool, _bool);
DEFINE_VALUE_IS_AS_HELPERS(_Bool *, _bool_pointer);
MASS_DEFINE_INTEGER_C_TYPE(char, char, 1)
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_char_ptr, char_pointer, Array_char_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_char, char, Array_char);
DEFINE_VALUE_IS_AS_HELPERS(char, char);
DEFINE_VALUE_IS_AS_HELPERS(char *, char_pointer);
MASS_DEFINE_INTEGER_C_TYPE(int, int, 1)
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_int_ptr, int_pointer, Array_int_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_int, int, Array_int);
DEFINE_VALUE_IS_AS_HELPERS(int, int);
DEFINE_VALUE_IS_AS_HELPERS(int *, int_pointer);
MASS_DEFINE_OPAQUE_C_TYPE(allocator, Allocator)
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_allocator_ptr, allocator_pointer, Array_Allocator_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_allocator, allocator, Array_Allocator);
DEFINE_VALUE_IS_AS_HELPERS(Allocator, allocator);
DEFINE_VALUE_IS_AS_HELPERS(Allocator *, allocator_pointer);
MASS_DEFINE_OPAQUE_C_TYPE(virtual_memory_buffer, Virtual_Memory_Buffer)
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_virtual_memory_buffer_ptr, virtual_memory_buffer_pointer, Array_Virtual_Memory_Buffer_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_virtual_memory_buffer, virtual_memory_buffer, Array_Virtual_Memory_Buffer);
DEFINE_VALUE_IS_AS_HELPERS(Virtual_Memory_Buffer, virtual_memory_buffer);
DEFINE_VALUE_IS_AS_HELPERS(Virtual_Memory_Buffer *, virtual_memory_buffer_pointer);
MASS_DEFINE_RAW_C_TYPE(i8, i8)
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_i8_ptr, i8_pointer, Array_i8_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_i8, i8, Array_i8);
DEFINE_VALUE_IS_AS_HELPERS(i8, i8);
DEFINE_VALUE_IS_AS_HELPERS(i8 *, i8_pointer);
MASS_DEFINE_RAW_C_TYPE(i16, i16)
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_i16_ptr, i16_pointer, Array_i16_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_i16, i16, Array_i16);
DEFINE_VALUE_IS_AS_HELPERS(i16, i16);
DEFINE_VALUE_IS_AS_HELPERS(i16 *, i16_pointer);
MASS_DEFINE_RAW_C_TYPE(i32, i32)
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_i32_ptr, i32_pointer, Array_i32_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_i32, i32, Array_i32);
DEFINE_VALUE_IS_AS_HELPERS(i32, i32);
DEFINE_VALUE_IS_AS_HELPERS(i32 *, i32_pointer);
MASS_DEFINE_RAW_C_TYPE(i64, i64)
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_i64_ptr, i64_pointer, Array_i64_Ptr);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_i64, i64, Array_i64);
DEFINE_VALUE_IS_AS_HELPERS(i64, i64);
DEFINE_VALUE_IS_AS_HELPERS(i64 *, i64_pointer);
MASS_DEFINE_INTEGER_C_TYPE(u8, u8, 0)
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_u8_ptr, u8_pointer, Array_u8_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(u8, u8);
DEFINE_VALUE_IS_AS_HELPERS(u8 *, u8_pointer);
MASS_DEFINE_INTEGER_C_TYPE(u16, u16, 0)
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_u16_ptr, u16_pointer, Array_u16_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(u16, u16);
DEFINE_VALUE_IS_AS_HELPERS(u16 *, u16_pointer);
MASS_DEFINE_INTEGER_C_TYPE(u32, u32, 0)
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_u32_ptr, u32_pointer, Array_u32_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(u32, u32);
DEFINE_VALUE_IS_AS_HELPERS(u32 *, u32_pointer);
MASS_DEFINE_INTEGER_C_TYPE(u64, u64, 0)
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_u64_ptr, u64_pointer, Array_u64_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(u64, u64);
DEFINE_VALUE_IS_AS_HELPERS(u64 *, u64_pointer);
MASS_DEFINE_INTEGER_C_TYPE(s8, s8, 1)
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_s8_ptr, s8_pointer, Array_s8_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(s8, s8);
DEFINE_VALUE_IS_AS_HELPERS(s8 *, s8_pointer);
MASS_DEFINE_INTEGER_C_TYPE(s16, s16, 1)
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_s16_ptr, s16_pointer, Array_s16_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(s16, s16);
DEFINE_VALUE_IS_AS_HELPERS(s16 *, s16_pointer);
MASS_DEFINE_INTEGER_C_TYPE(s32, s32, 1)
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_s32_ptr, s32_pointer, Array_s32_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(s32, s32);
DEFINE_VALUE_IS_AS_HELPERS(s32 *, s32_pointer);
MASS_DEFINE_INTEGER_C_TYPE(s64, s64, 1)
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_s64_ptr, s64_pointer, Array_s64_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(s64, s64);
DEFINE_VALUE_IS_AS_HELPERS(s64 *, s64_pointer);
MASS_DEFINE_FLOAT_C_TYPE(f32, f32)
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_f32_ptr, f32_pointer, Array_f32_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(f32, f32);
DEFINE_VALUE_IS_AS_HELPERS(f32 *, f32_pointer);
MASS_DEFINE_FLOAT_C_TYPE(f64, f64)
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_f64_ptr, f64_pointer, Array_f64_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(f64, f64);
DEFINE_VALUE_IS_AS_HELPERS(f64 *, f64_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(range_u8, Range_u8,
  {
    .descriptor = &descriptor_i8,
    .name = slice_literal_fields("from"),
    .offset = offsetof(Range_u8, from),
  },
  {
    .descriptor = &descriptor_i8,
    .name = slice_literal_fields("to"),
    .offset = offsetof(Range_u8, to),
  },
);
MASS_DEFINE_TYPE_VALUE(range_u8);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_range_u8_ptr, range_u8_pointer, Array_Range_u8_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(Range_u8, range_u8);
DEFINE_VALUE_IS_AS_HELPERS(Range_u8 *, range_u8_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(range_u16, Range_u16,
  {
    .descriptor = &descriptor_i16,
    .name = slice_literal_fields("from"),
    .offset = offsetof(Range_u16, from),
  },
  {
    .descriptor = &descriptor_i16,
    .name = slice_literal_fields("to"),
    .offset = offsetof(Range_u16, to),
  },
);
MASS_DEFINE_TYPE_VALUE(range_u16);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_range_u16_ptr, range_u16_pointer, Array_Range_u16_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(Range_u16, range_u16);
DEFINE_VALUE_IS_AS_HELPERS(Range_u16 *, range_u16_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(range_u32, Range_u32,
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("from"),
    .offset = offsetof(Range_u32, from),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("to"),
    .offset = offsetof(Range_u32, to),
  },
);
MASS_DEFINE_TYPE_VALUE(range_u32);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_range_u32_ptr, range_u32_pointer, Array_Range_u32_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(Range_u32, range_u32);
DEFINE_VALUE_IS_AS_HELPERS(Range_u32 *, range_u32_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(range_u64, Range_u64,
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("from"),
    .offset = offsetof(Range_u64, from),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("to"),
    .offset = offsetof(Range_u64, to),
  },
);
MASS_DEFINE_TYPE_VALUE(range_u64);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_range_u64_ptr, range_u64_pointer, Array_Range_u64_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(Range_u64, range_u64);
DEFINE_VALUE_IS_AS_HELPERS(Range_u64 *, range_u64_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(range_s8, Range_s8,
  {
    .descriptor = &descriptor_i8,
    .name = slice_literal_fields("from"),
    .offset = offsetof(Range_s8, from),
  },
  {
    .descriptor = &descriptor_i8,
    .name = slice_literal_fields("to"),
    .offset = offsetof(Range_s8, to),
  },
);
MASS_DEFINE_TYPE_VALUE(range_s8);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_range_s8_ptr, range_s8_pointer, Array_Range_s8_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(Range_s8, range_s8);
DEFINE_VALUE_IS_AS_HELPERS(Range_s8 *, range_s8_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(range_s16, Range_s16,
  {
    .descriptor = &descriptor_i16,
    .name = slice_literal_fields("from"),
    .offset = offsetof(Range_s16, from),
  },
  {
    .descriptor = &descriptor_i16,
    .name = slice_literal_fields("to"),
    .offset = offsetof(Range_s16, to),
  },
);
MASS_DEFINE_TYPE_VALUE(range_s16);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_range_s16_ptr, range_s16_pointer, Array_Range_s16_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(Range_s16, range_s16);
DEFINE_VALUE_IS_AS_HELPERS(Range_s16 *, range_s16_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(range_s32, Range_s32,
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("from"),
    .offset = offsetof(Range_s32, from),
  },
  {
    .descriptor = &descriptor_i32,
    .name = slice_literal_fields("to"),
    .offset = offsetof(Range_s32, to),
  },
);
MASS_DEFINE_TYPE_VALUE(range_s32);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_range_s32_ptr, range_s32_pointer, Array_Range_s32_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(Range_s32, range_s32);
DEFINE_VALUE_IS_AS_HELPERS(Range_s32 *, range_s32_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(range_s64, Range_s64,
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("from"),
    .offset = offsetof(Range_s64, from),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("to"),
    .offset = offsetof(Range_s64, to),
  },
);
MASS_DEFINE_TYPE_VALUE(range_s64);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_range_s64_ptr, range_s64_pointer, Array_Range_s64_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(Range_s64, range_s64);
DEFINE_VALUE_IS_AS_HELPERS(Range_s64 *, range_s64_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(range_f32, Range_f32,
  {
    .descriptor = &descriptor_f32,
    .name = slice_literal_fields("from"),
    .offset = offsetof(Range_f32, from),
  },
  {
    .descriptor = &descriptor_f32,
    .name = slice_literal_fields("to"),
    .offset = offsetof(Range_f32, to),
  },
);
MASS_DEFINE_TYPE_VALUE(range_f32);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_range_f32_ptr, range_f32_pointer, Array_Range_f32_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(Range_f32, range_f32);
DEFINE_VALUE_IS_AS_HELPERS(Range_f32 *, range_f32_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(range_f64, Range_f64,
  {
    .descriptor = &descriptor_f64,
    .name = slice_literal_fields("from"),
    .offset = offsetof(Range_f64, from),
  },
  {
    .descriptor = &descriptor_f64,
    .name = slice_literal_fields("to"),
    .offset = offsetof(Range_f64, to),
  },
);
MASS_DEFINE_TYPE_VALUE(range_f64);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_range_f64_ptr, range_f64_pointer, Array_Range_f64_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(Range_f64, range_f64);
DEFINE_VALUE_IS_AS_HELPERS(Range_f64 *, range_f64_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(slice, Slice,
  {
    .descriptor = &descriptor_i8_pointer,
    .name = slice_literal_fields("bytes"),
    .offset = offsetof(Slice, bytes),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("length"),
    .offset = offsetof(Slice, length),
  },
);
MASS_DEFINE_TYPE_VALUE(slice);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_slice_ptr, slice_pointer, Array_Slice_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(Slice, slice);
DEFINE_VALUE_IS_AS_HELPERS(Slice *, slice_pointer);
MASS_DEFINE_STRUCT_DESCRIPTOR(dyn_array_internal, Dyn_Array_Internal,
  {
    .descriptor = &descriptor_allocator_pointer,
    .name = slice_literal_fields("allocator"),
    .offset = offsetof(Dyn_Array_Internal, allocator),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("length"),
    .offset = offsetof(Dyn_Array_Internal, length),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("capacity"),
    .offset = offsetof(Dyn_Array_Internal, capacity),
  },
  {
    .descriptor = &descriptor_i8_0,
    .name = slice_literal_fields("items"),
    .offset = offsetof(Dyn_Array_Internal, items),
  },
);
MASS_DEFINE_TYPE_VALUE(dyn_array_internal);
MASS_DEFINE_C_DYN_ARRAY_TYPE(array_dyn_array_internal_ptr, dyn_array_internal_pointer, Array_Dyn_Array_Internal_Ptr);
DEFINE_VALUE_IS_AS_HELPERS(Dyn_Array_Internal, dyn_array_internal);
DEFINE_VALUE_IS_AS_HELPERS(Dyn_Array_Internal *, dyn_array_internal_pointer);

#endif // GENERATED_TYPES_H
