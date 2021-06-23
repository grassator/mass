#ifndef GENERATED_TYPES_H
#define GENERATED_TYPES_H
static Atomic_u64 next_struct_id = {65537};

_Pragma("warning (push)") _Pragma("warning (default: 4820)")
typedef void(*fn_type_opaque)();

typedef struct X64_Mnemonic X64_Mnemonic;

// Forward declarations

typedef struct Source_Position Source_Position;
typedef dyn_array_type(Source_Position *) Array_Source_Position_Ptr;
typedef dyn_array_type(const Source_Position *) Array_Const_Source_Position_Ptr;

typedef struct Source_File Source_File;
typedef dyn_array_type(Source_File *) Array_Source_File_Ptr;
typedef dyn_array_type(const Source_File *) Array_Const_Source_File_Ptr;

typedef struct Source_Range Source_Range;
typedef dyn_array_type(Source_Range *) Array_Source_Range_Ptr;
typedef dyn_array_type(const Source_Range *) Array_Const_Source_Range_Ptr;

typedef struct Module_Export Module_Export;
typedef dyn_array_type(Module_Export *) Array_Module_Export_Ptr;
typedef dyn_array_type(const Module_Export *) Array_Const_Module_Export_Ptr;

typedef struct Module Module;
typedef dyn_array_type(Module *) Array_Module_Ptr;
typedef dyn_array_type(const Module *) Array_Const_Module_Ptr;

typedef struct Parse_Error Parse_Error;
typedef dyn_array_type(Parse_Error *) Array_Parse_Error_Ptr;
typedef dyn_array_type(const Parse_Error *) Array_Const_Parse_Error_Ptr;

typedef enum Group_Tag Group_Tag;

typedef struct Value_View Value_View;
typedef dyn_array_type(Value_View *) Array_Value_View_Ptr;
typedef dyn_array_type(const Value_View *) Array_Const_Value_View_Ptr;

typedef enum Symbol_Type Symbol_Type;

typedef struct Symbol Symbol;
typedef dyn_array_type(Symbol *) Array_Symbol_Ptr;
typedef dyn_array_type(const Symbol *) Array_Const_Symbol_Ptr;

typedef struct Group Group;
typedef dyn_array_type(Group *) Array_Group_Ptr;
typedef dyn_array_type(const Group *) Array_Const_Group_Ptr;

typedef struct Token_Pattern Token_Pattern;
typedef dyn_array_type(Token_Pattern *) Array_Token_Pattern_Ptr;
typedef dyn_array_type(const Token_Pattern *) Array_Const_Token_Pattern_Ptr;

typedef enum Section_Permissions Section_Permissions;

typedef struct Section Section;
typedef dyn_array_type(Section *) Array_Section_Ptr;
typedef dyn_array_type(const Section *) Array_Const_Section_Ptr;

typedef struct Program_Memory Program_Memory;
typedef dyn_array_type(Program_Memory *) Array_Program_Memory_Ptr;
typedef dyn_array_type(const Program_Memory *) Array_Const_Program_Memory_Ptr;

typedef enum Register Register;

typedef struct Label_Index Label_Index;
typedef dyn_array_type(Label_Index *) Array_Label_Index_Ptr;
typedef dyn_array_type(const Label_Index *) Array_Const_Label_Index_Ptr;

typedef struct Label Label;
typedef dyn_array_type(Label *) Array_Label_Ptr;
typedef dyn_array_type(const Label *) Array_Const_Label_Ptr;

typedef struct Label_Location_Diff_Patch_Info Label_Location_Diff_Patch_Info;
typedef dyn_array_type(Label_Location_Diff_Patch_Info *) Array_Label_Location_Diff_Patch_Info_Ptr;
typedef dyn_array_type(const Label_Location_Diff_Patch_Info *) Array_Const_Label_Location_Diff_Patch_Info_Ptr;

typedef enum Number_Base Number_Base;

typedef struct Number_Literal Number_Literal;
typedef dyn_array_type(Number_Literal *) Array_Number_Literal_Ptr;
typedef dyn_array_type(const Number_Literal *) Array_Const_Number_Literal_Ptr;

typedef struct Macro_Capture Macro_Capture;
typedef dyn_array_type(Macro_Capture *) Array_Macro_Capture_Ptr;
typedef dyn_array_type(const Macro_Capture *) Array_Const_Macro_Capture_Ptr;

typedef struct External_Symbol External_Symbol;
typedef dyn_array_type(External_Symbol *) Array_External_Symbol_Ptr;
typedef dyn_array_type(const External_Symbol *) Array_Const_External_Symbol_Ptr;

typedef struct Import_Symbol Import_Symbol;
typedef dyn_array_type(Import_Symbol *) Array_Import_Symbol_Ptr;
typedef dyn_array_type(const Import_Symbol *) Array_Const_Import_Symbol_Ptr;

typedef struct Import_Library Import_Library;
typedef dyn_array_type(Import_Library *) Array_Import_Library_Ptr;
typedef dyn_array_type(const Import_Library *) Array_Const_Import_Library_Ptr;

typedef enum Compare_Type Compare_Type;

typedef enum Stack_Area Stack_Area;

typedef struct Memory_Location Memory_Location;
typedef dyn_array_type(Memory_Location *) Array_Memory_Location_Ptr;
typedef dyn_array_type(const Memory_Location *) Array_Const_Memory_Location_Ptr;

typedef struct Static_Memory Static_Memory;
typedef dyn_array_type(Static_Memory *) Array_Static_Memory_Ptr;
typedef dyn_array_type(const Static_Memory *) Array_Const_Static_Memory_Ptr;

typedef struct Storage Storage;
typedef dyn_array_type(Storage *) Array_Storage_Ptr;
typedef dyn_array_type(const Storage *) Array_Const_Storage_Ptr;

typedef struct Relocation Relocation;
typedef dyn_array_type(Relocation *) Array_Relocation_Ptr;
typedef dyn_array_type(const Relocation *) Array_Const_Relocation_Ptr;

typedef struct Compiler_Source_Location Compiler_Source_Location;
typedef dyn_array_type(Compiler_Source_Location *) Array_Compiler_Source_Location_Ptr;
typedef dyn_array_type(const Compiler_Source_Location *) Array_Const_Compiler_Source_Location_Ptr;

typedef struct Instruction Instruction;
typedef dyn_array_type(Instruction *) Array_Instruction_Ptr;
typedef dyn_array_type(const Instruction *) Array_Const_Instruction_Ptr;

#define INSTRUCTION_BYTES_NO_LABEL (255)

typedef struct Code_Block Code_Block;
typedef dyn_array_type(Code_Block *) Array_Code_Block_Ptr;
typedef dyn_array_type(const Code_Block *) Array_Const_Code_Block_Ptr;

typedef struct Function_Builder Function_Builder;
typedef dyn_array_type(Function_Builder *) Array_Function_Builder_Ptr;
typedef dyn_array_type(const Function_Builder *) Array_Const_Function_Builder_Ptr;

typedef enum Function_Argument_Mode Function_Argument_Mode;

typedef enum Operator_Fixity Operator_Fixity;

typedef enum Operator_Associativity Operator_Associativity;

typedef struct Function_Layout Function_Layout;
typedef dyn_array_type(Function_Layout *) Array_Function_Layout_Ptr;
typedef dyn_array_type(const Function_Layout *) Array_Const_Function_Layout_Ptr;

typedef enum Execution_Context_Flags Execution_Context_Flags;

typedef struct Execution_Context Execution_Context;
typedef dyn_array_type(Execution_Context *) Array_Execution_Context_Ptr;
typedef dyn_array_type(const Execution_Context *) Array_Const_Execution_Context_Ptr;

typedef struct User_Defined_Operator User_Defined_Operator;
typedef dyn_array_type(User_Defined_Operator *) Array_User_Defined_Operator_Ptr;
typedef dyn_array_type(const User_Defined_Operator *) Array_Const_User_Defined_Operator_Ptr;

typedef struct Operator Operator;
typedef dyn_array_type(Operator *) Array_Operator_Ptr;
typedef dyn_array_type(const Operator *) Array_Const_Operator_Ptr;

typedef struct Macro_Pattern Macro_Pattern;
typedef dyn_array_type(Macro_Pattern *) Array_Macro_Pattern_Ptr;
typedef dyn_array_type(const Macro_Pattern *) Array_Const_Macro_Pattern_Ptr;

typedef struct Macro Macro;
typedef dyn_array_type(Macro *) Array_Macro_Ptr;
typedef dyn_array_type(const Macro *) Array_Const_Macro_Ptr;

typedef struct Token_Statement_Matcher Token_Statement_Matcher;
typedef dyn_array_type(Token_Statement_Matcher *) Array_Token_Statement_Matcher_Ptr;
typedef dyn_array_type(const Token_Statement_Matcher *) Array_Const_Token_Statement_Matcher_Ptr;

typedef struct Scope_Entry Scope_Entry;
typedef dyn_array_type(Scope_Entry *) Array_Scope_Entry_Ptr;
typedef dyn_array_type(const Scope_Entry *) Array_Const_Scope_Entry_Ptr;

typedef struct Scope Scope;
typedef dyn_array_type(Scope *) Array_Scope_Ptr;
typedef dyn_array_type(const Scope *) Array_Const_Scope_Ptr;

typedef struct Value Value;
typedef dyn_array_type(Value *) Array_Value_Ptr;
typedef dyn_array_type(const Value *) Array_Const_Value_Ptr;

typedef enum Expected_Result_Storage Expected_Result_Storage;

typedef struct Expected_Result Expected_Result;
typedef dyn_array_type(Expected_Result *) Array_Expected_Result_Ptr;
typedef dyn_array_type(const Expected_Result *) Array_Const_Expected_Result_Ptr;

typedef Value * (*Lazy_Value_Proc)
  (Execution_Context * context, Function_Builder * builder, const Expected_Result * expected_result, void * payload);

typedef struct Lazy_Value Lazy_Value;
typedef dyn_array_type(Lazy_Value *) Array_Lazy_Value_Ptr;
typedef dyn_array_type(const Lazy_Value *) Array_Const_Lazy_Value_Ptr;

typedef struct Lazy_Static_Value Lazy_Static_Value;
typedef dyn_array_type(Lazy_Static_Value *) Array_Lazy_Static_Value_Ptr;
typedef dyn_array_type(const Lazy_Static_Value *) Array_Const_Lazy_Static_Value_Ptr;

typedef Value * (*Mass_Handle_Operator_Proc)
  (Execution_Context * context, Value_View view, void * payload);

typedef enum Memory_Layout_Item_Flags Memory_Layout_Item_Flags;

typedef struct Memory_Layout_Item Memory_Layout_Item;
typedef dyn_array_type(Memory_Layout_Item *) Array_Memory_Layout_Item_Ptr;
typedef dyn_array_type(const Memory_Layout_Item *) Array_Const_Memory_Layout_Item_Ptr;

typedef struct Memory_Layout Memory_Layout;
typedef dyn_array_type(Memory_Layout *) Array_Memory_Layout_Ptr;
typedef dyn_array_type(const Memory_Layout *) Array_Const_Memory_Layout_Ptr;

typedef struct Function_Return Function_Return;
typedef dyn_array_type(Function_Return *) Array_Function_Return_Ptr;
typedef dyn_array_type(const Function_Return *) Array_Const_Function_Return_Ptr;

typedef struct Function_Argument Function_Argument;
typedef dyn_array_type(Function_Argument *) Array_Function_Argument_Ptr;
typedef dyn_array_type(const Function_Argument *) Array_Const_Function_Argument_Ptr;

typedef enum Descriptor_Function_Flags Descriptor_Function_Flags;

typedef struct Function_Info Function_Info;
typedef dyn_array_type(Function_Info *) Array_Function_Info_Ptr;
typedef dyn_array_type(const Function_Info *) Array_Const_Function_Info_Ptr;

typedef struct Function_Literal Function_Literal;
typedef dyn_array_type(Function_Literal *) Array_Function_Literal_Ptr;
typedef dyn_array_type(const Function_Literal *) Array_Const_Function_Literal_Ptr;

typedef struct Descriptor Descriptor;
typedef dyn_array_type(Descriptor *) Array_Descriptor_Ptr;
typedef dyn_array_type(const Descriptor *) Array_Const_Descriptor_Ptr;

typedef struct Mass_Error Mass_Error;
typedef dyn_array_type(Mass_Error *) Array_Mass_Error_Ptr;
typedef dyn_array_type(const Mass_Error *) Array_Const_Mass_Error_Ptr;

typedef struct Mass_Result Mass_Result;
typedef dyn_array_type(Mass_Result *) Array_Mass_Result_Ptr;
typedef dyn_array_type(const Mass_Result *) Array_Const_Mass_Result_Ptr;

typedef struct Program Program;
typedef dyn_array_type(Program *) Array_Program_Ptr;
typedef dyn_array_type(const Program *) Array_Const_Program_Ptr;

typedef void (*Calling_Convention_Body_End_Proc)
  (Program * program, Function_Builder * builder);

typedef Memory_Layout (*Calling_Convention_Arguments_Layout_Proc)
  (const Allocator * allocator, const Function_Info * function_info);

typedef Value * (*Calling_Convention_Return_Proc)
  (const Allocator * allocator, const Function_Info * function_info, Function_Argument_Mode mode);

typedef struct Calling_Convention Calling_Convention;
typedef dyn_array_type(Calling_Convention *) Array_Calling_Convention_Ptr;
typedef dyn_array_type(const Calling_Convention *) Array_Const_Calling_Convention_Ptr;

typedef u64 (*Token_Statement_Matcher_Proc)
  (Execution_Context * context, Value_View view, Lazy_Value * out_lazy_value, void * payload);

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

typedef struct Static_Pointer_Map Static_Pointer_Map;

typedef struct Compilation Compilation;
typedef dyn_array_type(Compilation *) Array_Compilation_Ptr;
typedef dyn_array_type(const Compilation *) Array_Const_Compilation_Ptr;


// Type Definitions

typedef struct Source_Position {
  u64 line;
  u64 column;
} Source_Position;
typedef dyn_array_type(Source_Position) Array_Source_Position;

typedef struct Source_File {
  Slice path;
  Slice text;
  Array_Range_u64 line_ranges;
} Source_File;
typedef dyn_array_type(Source_File) Array_Source_File;

typedef struct Source_Range {
  const Source_File * file;
  Range_u64 offsets;
} Source_Range;
typedef dyn_array_type(Source_Range) Array_Source_Range;

typedef enum {
  Module_Export_Tag_None = 0,
  Module_Export_Tag_All = 1,
  Module_Export_Tag_Selective = 2,
} Module_Export_Tag;

typedef struct Module_Export_Selective {
  Array_Value_Ptr symbols;
} Module_Export_Selective;
typedef struct Module_Export {
  Module_Export_Tag tag;
  char _tag_padding[4];
  Scope * scope;
  union {
    Module_Export_Selective Selective;
  };
} Module_Export;
typedef dyn_array_type(Module_Export) Array_Module_Export;
typedef struct Module {
  Source_File source_file;
  Source_Range exports_source_range;
  Scope * own_scope;
  Module_Export export;
} Module;
typedef dyn_array_type(Module) Array_Module;

typedef struct Parse_Error {
  Slice message;
  Source_Range source_range;
} Parse_Error;
typedef dyn_array_type(Parse_Error) Array_Parse_Error;

typedef enum Group_Tag {
  Group_Tag_Paren = 1,
  Group_Tag_Square = 2,
  Group_Tag_Curly = 3,
} Group_Tag;

const char *group_tag_name(Group_Tag value) {
  if (value == 1) return "Group_Tag_Paren";
  if (value == 2) return "Group_Tag_Square";
  if (value == 3) return "Group_Tag_Curly";
  assert(!"Unexpected value for enum Group_Tag");
  return 0;
};

typedef struct Value_View {
  Value * * values;
  u64 length;
  Source_Range source_range;
} Value_View;
typedef dyn_array_type(Value_View) Array_Value_View;

typedef enum Symbol_Type {
  Symbol_Type_Id_Like = 1,
  Symbol_Type_Operator_Like = 2,
} Symbol_Type;

const char *symbol_type_name(Symbol_Type value) {
  if (value == 1) return "Symbol_Type_Id_Like";
  if (value == 2) return "Symbol_Type_Operator_Like";
  assert(!"Unexpected value for enum Symbol_Type");
  return 0;
};

typedef struct Symbol {
  Symbol_Type type;
  u32 _type_padding;
  Slice name;
} Symbol;
typedef dyn_array_type(Symbol) Array_Symbol;

typedef struct Group {
  Group_Tag tag;
  u32 _tag_padding;
  Value_View children;
} Group;
typedef dyn_array_type(Group) Array_Group;

typedef enum {
  Token_Pattern_Tag_Invalid = 0,
  Token_Pattern_Tag_Any = 1,
  Token_Pattern_Tag_Symbol = 2,
  Token_Pattern_Tag_Group = 3,
  Token_Pattern_Tag_String = 4,
} Token_Pattern_Tag;

typedef struct Token_Pattern_Symbol {
  Slice name;
} Token_Pattern_Symbol;
typedef struct Token_Pattern_Group {
  Group_Tag tag;
} Token_Pattern_Group;
typedef struct Token_Pattern_String {
  Slice slice;
} Token_Pattern_String;
typedef struct Token_Pattern {
  Token_Pattern_Tag tag;
  char _tag_padding[4];
  Token_Pattern * or;
  union {
    Token_Pattern_Symbol Symbol;
    Token_Pattern_Group Group;
    Token_Pattern_String String;
  };
} Token_Pattern;
typedef dyn_array_type(Token_Pattern) Array_Token_Pattern;
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

typedef struct Label_Index {
  const Program * program;
  u64 value;
} Label_Index;
typedef dyn_array_type(Label_Index) Array_Label_Index;

typedef struct Label {
  u32 resolved;
  u32 offset_in_section;
  Slice name;
  Section * section;
} Label;
typedef dyn_array_type(Label) Array_Label;

typedef struct Label_Location_Diff_Patch_Info {
  Label_Index target_label_index;
  Label from;
  s32 * patch_target;
} Label_Location_Diff_Patch_Info;
typedef dyn_array_type(Label_Location_Diff_Patch_Info) Array_Label_Location_Diff_Patch_Info;

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

typedef struct Number_Literal {
  Number_Base base;
  u32 negative;
  u64 bits;
} Number_Literal;
typedef dyn_array_type(Number_Literal) Array_Number_Literal;

typedef struct Macro_Capture {
  Scope * scope;
  Slice name;
  Value_View view;
  Source_Range source_range;
} Macro_Capture;
typedef dyn_array_type(Macro_Capture) Array_Macro_Capture;

typedef struct External_Symbol {
  Slice library_name;
  Slice symbol_name;
} External_Symbol;
typedef dyn_array_type(External_Symbol) Array_External_Symbol;

typedef struct Import_Symbol {
  Slice name;
  Label_Index label32;
} Import_Symbol;
typedef dyn_array_type(Import_Symbol) Array_Import_Symbol;

typedef struct Import_Library {
  Slice name;
  Array_Import_Symbol symbols;
} Import_Library;
typedef dyn_array_type(Import_Library) Array_Import_Library;

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

typedef enum {
  Memory_Location_Tag_Instruction_Pointer_Relative = 0,
  Memory_Location_Tag_Indirect = 1,
  Memory_Location_Tag_Stack = 2,
} Memory_Location_Tag;

typedef struct Memory_Location_Instruction_Pointer_Relative {
  Label_Index label_index;
} Memory_Location_Instruction_Pointer_Relative;
typedef struct Memory_Location_Indirect {
  Register base_register;
  u32 _base_register_padding;
  s64 offset;
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
typedef dyn_array_type(Memory_Location) Array_Memory_Location;
typedef enum {
  Static_Memory_Tag_U8 = 0,
  Static_Memory_Tag_U16 = 1,
  Static_Memory_Tag_U32 = 2,
  Static_Memory_Tag_U64 = 3,
  Static_Memory_Tag_Heap = 4,
} Static_Memory_Tag;

typedef struct Static_Memory_U8 {
  u8 value;
} Static_Memory_U8;
typedef struct Static_Memory_U16 {
  u16 value;
} Static_Memory_U16;
typedef struct Static_Memory_U32 {
  u32 value;
} Static_Memory_U32;
typedef struct Static_Memory_U64 {
  u64 value;
} Static_Memory_U64;
typedef struct Static_Memory_Heap {
  const void * pointer;
} Static_Memory_Heap;
typedef struct Static_Memory {
  Static_Memory_Tag tag;
  char _tag_padding[4];
  union {
    Static_Memory_U8 U8;
    Static_Memory_U16 U16;
    Static_Memory_U32 U32;
    Static_Memory_U64 U64;
    Static_Memory_Heap Heap;
  };
} Static_Memory;
typedef dyn_array_type(Static_Memory) Array_Static_Memory;
typedef enum {
  Storage_Tag_None = 0,
  Storage_Tag_Any = 1,
  Storage_Tag_Eflags = 2,
  Storage_Tag_Register = 3,
  Storage_Tag_Xmm = 4,
  Storage_Tag_Static = 5,
  Storage_Tag_Memory = 6,
} Storage_Tag;

typedef struct Storage_Eflags {
  Compare_Type compare_type;
} Storage_Eflags;
typedef struct Storage_Register {
  Register index;
} Storage_Register;
typedef struct Storage_Xmm {
  Register index;
} Storage_Xmm;
typedef struct Storage_Static {
  Static_Memory memory;
} Storage_Static;
typedef struct Storage_Memory {
  Memory_Location location;
} Storage_Memory;
typedef struct Storage {
  Storage_Tag tag;
  char _tag_padding[4];
  u64 byte_size;
  union {
    Storage_Eflags Eflags;
    Storage_Register Register;
    Storage_Xmm Xmm;
    Storage_Static Static;
    Storage_Memory Memory;
  };
} Storage;
typedef dyn_array_type(Storage) Array_Storage;
typedef struct Relocation {
  Storage patch_at;
  Storage address_of;
} Relocation;
typedef dyn_array_type(Relocation) Array_Relocation;

typedef struct Compiler_Source_Location {
  const char * filename;
  const char * function_name;
  u64 line_number;
} Compiler_Source_Location;
typedef dyn_array_type(Compiler_Source_Location) Array_Compiler_Source_Location;

typedef enum {
  Instruction_Tag_Assembly = 0,
  Instruction_Tag_Label = 1,
  Instruction_Tag_Bytes = 2,
  Instruction_Tag_Label_Patch = 3,
} Instruction_Tag;

typedef struct Instruction_Assembly {
  const X64_Mnemonic * mnemonic;
  Storage operands[3];
} Instruction_Assembly;
typedef struct Instruction_Label {
  Label_Index index;
} Instruction_Label;
typedef struct Instruction_Bytes {
  u8 memory[15];
  u8 length;
} Instruction_Bytes;
typedef struct Instruction_Label_Patch {
  u64 offset;
  Label_Index label_index;
} Instruction_Label_Patch;
typedef struct Instruction {
  Instruction_Tag tag;
  char _tag_padding[4];
  Compiler_Source_Location compiler_source_location;
  Source_Range source_range;
  Scope * scope;
  u64 encoded_byte_size;
  union {
    Instruction_Assembly Assembly;
    Instruction_Label Label;
    Instruction_Bytes Bytes;
    Instruction_Label_Patch Label_Patch;
  };
} Instruction;
typedef dyn_array_type(Instruction) Array_Instruction;
typedef struct Code_Block {
  Label_Index start_label;
  Label_Index end_label;
  Array_Instruction instructions;
} Code_Block;
typedef dyn_array_type(Code_Block) Array_Code_Block;

typedef struct Function_Builder {
  s32 stack_reserve;
  u32 max_call_parameters_stack_size;
  Value * return_value;
  Code_Block code_block;
  u64 used_register_bitset;
  u64 register_volatile_bitset;
  u64 register_occupied_bitset;
  Storage * register_occupied_storage[32];
  Slice source;
  const Function_Info * function;
} Function_Builder;
typedef dyn_array_type(Function_Builder) Array_Function_Builder;

typedef enum Function_Argument_Mode {
  Function_Argument_Mode_Call = 0,
  Function_Argument_Mode_Body = 1,
} Function_Argument_Mode;

const char *function_argument_mode_name(Function_Argument_Mode value) {
  if (value == 0) return "Function_Argument_Mode_Call";
  if (value == 1) return "Function_Argument_Mode_Body";
  assert(!"Unexpected value for enum Function_Argument_Mode");
  return 0;
};

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

typedef enum Execution_Context_Flags {
  Execution_Context_Flags_None = 0,
  Execution_Context_Flags_Global = 1,
} Execution_Context_Flags;

const char *execution_context_flags_name(Execution_Context_Flags value) {
  if (value == 0) return "Execution_Context_Flags_None";
  if (value == 1) return "Execution_Context_Flags_Global";
  assert(!"Unexpected value for enum Execution_Context_Flags");
  return 0;
};

typedef struct Execution_Context {
  Allocator * allocator;
  Allocator * temp_allocator;
  const Value * current_compile_time_function_call_target;
  Execution_Context_Flags flags;
  s32 _flags_padding;
  Compilation * compilation;
  u64 epoch;
  Program * program;
  Scope * scope;
  Module * module;
  Mass_Result * result;
} Execution_Context;
typedef dyn_array_type(Execution_Context) Array_Execution_Context;

typedef struct User_Defined_Operator {
  Operator_Fixity fixity;
  u32 argument_count;
  Slice argument_names[2];
  Value * body;
  Scope * scope;
} User_Defined_Operator;
typedef dyn_array_type(User_Defined_Operator) Array_User_Defined_Operator;

typedef struct Operator {
  Operator_Fixity fixity;
  Operator_Associativity associativity;
  u64 precedence;
  u64 argument_count;
  Mass_Handle_Operator_Proc handler;
  void * handler_payload;
} Operator;
typedef dyn_array_type(Operator) Array_Operator;

typedef enum {
  Macro_Pattern_Tag_Any_Token_Sequence = 0,
  Macro_Pattern_Tag_Single_Token = 1,
} Macro_Pattern_Tag;

typedef struct Macro_Pattern_Single_Token {
  Token_Pattern token_pattern;
} Macro_Pattern_Single_Token;
typedef struct Macro_Pattern {
  Macro_Pattern_Tag tag;
  char _tag_padding[4];
  Slice capture_name;
  union {
    Macro_Pattern_Single_Token Single_Token;
  };
} Macro_Pattern;
typedef dyn_array_type(Macro_Pattern) Array_Macro_Pattern;
typedef struct Macro {
  Array_Macro_Pattern pattern;
  Value_View replacement;
  Scope * scope;
} Macro;
typedef dyn_array_type(Macro) Array_Macro;

typedef struct Token_Statement_Matcher {
  Token_Statement_Matcher_Proc proc;
  void * payload;
} Token_Statement_Matcher;
typedef dyn_array_type(Token_Statement_Matcher) Array_Token_Statement_Matcher;

typedef enum {
  Scope_Entry_Tag_Value = 0,
  Scope_Entry_Tag_Operator = 1,
} Scope_Entry_Tag;

typedef struct Scope_Entry_Value {
  Value * value;
} Scope_Entry_Value;
typedef struct Scope_Entry_Operator {
  Operator * maybe_prefix;
  Operator * maybe_infix_or_postfix;
} Scope_Entry_Operator;
typedef struct Scope_Entry {
  Scope_Entry_Tag tag;
  char _tag_padding[4];
  u64 epoch;
  Source_Range source_range;
  union {
    Scope_Entry_Value Value;
    Scope_Entry_Operator Operator;
  };
} Scope_Entry;
typedef dyn_array_type(Scope_Entry) Array_Scope_Entry;
typedef struct Scope {
  const Allocator * allocator;
  u64 id;
  const Scope * parent;
  Scope_Map * map;
  Array_Macro_Ptr macros;
  Array_Token_Statement_Matcher statement_matchers;
} Scope;
typedef dyn_array_type(Scope) Array_Scope;

typedef struct Value {
  const Descriptor * descriptor;
  Storage storage;
  Value * next_overload;
  u64 is_temporary;
  Source_Range source_range;
  Compiler_Source_Location compiler_source_location;
} Value;
typedef dyn_array_type(Value) Array_Value;

typedef enum Expected_Result_Storage {
  Expected_Result_Storage_None = 0,
  Expected_Result_Storage_Static = 1,
  Expected_Result_Storage_Memory = 2,
  Expected_Result_Storage_Register = 4,
  Expected_Result_Storage_Xmm = 8,
  Expected_Result_Storage_Eflags = 16,
} Expected_Result_Storage;

const char *expected_result_storage_name(Expected_Result_Storage value) {
  if (value == 0) return "Expected_Result_Storage_None";
  if (value == 1) return "Expected_Result_Storage_Static";
  if (value == 2) return "Expected_Result_Storage_Memory";
  if (value == 4) return "Expected_Result_Storage_Register";
  if (value == 8) return "Expected_Result_Storage_Xmm";
  if (value == 16) return "Expected_Result_Storage_Eflags";
  assert(!"Unexpected value for enum Expected_Result_Storage");
  return 0;
};

typedef enum {
  Expected_Result_Tag_Exact = 0,
  Expected_Result_Tag_Flexible = 1,
} Expected_Result_Tag;

typedef struct Expected_Result_Exact {
  Value * value;
} Expected_Result_Exact;
typedef struct Expected_Result_Flexible {
  const Descriptor * descriptor;
  Expected_Result_Storage storage;
  s32 _storage_padding;
  u64 register_bit_set;
} Expected_Result_Flexible;
typedef struct Expected_Result {
  Expected_Result_Tag tag;
  char _tag_padding[4];
  union {
    Expected_Result_Exact Exact;
    Expected_Result_Flexible Flexible;
  };
} Expected_Result;
typedef dyn_array_type(Expected_Result) Array_Expected_Result;
typedef struct Lazy_Value {
  Execution_Context context;
  const Descriptor * descriptor;
  Lazy_Value_Proc proc;
  void * payload;
  u64 epoch;
} Lazy_Value;
typedef dyn_array_type(Lazy_Value) Array_Lazy_Value;

typedef struct Lazy_Static_Value {
  Execution_Context context;
  Value_View expression;
} Lazy_Static_Value;
typedef dyn_array_type(Lazy_Static_Value) Array_Lazy_Static_Value;

typedef enum Memory_Layout_Item_Flags {
  Memory_Layout_Item_Flags_None = 0,
  Memory_Layout_Item_Flags_Uninitialized = 1,
} Memory_Layout_Item_Flags;

const char *memory_layout_item_flags_name(Memory_Layout_Item_Flags value) {
  if (value == 0) return "Memory_Layout_Item_Flags_None";
  if (value == 1) return "Memory_Layout_Item_Flags_Uninitialized";
  assert(!"Unexpected value for enum Memory_Layout_Item_Flags");
  return 0;
};

typedef enum {
  Memory_Layout_Item_Tag_Absolute = 0,
  Memory_Layout_Item_Tag_Base_Relative = 1,
} Memory_Layout_Item_Tag;

typedef struct Memory_Layout_Item_Absolute {
  Storage storage;
} Memory_Layout_Item_Absolute;
typedef struct Memory_Layout_Item_Base_Relative {
  s64 offset;
} Memory_Layout_Item_Base_Relative;
typedef struct Memory_Layout_Item {
  Memory_Layout_Item_Tag tag;
  char _tag_padding[4];
  Memory_Layout_Item_Flags flags;
  u32 _flags_padding;
  Slice name;
  const Descriptor * descriptor;
  Source_Range source_range;
  union {
    Memory_Layout_Item_Absolute Absolute;
    Memory_Layout_Item_Base_Relative Base_Relative;
  };
} Memory_Layout_Item;
typedef dyn_array_type(Memory_Layout_Item) Array_Memory_Layout_Item;
typedef struct Memory_Layout {
  Array_Memory_Layout_Item items;
} Memory_Layout;
typedef dyn_array_type(Memory_Layout) Array_Memory_Layout;

typedef struct Function_Return {
  Slice name;
  const Descriptor * descriptor;
  Source_Range source_range;
} Function_Return;
typedef dyn_array_type(Function_Return) Array_Function_Return;

typedef struct Function_Argument {
  Slice name;
  const Descriptor * descriptor;
  Source_Range source_range;
  Value_View maybe_default_expression;
} Function_Argument;
typedef dyn_array_type(Function_Argument) Array_Function_Argument;

typedef enum Descriptor_Function_Flags {
  Descriptor_Function_Flags_None = 0,
  Descriptor_Function_Flags_Macro = 1,
  Descriptor_Function_Flags_Compile_Time = 4,
} Descriptor_Function_Flags;

const char *descriptor_function_flags_name(Descriptor_Function_Flags value) {
  if (value == 0) return "Descriptor_Function_Flags_None";
  if (value == 1) return "Descriptor_Function_Flags_Macro";
  if (value == 4) return "Descriptor_Function_Flags_Compile_Time";
  assert(!"Unexpected value for enum Descriptor_Function_Flags");
  return 0;
};

typedef struct Function_Info {
  Descriptor_Function_Flags flags;
  u32 _flags_padding;
  Array_Function_Argument arguments;
  Scope * scope;
  Function_Return returns;
} Function_Info;
typedef dyn_array_type(Function_Info) Array_Function_Info;

typedef struct Function_Literal {
  Function_Info * info;
  Value * body;
  Value * runtime_instance;
  Value * compile_time_instance;
} Function_Literal;
typedef dyn_array_type(Function_Literal) Array_Function_Literal;

typedef enum {
  Descriptor_Tag_Opaque = 0,
  Descriptor_Tag_Function_Instance = 1,
  Descriptor_Tag_Fixed_Size_Array = 2,
  Descriptor_Tag_Struct = 3,
  Descriptor_Tag_Pointer_To = 4,
} Descriptor_Tag;

typedef struct Descriptor_Function_Instance {
  Function_Info * info;
  const Calling_Convention * calling_convention;
  Memory_Layout arguments_layout;
  Value * return_value;
} Descriptor_Function_Instance;
typedef struct Descriptor_Fixed_Size_Array {
  const Descriptor * item;
  u64 length;
} Descriptor_Fixed_Size_Array;
typedef struct Descriptor_Struct {
  u64 id;
  Memory_Layout memory_layout;
} Descriptor_Struct;
typedef struct Descriptor_Pointer_To {
  const Descriptor * descriptor;
} Descriptor_Pointer_To;
typedef struct Descriptor {
  Descriptor_Tag tag;
  char _tag_padding[4];
  Slice name;
  u64 bit_size;
  u64 bit_alignment;
  union {
    Descriptor_Function_Instance Function_Instance;
    Descriptor_Fixed_Size_Array Fixed_Size_Array;
    Descriptor_Struct Struct;
    Descriptor_Pointer_To Pointer_To;
  };
} Descriptor;
typedef dyn_array_type(Descriptor) Array_Descriptor;
typedef enum {
  Mass_Error_Tag_Unimplemented = 0,
  Mass_Error_Tag_User_Defined = 1,
  Mass_Error_Tag_Parse = 2,
  Mass_Error_Tag_Non_Trailing_Default_Argument = 3,
  Mass_Error_Tag_Expected_Static = 4,
  Mass_Error_Tag_Integer_Range = 5,
  Mass_Error_Tag_File_Open = 6,
  Mass_Error_Tag_Unexpected_Token = 7,
  Mass_Error_Tag_Operator_Infix_Suffix_Conflict = 8,
  Mass_Error_Tag_Operator_Prefix_Conflict = 9,
  Mass_Error_Tag_Undefined_Variable = 10,
  Mass_Error_Tag_Redifinition = 11,
  Mass_Error_Tag_Unknown_Field = 12,
  Mass_Error_Tag_Invalid_Identifier = 13,
  Mass_Error_Tag_Type_Mismatch = 14,
  Mass_Error_Tag_Epoch_Mismatch = 15,
  Mass_Error_Tag_No_Matching_Overload = 16,
  Mass_Error_Tag_Undecidable_Overload = 17,
} Mass_Error_Tag;

typedef struct Mass_Error_User_Defined {
  Slice name;
} Mass_Error_User_Defined;
typedef struct Mass_Error_Integer_Range {
  const Descriptor * descriptor;
} Mass_Error_Integer_Range;
typedef struct Mass_Error_File_Open {
  Slice path;
} Mass_Error_File_Open;
typedef struct Mass_Error_Unexpected_Token {
  Slice expected;
} Mass_Error_Unexpected_Token;
typedef struct Mass_Error_Operator_Infix_Suffix_Conflict {
  Slice symbol;
} Mass_Error_Operator_Infix_Suffix_Conflict;
typedef struct Mass_Error_Operator_Prefix_Conflict {
  Slice symbol;
} Mass_Error_Operator_Prefix_Conflict;
typedef struct Mass_Error_Undefined_Variable {
  Slice name;
  u64 is_operator;
} Mass_Error_Undefined_Variable;
typedef struct Mass_Error_Redifinition {
  Slice name;
  Source_Range previous_source_range;
} Mass_Error_Redifinition;
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
  Array_Value_Ptr arguments;
} Mass_Error_No_Matching_Overload;
typedef struct Mass_Error_Undecidable_Overload {
  Value * a;
  Value * b;
} Mass_Error_Undecidable_Overload;
typedef struct Mass_Error {
  Mass_Error_Tag tag;
  char _tag_padding[4];
  Slice detailed_message;
  Source_Range source_range;
  union {
    Mass_Error_User_Defined User_Defined;
    Mass_Error_Integer_Range Integer_Range;
    Mass_Error_File_Open File_Open;
    Mass_Error_Unexpected_Token Unexpected_Token;
    Mass_Error_Operator_Infix_Suffix_Conflict Operator_Infix_Suffix_Conflict;
    Mass_Error_Operator_Prefix_Conflict Operator_Prefix_Conflict;
    Mass_Error_Undefined_Variable Undefined_Variable;
    Mass_Error_Redifinition Redifinition;
    Mass_Error_Unknown_Field Unknown_Field;
    Mass_Error_Invalid_Identifier Invalid_Identifier;
    Mass_Error_Type_Mismatch Type_Mismatch;
    Mass_Error_No_Matching_Overload No_Matching_Overload;
    Mass_Error_Undecidable_Overload Undecidable_Overload;
  };
} Mass_Error;
typedef dyn_array_type(Mass_Error) Array_Mass_Error;
typedef enum {
  Mass_Result_Tag_Success = 0,
  Mass_Result_Tag_Error = 1,
} Mass_Result_Tag;

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
typedef dyn_array_type(Mass_Result) Array_Mass_Result;
typedef struct Program {
  Array_Import_Library import_libraries;
  Array_Label labels;
  Array_Label_Location_Diff_Patch_Info patch_info_array;
  Array_Value_Ptr startup_functions;
  Array_Relocation relocations;
  Value * entry_point;
  Array_Function_Builder functions;
  Program_Memory memory;
  const Calling_Convention * default_calling_convention;
} Program;
typedef dyn_array_type(Program) Array_Program;

typedef struct Calling_Convention {
  u64 register_volatile_bitset;
  Calling_Convention_Body_End_Proc body_end_proc;
  Calling_Convention_Arguments_Layout_Proc arguments_layout_proc;
  Calling_Convention_Return_Proc return_proc;
} Calling_Convention;
typedef dyn_array_type(Calling_Convention) Array_Calling_Convention;

hash_map_slice_template(Scope_Map, Scope_Entry *)
hash_map_slice_template(Macro_Replacement_Map, Value_View)
hash_map_slice_template(Jit_Import_Library_Handle_Map, void *)
hash_map_slice_template(Imported_Module_Map, Module *)
typedef struct Jit_Counters {
  u64 functions;
  u64 imports;
  u64 startup;
  u64 relocations;
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

hash_map_template(Static_Pointer_Map, const void *, Value, hash_pointer, const_void_pointer_equal)
typedef struct Compilation {
  Virtual_Memory_Buffer temp_buffer;
  Allocator * temp_allocator;
  Virtual_Memory_Buffer allocation_buffer;
  Allocator * allocator;
  Jit jit;
  Module compiler_module;
  Static_Pointer_Map * static_pointer_map;
  Imported_Module_Map * module_map;
  Scope * root_scope;
  Program * runtime_program;
  Mass_Result * result;
} Compilation;
typedef dyn_array_type(Compilation) Array_Compilation;

_Pragma("warning (pop)")

// Mass Type Reflection

static Descriptor descriptor_x64_mnemonic_pointer;
static Descriptor descriptor_void;
static Descriptor descriptor_void_pointer;
static Descriptor descriptor_char;
static Descriptor descriptor_char_pointer;
static Descriptor descriptor_descriptor;
MASS_DEFINE_OPAQUE_C_TYPE(allocator, Allocator);
MASS_DEFINE_OPAQUE_C_TYPE(virtual_memory_buffer, Virtual_Memory_Buffer);
MASS_DEFINE_OPAQUE_C_TYPE(range_u64, Range_u64);
MASS_DEFINE_OPAQUE_C_TYPE(array_range_u64, Array_Range_u64);
#undef MASS_PROCESS_BUILT_IN_TYPE

typedef dyn_array_type(Slice *) Array_Slice_Ptr;
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
static Descriptor descriptor_module_export;
static Descriptor descriptor_array_module_export;
static Descriptor descriptor_array_module_export_ptr;
static Descriptor descriptor_module_export_pointer;
static Descriptor descriptor_module_export_pointer_pointer;
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
static Descriptor descriptor_group_tag;
static Descriptor descriptor_array_group_tag;
static Descriptor descriptor_array_group_tag_ptr;
static Descriptor descriptor_group_tag_pointer;
static Descriptor descriptor_group_tag_pointer_pointer;
static Descriptor descriptor_value_view;
static Descriptor descriptor_array_value_view;
static Descriptor descriptor_array_value_view_ptr;
static Descriptor descriptor_value_view_pointer;
static Descriptor descriptor_value_view_pointer_pointer;
static Descriptor descriptor_symbol_type;
static Descriptor descriptor_array_symbol_type;
static Descriptor descriptor_array_symbol_type_ptr;
static Descriptor descriptor_symbol_type_pointer;
static Descriptor descriptor_symbol_type_pointer_pointer;
static Descriptor descriptor_symbol;
static Descriptor descriptor_array_symbol;
static Descriptor descriptor_array_symbol_ptr;
static Descriptor descriptor_symbol_pointer;
static Descriptor descriptor_symbol_pointer_pointer;
static Descriptor descriptor_group;
static Descriptor descriptor_array_group;
static Descriptor descriptor_array_group_ptr;
static Descriptor descriptor_group_pointer;
static Descriptor descriptor_group_pointer_pointer;
static Descriptor descriptor_token_pattern;
static Descriptor descriptor_array_token_pattern;
static Descriptor descriptor_array_token_pattern_ptr;
static Descriptor descriptor_token_pattern_pointer;
static Descriptor descriptor_token_pattern_pointer_pointer;
static Descriptor descriptor_section_permissions;
static Descriptor descriptor_array_section_permissions;
static Descriptor descriptor_array_section_permissions_ptr;
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
static Descriptor descriptor_register_pointer;
static Descriptor descriptor_register_pointer_pointer;
static Descriptor descriptor_label_index;
static Descriptor descriptor_array_label_index;
static Descriptor descriptor_array_label_index_ptr;
static Descriptor descriptor_label_index_pointer;
static Descriptor descriptor_label_index_pointer_pointer;
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
static Descriptor descriptor_number_base_pointer;
static Descriptor descriptor_number_base_pointer_pointer;
static Descriptor descriptor_number_literal;
static Descriptor descriptor_array_number_literal;
static Descriptor descriptor_array_number_literal_ptr;
static Descriptor descriptor_number_literal_pointer;
static Descriptor descriptor_number_literal_pointer_pointer;
static Descriptor descriptor_macro_capture;
static Descriptor descriptor_array_macro_capture;
static Descriptor descriptor_array_macro_capture_ptr;
static Descriptor descriptor_macro_capture_pointer;
static Descriptor descriptor_macro_capture_pointer_pointer;
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
static Descriptor descriptor_compare_type_pointer;
static Descriptor descriptor_compare_type_pointer_pointer;
static Descriptor descriptor_stack_area;
static Descriptor descriptor_array_stack_area;
static Descriptor descriptor_array_stack_area_ptr;
static Descriptor descriptor_stack_area_pointer;
static Descriptor descriptor_stack_area_pointer_pointer;
static Descriptor descriptor_memory_location;
static Descriptor descriptor_array_memory_location;
static Descriptor descriptor_array_memory_location_ptr;
static Descriptor descriptor_memory_location_pointer;
static Descriptor descriptor_memory_location_pointer_pointer;
static Descriptor descriptor_static_memory;
static Descriptor descriptor_array_static_memory;
static Descriptor descriptor_array_static_memory_ptr;
static Descriptor descriptor_static_memory_pointer;
static Descriptor descriptor_static_memory_pointer_pointer;
static Descriptor descriptor_storage;
static Descriptor descriptor_array_storage;
static Descriptor descriptor_array_storage_ptr;
static Descriptor descriptor_storage_pointer;
static Descriptor descriptor_storage_pointer_pointer;
static Descriptor descriptor_relocation;
static Descriptor descriptor_array_relocation;
static Descriptor descriptor_array_relocation_ptr;
static Descriptor descriptor_relocation_pointer;
static Descriptor descriptor_relocation_pointer_pointer;
static Descriptor descriptor_compiler_source_location;
static Descriptor descriptor_array_compiler_source_location;
static Descriptor descriptor_array_compiler_source_location_ptr;
static Descriptor descriptor_compiler_source_location_pointer;
static Descriptor descriptor_compiler_source_location_pointer_pointer;
static Descriptor descriptor_instruction;
static Descriptor descriptor_array_instruction;
static Descriptor descriptor_array_instruction_ptr;
static Descriptor descriptor_instruction_pointer;
static Descriptor descriptor_instruction_pointer_pointer;
static Descriptor descriptor_code_block;
static Descriptor descriptor_array_code_block;
static Descriptor descriptor_array_code_block_ptr;
static Descriptor descriptor_code_block_pointer;
static Descriptor descriptor_code_block_pointer_pointer;
static Descriptor descriptor_function_builder;
static Descriptor descriptor_array_function_builder;
static Descriptor descriptor_array_function_builder_ptr;
static Descriptor descriptor_function_builder_pointer;
static Descriptor descriptor_function_builder_pointer_pointer;
static Descriptor descriptor_function_argument_mode;
static Descriptor descriptor_array_function_argument_mode;
static Descriptor descriptor_array_function_argument_mode_ptr;
static Descriptor descriptor_function_argument_mode_pointer;
static Descriptor descriptor_function_argument_mode_pointer_pointer;
static Descriptor descriptor_operator_fixity;
static Descriptor descriptor_array_operator_fixity;
static Descriptor descriptor_array_operator_fixity_ptr;
static Descriptor descriptor_operator_fixity_pointer;
static Descriptor descriptor_operator_fixity_pointer_pointer;
static Descriptor descriptor_operator_associativity;
static Descriptor descriptor_array_operator_associativity;
static Descriptor descriptor_array_operator_associativity_ptr;
static Descriptor descriptor_operator_associativity_pointer;
static Descriptor descriptor_operator_associativity_pointer_pointer;
static Descriptor descriptor_function_layout;
static Descriptor descriptor_array_function_layout;
static Descriptor descriptor_array_function_layout_ptr;
static Descriptor descriptor_function_layout_pointer;
static Descriptor descriptor_function_layout_pointer_pointer;
static Descriptor descriptor_execution_context_flags;
static Descriptor descriptor_array_execution_context_flags;
static Descriptor descriptor_array_execution_context_flags_ptr;
static Descriptor descriptor_execution_context_flags_pointer;
static Descriptor descriptor_execution_context_flags_pointer_pointer;
static Descriptor descriptor_execution_context;
static Descriptor descriptor_array_execution_context;
static Descriptor descriptor_array_execution_context_ptr;
static Descriptor descriptor_execution_context_pointer;
static Descriptor descriptor_execution_context_pointer_pointer;
static Descriptor descriptor_user_defined_operator;
static Descriptor descriptor_array_user_defined_operator;
static Descriptor descriptor_array_user_defined_operator_ptr;
static Descriptor descriptor_user_defined_operator_pointer;
static Descriptor descriptor_user_defined_operator_pointer_pointer;
static Descriptor descriptor_operator;
static Descriptor descriptor_array_operator;
static Descriptor descriptor_array_operator_ptr;
static Descriptor descriptor_operator_pointer;
static Descriptor descriptor_operator_pointer_pointer;
static Descriptor descriptor_macro_pattern;
static Descriptor descriptor_array_macro_pattern;
static Descriptor descriptor_array_macro_pattern_ptr;
static Descriptor descriptor_macro_pattern_pointer;
static Descriptor descriptor_macro_pattern_pointer_pointer;
static Descriptor descriptor_macro;
static Descriptor descriptor_array_macro;
static Descriptor descriptor_array_macro_ptr;
static Descriptor descriptor_macro_pointer;
static Descriptor descriptor_macro_pointer_pointer;
static Descriptor descriptor_token_statement_matcher;
static Descriptor descriptor_array_token_statement_matcher;
static Descriptor descriptor_array_token_statement_matcher_ptr;
static Descriptor descriptor_token_statement_matcher_pointer;
static Descriptor descriptor_token_statement_matcher_pointer_pointer;
static Descriptor descriptor_scope_entry;
static Descriptor descriptor_array_scope_entry;
static Descriptor descriptor_array_scope_entry_ptr;
static Descriptor descriptor_scope_entry_pointer;
static Descriptor descriptor_scope_entry_pointer_pointer;
static Descriptor descriptor_scope;
static Descriptor descriptor_array_scope;
static Descriptor descriptor_array_scope_ptr;
static Descriptor descriptor_scope_pointer;
static Descriptor descriptor_scope_pointer_pointer;
static Descriptor descriptor_value;
static Descriptor descriptor_array_value;
static Descriptor descriptor_array_value_ptr;
static Descriptor descriptor_value_pointer;
static Descriptor descriptor_value_pointer_pointer;
static Descriptor descriptor_expected_result_storage;
static Descriptor descriptor_array_expected_result_storage;
static Descriptor descriptor_array_expected_result_storage_ptr;
static Descriptor descriptor_expected_result_storage_pointer;
static Descriptor descriptor_expected_result_storage_pointer_pointer;
static Descriptor descriptor_expected_result;
static Descriptor descriptor_array_expected_result;
static Descriptor descriptor_array_expected_result_ptr;
static Descriptor descriptor_expected_result_pointer;
static Descriptor descriptor_expected_result_pointer_pointer;
static Descriptor descriptor_lazy_value_proc;
static Descriptor descriptor_lazy_value;
static Descriptor descriptor_array_lazy_value;
static Descriptor descriptor_array_lazy_value_ptr;
static Descriptor descriptor_lazy_value_pointer;
static Descriptor descriptor_lazy_value_pointer_pointer;
static Descriptor descriptor_lazy_static_value;
static Descriptor descriptor_array_lazy_static_value;
static Descriptor descriptor_array_lazy_static_value_ptr;
static Descriptor descriptor_lazy_static_value_pointer;
static Descriptor descriptor_lazy_static_value_pointer_pointer;
static Descriptor descriptor_mass_handle_operator_proc;
static Descriptor descriptor_memory_layout_item_flags;
static Descriptor descriptor_array_memory_layout_item_flags;
static Descriptor descriptor_array_memory_layout_item_flags_ptr;
static Descriptor descriptor_memory_layout_item_flags_pointer;
static Descriptor descriptor_memory_layout_item_flags_pointer_pointer;
static Descriptor descriptor_memory_layout_item;
static Descriptor descriptor_array_memory_layout_item;
static Descriptor descriptor_array_memory_layout_item_ptr;
static Descriptor descriptor_memory_layout_item_pointer;
static Descriptor descriptor_memory_layout_item_pointer_pointer;
static Descriptor descriptor_memory_layout;
static Descriptor descriptor_array_memory_layout;
static Descriptor descriptor_array_memory_layout_ptr;
static Descriptor descriptor_memory_layout_pointer;
static Descriptor descriptor_memory_layout_pointer_pointer;
static Descriptor descriptor_function_return;
static Descriptor descriptor_array_function_return;
static Descriptor descriptor_array_function_return_ptr;
static Descriptor descriptor_function_return_pointer;
static Descriptor descriptor_function_return_pointer_pointer;
static Descriptor descriptor_function_argument;
static Descriptor descriptor_array_function_argument;
static Descriptor descriptor_array_function_argument_ptr;
static Descriptor descriptor_function_argument_pointer;
static Descriptor descriptor_function_argument_pointer_pointer;
static Descriptor descriptor_descriptor_function_flags;
static Descriptor descriptor_array_descriptor_function_flags;
static Descriptor descriptor_array_descriptor_function_flags_ptr;
static Descriptor descriptor_descriptor_function_flags_pointer;
static Descriptor descriptor_descriptor_function_flags_pointer_pointer;
static Descriptor descriptor_function_info;
static Descriptor descriptor_array_function_info;
static Descriptor descriptor_array_function_info_ptr;
static Descriptor descriptor_function_info_pointer;
static Descriptor descriptor_function_info_pointer_pointer;
static Descriptor descriptor_function_literal;
static Descriptor descriptor_array_function_literal;
static Descriptor descriptor_array_function_literal_ptr;
static Descriptor descriptor_function_literal_pointer;
static Descriptor descriptor_function_literal_pointer_pointer;
static Descriptor descriptor_descriptor;
static Descriptor descriptor_array_descriptor;
static Descriptor descriptor_array_descriptor_ptr;
static Descriptor descriptor_descriptor_pointer;
static Descriptor descriptor_descriptor_pointer_pointer;
static Descriptor descriptor_mass_error;
static Descriptor descriptor_array_mass_error;
static Descriptor descriptor_array_mass_error_ptr;
static Descriptor descriptor_mass_error_pointer;
static Descriptor descriptor_mass_error_pointer_pointer;
static Descriptor descriptor_mass_result;
static Descriptor descriptor_array_mass_result;
static Descriptor descriptor_array_mass_result_ptr;
static Descriptor descriptor_mass_result_pointer;
static Descriptor descriptor_mass_result_pointer_pointer;
static Descriptor descriptor_program;
static Descriptor descriptor_array_program;
static Descriptor descriptor_array_program_ptr;
static Descriptor descriptor_program_pointer;
static Descriptor descriptor_program_pointer_pointer;
static Descriptor descriptor_calling_convention_body_end_proc;
static Descriptor descriptor_calling_convention_arguments_layout_proc;
static Descriptor descriptor_calling_convention_return_proc;
static Descriptor descriptor_calling_convention;
static Descriptor descriptor_array_calling_convention;
static Descriptor descriptor_array_calling_convention_ptr;
static Descriptor descriptor_calling_convention_pointer;
static Descriptor descriptor_calling_convention_pointer_pointer;
static Descriptor descriptor_token_statement_matcher_proc;
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
MASS_DEFINE_OPAQUE_C_TYPE(static_pointer_map, Static_Pointer_Map);
static Descriptor descriptor_compilation;
static Descriptor descriptor_array_compilation;
static Descriptor descriptor_array_compilation_ptr;
static Descriptor descriptor_compilation_pointer;
static Descriptor descriptor_compilation_pointer_pointer;
static Descriptor descriptor_u8;
static Descriptor descriptor_array_u8;
static Descriptor descriptor_u8_pointer;
static Descriptor descriptor_u8_pointer_pointer;
static Descriptor descriptor_u16;
static Descriptor descriptor_array_u16;
static Descriptor descriptor_u16_pointer;
static Descriptor descriptor_u16_pointer_pointer;
static Descriptor descriptor_u32;
static Descriptor descriptor_array_u32;
static Descriptor descriptor_u32_pointer;
static Descriptor descriptor_u32_pointer_pointer;
static Descriptor descriptor_u64;
static Descriptor descriptor_array_u64;
static Descriptor descriptor_u64_pointer;
static Descriptor descriptor_u64_pointer_pointer;
static Descriptor descriptor_s8;
static Descriptor descriptor_array_s8;
static Descriptor descriptor_s8_pointer;
static Descriptor descriptor_s8_pointer_pointer;
static Descriptor descriptor_s16;
static Descriptor descriptor_array_s16;
static Descriptor descriptor_s16_pointer;
static Descriptor descriptor_s16_pointer_pointer;
static Descriptor descriptor_s32;
static Descriptor descriptor_array_s32;
static Descriptor descriptor_s32_pointer;
static Descriptor descriptor_s32_pointer_pointer;
static Descriptor descriptor_s64;
static Descriptor descriptor_array_s64;
static Descriptor descriptor_s64_pointer;
static Descriptor descriptor_s64_pointer_pointer;
static Descriptor descriptor_f32;
static Descriptor descriptor_array_f32;
static Descriptor descriptor_f32_pointer;
static Descriptor descriptor_f32_pointer_pointer;
static Descriptor descriptor_f64;
static Descriptor descriptor_array_f64;
static Descriptor descriptor_f64_pointer;
static Descriptor descriptor_f64_pointer_pointer;
static Descriptor descriptor_slice;
static Descriptor descriptor_array_slice;
static Descriptor descriptor_array_slice_ptr;
static Descriptor descriptor_slice_pointer;
static Descriptor descriptor_slice_pointer_pointer;
static Descriptor descriptor_storage_3 = MASS_DESCRIPTOR_STATIC_ARRAY(Storage, 3, &descriptor_storage);
static Descriptor descriptor_u8_15 = MASS_DESCRIPTOR_STATIC_ARRAY(u8, 15, &descriptor_u8);
static Descriptor descriptor_storage_pointer_32 = MASS_DESCRIPTOR_STATIC_ARRAY(Storage *, 32, &descriptor_storage_pointer);
static Descriptor descriptor_u8_16 = MASS_DESCRIPTOR_STATIC_ARRAY(u8, 16, &descriptor_u8);
static Descriptor descriptor_slice_2 = MASS_DESCRIPTOR_STATIC_ARRAY(Slice, 2, &descriptor_slice);
MASS_DEFINE_OPAQUE_C_TYPE(array_source_position_ptr, Array_Source_Position_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_source_position, Array_Source_Position)
MASS_DEFINE_STRUCT_DESCRIPTOR(1, source_position, Source_Position,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("line"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Source_Position, line),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("column"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Source_Position, column),
  },
);
MASS_DEFINE_TYPE_VALUE(source_position);
MASS_DEFINE_OPAQUE_C_TYPE(array_source_file_ptr, Array_Source_File_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_source_file, Array_Source_File)
MASS_DEFINE_STRUCT_DESCRIPTOR(2, source_file, Source_File,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("path"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Source_File, path),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("text"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Source_File, text),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("line_ranges"),
    .descriptor = &descriptor_array_range_u64,
    .Base_Relative.offset = offsetof(Source_File, line_ranges),
  },
);
MASS_DEFINE_TYPE_VALUE(source_file);
MASS_DEFINE_OPAQUE_C_TYPE(array_source_range_ptr, Array_Source_Range_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_source_range, Array_Source_Range)
MASS_DEFINE_STRUCT_DESCRIPTOR(3, source_range, Source_Range,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("file"),
    .descriptor = &descriptor_source_file_pointer,
    .Base_Relative.offset = offsetof(Source_Range, file),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("offsets"),
    .descriptor = &descriptor_range_u64,
    .Base_Relative.offset = offsetof(Source_Range, offsets),
  },
);
MASS_DEFINE_TYPE_VALUE(source_range);
/*union struct start */
MASS_DEFINE_OPAQUE_C_TYPE(array_module_export_ptr, Array_Module_Export_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_module_export, Array_Module_Export)
MASS_DEFINE_OPAQUE_C_TYPE(module_export_tag, Module_Export_Tag)
static C_Enum_Item module_export_tag_items[] = {
{ .name = slice_literal_fields("None"), .value = 0 },
{ .name = slice_literal_fields("All"), .value = 1 },
{ .name = slice_literal_fields("Selective"), .value = 2 },
};
MASS_DEFINE_STRUCT_DESCRIPTOR(4, module_export_selective, Module_Export_Selective,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("symbols"),
    .descriptor = &descriptor_array_value_ptr,
    .Base_Relative.offset = offsetof(Module_Export_Selective, symbols),
  },
);
MASS_DEFINE_TYPE_VALUE(module_export_selective);
MASS_DEFINE_STRUCT_DESCRIPTOR(5, module_export, Module_Export,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_module_export_tag,
    .Base_Relative.offset = offsetof(Module_Export, tag),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("scope"),
    .descriptor = &descriptor_scope_pointer,
    .Base_Relative.offset = offsetof(Module_Export, scope),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Selective"),
    .descriptor = &descriptor_module_export_selective,
    .Base_Relative.offset = offsetof(Module_Export, Selective),
  },
);
MASS_DEFINE_TYPE_VALUE(module_export);
/*union struct end*/
MASS_DEFINE_OPAQUE_C_TYPE(array_module_ptr, Array_Module_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_module, Array_Module)
MASS_DEFINE_STRUCT_DESCRIPTOR(6, module, Module,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("source_file"),
    .descriptor = &descriptor_source_file,
    .Base_Relative.offset = offsetof(Module, source_file),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("exports_source_range"),
    .descriptor = &descriptor_source_range,
    .Base_Relative.offset = offsetof(Module, exports_source_range),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("own_scope"),
    .descriptor = &descriptor_scope_pointer,
    .Base_Relative.offset = offsetof(Module, own_scope),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("export"),
    .descriptor = &descriptor_module_export,
    .Base_Relative.offset = offsetof(Module, export),
  },
);
MASS_DEFINE_TYPE_VALUE(module);
MASS_DEFINE_OPAQUE_C_TYPE(array_parse_error_ptr, Array_Parse_Error_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_parse_error, Array_Parse_Error)
MASS_DEFINE_STRUCT_DESCRIPTOR(7, parse_error, Parse_Error,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("message"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Parse_Error, message),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("source_range"),
    .descriptor = &descriptor_source_range,
    .Base_Relative.offset = offsetof(Parse_Error, source_range),
  },
);
MASS_DEFINE_TYPE_VALUE(parse_error);
MASS_DEFINE_OPAQUE_C_TYPE(group_tag, Group_Tag)
static C_Enum_Item group_tag_items[] = {
{ .name = slice_literal_fields("Paren"), .value = 1 },
{ .name = slice_literal_fields("Square"), .value = 2 },
{ .name = slice_literal_fields("Curly"), .value = 3 },
};
MASS_DEFINE_OPAQUE_C_TYPE(array_value_view_ptr, Array_Value_View_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_value_view, Array_Value_View)
MASS_DEFINE_STRUCT_DESCRIPTOR(8, value_view, Value_View,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("values"),
    .descriptor = &descriptor_value_pointer_pointer,
    .Base_Relative.offset = offsetof(Value_View, values),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("length"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Value_View, length),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("source_range"),
    .descriptor = &descriptor_source_range,
    .Base_Relative.offset = offsetof(Value_View, source_range),
  },
);
MASS_DEFINE_TYPE_VALUE(value_view);
MASS_DEFINE_OPAQUE_C_TYPE(symbol_type, Symbol_Type)
static C_Enum_Item symbol_type_items[] = {
{ .name = slice_literal_fields("Id_Like"), .value = 1 },
{ .name = slice_literal_fields("Operator_Like"), .value = 2 },
};
MASS_DEFINE_OPAQUE_C_TYPE(array_symbol_ptr, Array_Symbol_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_symbol, Array_Symbol)
MASS_DEFINE_STRUCT_DESCRIPTOR(9, symbol, Symbol,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("type"),
    .descriptor = &descriptor_symbol_type,
    .Base_Relative.offset = offsetof(Symbol, type),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("_type_padding"),
    .descriptor = &descriptor_u32,
    .Base_Relative.offset = offsetof(Symbol, _type_padding),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Symbol, name),
  },
);
MASS_DEFINE_TYPE_VALUE(symbol);
MASS_DEFINE_OPAQUE_C_TYPE(array_group_ptr, Array_Group_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_group, Array_Group)
MASS_DEFINE_STRUCT_DESCRIPTOR(10, group, Group,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_group_tag,
    .Base_Relative.offset = offsetof(Group, tag),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("_tag_padding"),
    .descriptor = &descriptor_u32,
    .Base_Relative.offset = offsetof(Group, _tag_padding),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("children"),
    .descriptor = &descriptor_value_view,
    .Base_Relative.offset = offsetof(Group, children),
  },
);
MASS_DEFINE_TYPE_VALUE(group);
/*union struct start */
MASS_DEFINE_OPAQUE_C_TYPE(array_token_pattern_ptr, Array_Token_Pattern_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_token_pattern, Array_Token_Pattern)
MASS_DEFINE_OPAQUE_C_TYPE(token_pattern_tag, Token_Pattern_Tag)
static C_Enum_Item token_pattern_tag_items[] = {
{ .name = slice_literal_fields("Invalid"), .value = 0 },
{ .name = slice_literal_fields("Any"), .value = 1 },
{ .name = slice_literal_fields("Symbol"), .value = 2 },
{ .name = slice_literal_fields("Group"), .value = 3 },
{ .name = slice_literal_fields("String"), .value = 4 },
};
MASS_DEFINE_STRUCT_DESCRIPTOR(11, token_pattern_symbol, Token_Pattern_Symbol,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Token_Pattern_Symbol, name),
  },
);
MASS_DEFINE_TYPE_VALUE(token_pattern_symbol);
MASS_DEFINE_STRUCT_DESCRIPTOR(12, token_pattern_group, Token_Pattern_Group,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_group_tag,
    .Base_Relative.offset = offsetof(Token_Pattern_Group, tag),
  },
);
MASS_DEFINE_TYPE_VALUE(token_pattern_group);
MASS_DEFINE_STRUCT_DESCRIPTOR(13, token_pattern_string, Token_Pattern_String,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("slice"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Token_Pattern_String, slice),
  },
);
MASS_DEFINE_TYPE_VALUE(token_pattern_string);
MASS_DEFINE_STRUCT_DESCRIPTOR(14, token_pattern, Token_Pattern,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_token_pattern_tag,
    .Base_Relative.offset = offsetof(Token_Pattern, tag),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("or"),
    .descriptor = &descriptor_token_pattern_pointer,
    .Base_Relative.offset = offsetof(Token_Pattern, or),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Symbol"),
    .descriptor = &descriptor_token_pattern_symbol,
    .Base_Relative.offset = offsetof(Token_Pattern, Symbol),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Group"),
    .descriptor = &descriptor_token_pattern_group,
    .Base_Relative.offset = offsetof(Token_Pattern, Group),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("String"),
    .descriptor = &descriptor_token_pattern_string,
    .Base_Relative.offset = offsetof(Token_Pattern, String),
  },
);
MASS_DEFINE_TYPE_VALUE(token_pattern);
/*union struct end*/
MASS_DEFINE_OPAQUE_C_TYPE(section_permissions, Section_Permissions)
static C_Enum_Item section_permissions_items[] = {
{ .name = slice_literal_fields("Read"), .value = 1 },
{ .name = slice_literal_fields("Write"), .value = 2 },
{ .name = slice_literal_fields("Execute"), .value = 4 },
};
MASS_DEFINE_OPAQUE_C_TYPE(array_section_ptr, Array_Section_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_section, Array_Section)
MASS_DEFINE_STRUCT_DESCRIPTOR(15, section, Section,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("buffer"),
    .descriptor = &descriptor_virtual_memory_buffer,
    .Base_Relative.offset = offsetof(Section, buffer),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Section, name),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("base_rva"),
    .descriptor = &descriptor_u32,
    .Base_Relative.offset = offsetof(Section, base_rva),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("permissions"),
    .descriptor = &descriptor_section_permissions,
    .Base_Relative.offset = offsetof(Section, permissions),
  },
);
MASS_DEFINE_TYPE_VALUE(section);
MASS_DEFINE_OPAQUE_C_TYPE(array_program_memory_ptr, Array_Program_Memory_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_program_memory, Array_Program_Memory)
MASS_DEFINE_STRUCT_DESCRIPTOR(16, program_memory, Program_Memory,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("buffer"),
    .descriptor = &descriptor_virtual_memory_buffer,
    .Base_Relative.offset = offsetof(Program_Memory, buffer),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("rw_data"),
    .descriptor = &descriptor_section,
    .Base_Relative.offset = offsetof(Program_Memory, rw_data),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("code"),
    .descriptor = &descriptor_section,
    .Base_Relative.offset = offsetof(Program_Memory, code),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("ro_data"),
    .descriptor = &descriptor_section,
    .Base_Relative.offset = offsetof(Program_Memory, ro_data),
  },
);
MASS_DEFINE_TYPE_VALUE(program_memory);
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
MASS_DEFINE_OPAQUE_C_TYPE(array_label_index_ptr, Array_Label_Index_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_label_index, Array_Label_Index)
MASS_DEFINE_STRUCT_DESCRIPTOR(17, label_index, Label_Index,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("program"),
    .descriptor = &descriptor_program_pointer,
    .Base_Relative.offset = offsetof(Label_Index, program),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("value"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Label_Index, value),
  },
);
MASS_DEFINE_TYPE_VALUE(label_index);
MASS_DEFINE_OPAQUE_C_TYPE(array_label_ptr, Array_Label_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_label, Array_Label)
MASS_DEFINE_STRUCT_DESCRIPTOR(18, label, Label,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("resolved"),
    .descriptor = &descriptor_u32,
    .Base_Relative.offset = offsetof(Label, resolved),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("offset_in_section"),
    .descriptor = &descriptor_u32,
    .Base_Relative.offset = offsetof(Label, offset_in_section),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Label, name),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("section"),
    .descriptor = &descriptor_section_pointer,
    .Base_Relative.offset = offsetof(Label, section),
  },
);
MASS_DEFINE_TYPE_VALUE(label);
MASS_DEFINE_OPAQUE_C_TYPE(array_label_location_diff_patch_info_ptr, Array_Label_Location_Diff_Patch_Info_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_label_location_diff_patch_info, Array_Label_Location_Diff_Patch_Info)
MASS_DEFINE_STRUCT_DESCRIPTOR(19, label_location_diff_patch_info, Label_Location_Diff_Patch_Info,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("target_label_index"),
    .descriptor = &descriptor_label_index,
    .Base_Relative.offset = offsetof(Label_Location_Diff_Patch_Info, target_label_index),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("from"),
    .descriptor = &descriptor_label,
    .Base_Relative.offset = offsetof(Label_Location_Diff_Patch_Info, from),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("patch_target"),
    .descriptor = &descriptor_s32_pointer,
    .Base_Relative.offset = offsetof(Label_Location_Diff_Patch_Info, patch_target),
  },
);
MASS_DEFINE_TYPE_VALUE(label_location_diff_patch_info);
MASS_DEFINE_OPAQUE_C_TYPE(number_base, Number_Base)
static C_Enum_Item number_base_items[] = {
{ .name = slice_literal_fields("2"), .value = 2 },
{ .name = slice_literal_fields("10"), .value = 10 },
{ .name = slice_literal_fields("16"), .value = 16 },
};
MASS_DEFINE_OPAQUE_C_TYPE(array_number_literal_ptr, Array_Number_Literal_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_number_literal, Array_Number_Literal)
MASS_DEFINE_STRUCT_DESCRIPTOR(20, number_literal, Number_Literal,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("base"),
    .descriptor = &descriptor_number_base,
    .Base_Relative.offset = offsetof(Number_Literal, base),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("negative"),
    .descriptor = &descriptor_u32,
    .Base_Relative.offset = offsetof(Number_Literal, negative),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("bits"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Number_Literal, bits),
  },
);
MASS_DEFINE_TYPE_VALUE(number_literal);
MASS_DEFINE_OPAQUE_C_TYPE(array_macro_capture_ptr, Array_Macro_Capture_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_macro_capture, Array_Macro_Capture)
MASS_DEFINE_STRUCT_DESCRIPTOR(21, macro_capture, Macro_Capture,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("scope"),
    .descriptor = &descriptor_scope_pointer,
    .Base_Relative.offset = offsetof(Macro_Capture, scope),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Macro_Capture, name),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("view"),
    .descriptor = &descriptor_value_view,
    .Base_Relative.offset = offsetof(Macro_Capture, view),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("source_range"),
    .descriptor = &descriptor_source_range,
    .Base_Relative.offset = offsetof(Macro_Capture, source_range),
  },
);
MASS_DEFINE_TYPE_VALUE(macro_capture);
MASS_DEFINE_OPAQUE_C_TYPE(array_external_symbol_ptr, Array_External_Symbol_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_external_symbol, Array_External_Symbol)
MASS_DEFINE_STRUCT_DESCRIPTOR(22, external_symbol, External_Symbol,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("library_name"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(External_Symbol, library_name),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("symbol_name"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(External_Symbol, symbol_name),
  },
);
MASS_DEFINE_TYPE_VALUE(external_symbol);
MASS_DEFINE_OPAQUE_C_TYPE(array_import_symbol_ptr, Array_Import_Symbol_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_import_symbol, Array_Import_Symbol)
MASS_DEFINE_STRUCT_DESCRIPTOR(23, import_symbol, Import_Symbol,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Import_Symbol, name),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("label32"),
    .descriptor = &descriptor_label_index,
    .Base_Relative.offset = offsetof(Import_Symbol, label32),
  },
);
MASS_DEFINE_TYPE_VALUE(import_symbol);
MASS_DEFINE_OPAQUE_C_TYPE(array_import_library_ptr, Array_Import_Library_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_import_library, Array_Import_Library)
MASS_DEFINE_STRUCT_DESCRIPTOR(24, import_library, Import_Library,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Import_Library, name),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("symbols"),
    .descriptor = &descriptor_array_import_symbol,
    .Base_Relative.offset = offsetof(Import_Library, symbols),
  },
);
MASS_DEFINE_TYPE_VALUE(import_library);
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
MASS_DEFINE_OPAQUE_C_TYPE(stack_area, Stack_Area)
static C_Enum_Item stack_area_items[] = {
{ .name = slice_literal_fields("Local"), .value = 0 },
{ .name = slice_literal_fields("Received_Argument"), .value = 1 },
{ .name = slice_literal_fields("Call_Target_Argument"), .value = 2 },
};
/*union struct start */
MASS_DEFINE_OPAQUE_C_TYPE(array_memory_location_ptr, Array_Memory_Location_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_memory_location, Array_Memory_Location)
MASS_DEFINE_OPAQUE_C_TYPE(memory_location_tag, Memory_Location_Tag)
static C_Enum_Item memory_location_tag_items[] = {
{ .name = slice_literal_fields("Instruction_Pointer_Relative"), .value = 0 },
{ .name = slice_literal_fields("Indirect"), .value = 1 },
{ .name = slice_literal_fields("Stack"), .value = 2 },
};
MASS_DEFINE_STRUCT_DESCRIPTOR(25, memory_location_instruction_pointer_relative, Memory_Location_Instruction_Pointer_Relative,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("label_index"),
    .descriptor = &descriptor_label_index,
    .Base_Relative.offset = offsetof(Memory_Location_Instruction_Pointer_Relative, label_index),
  },
);
MASS_DEFINE_TYPE_VALUE(memory_location_instruction_pointer_relative);
MASS_DEFINE_STRUCT_DESCRIPTOR(26, memory_location_indirect, Memory_Location_Indirect,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("base_register"),
    .descriptor = &descriptor_register,
    .Base_Relative.offset = offsetof(Memory_Location_Indirect, base_register),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("_base_register_padding"),
    .descriptor = &descriptor_u32,
    .Base_Relative.offset = offsetof(Memory_Location_Indirect, _base_register_padding),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("offset"),
    .descriptor = &descriptor_s64,
    .Base_Relative.offset = offsetof(Memory_Location_Indirect, offset),
  },
);
MASS_DEFINE_TYPE_VALUE(memory_location_indirect);
MASS_DEFINE_STRUCT_DESCRIPTOR(27, memory_location_stack, Memory_Location_Stack,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("area"),
    .descriptor = &descriptor_stack_area,
    .Base_Relative.offset = offsetof(Memory_Location_Stack, area),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("offset"),
    .descriptor = &descriptor_s32,
    .Base_Relative.offset = offsetof(Memory_Location_Stack, offset),
  },
);
MASS_DEFINE_TYPE_VALUE(memory_location_stack);
MASS_DEFINE_STRUCT_DESCRIPTOR(28, memory_location, Memory_Location,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_memory_location_tag,
    .Base_Relative.offset = offsetof(Memory_Location, tag),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Instruction_Pointer_Relative"),
    .descriptor = &descriptor_memory_location_instruction_pointer_relative,
    .Base_Relative.offset = offsetof(Memory_Location, Instruction_Pointer_Relative),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Indirect"),
    .descriptor = &descriptor_memory_location_indirect,
    .Base_Relative.offset = offsetof(Memory_Location, Indirect),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Stack"),
    .descriptor = &descriptor_memory_location_stack,
    .Base_Relative.offset = offsetof(Memory_Location, Stack),
  },
);
MASS_DEFINE_TYPE_VALUE(memory_location);
/*union struct end*/
/*union struct start */
MASS_DEFINE_OPAQUE_C_TYPE(array_static_memory_ptr, Array_Static_Memory_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_static_memory, Array_Static_Memory)
MASS_DEFINE_OPAQUE_C_TYPE(static_memory_tag, Static_Memory_Tag)
static C_Enum_Item static_memory_tag_items[] = {
{ .name = slice_literal_fields("U8"), .value = 0 },
{ .name = slice_literal_fields("U16"), .value = 1 },
{ .name = slice_literal_fields("U32"), .value = 2 },
{ .name = slice_literal_fields("U64"), .value = 3 },
{ .name = slice_literal_fields("Heap"), .value = 4 },
};
MASS_DEFINE_STRUCT_DESCRIPTOR(29, static_memory_u8, Static_Memory_U8,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("value"),
    .descriptor = &descriptor_u8,
    .Base_Relative.offset = offsetof(Static_Memory_U8, value),
  },
);
MASS_DEFINE_TYPE_VALUE(static_memory_u8);
MASS_DEFINE_STRUCT_DESCRIPTOR(30, static_memory_u16, Static_Memory_U16,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("value"),
    .descriptor = &descriptor_u16,
    .Base_Relative.offset = offsetof(Static_Memory_U16, value),
  },
);
MASS_DEFINE_TYPE_VALUE(static_memory_u16);
MASS_DEFINE_STRUCT_DESCRIPTOR(31, static_memory_u32, Static_Memory_U32,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("value"),
    .descriptor = &descriptor_u32,
    .Base_Relative.offset = offsetof(Static_Memory_U32, value),
  },
);
MASS_DEFINE_TYPE_VALUE(static_memory_u32);
MASS_DEFINE_STRUCT_DESCRIPTOR(32, static_memory_u64, Static_Memory_U64,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("value"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Static_Memory_U64, value),
  },
);
MASS_DEFINE_TYPE_VALUE(static_memory_u64);
MASS_DEFINE_STRUCT_DESCRIPTOR(33, static_memory_heap, Static_Memory_Heap,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("pointer"),
    .descriptor = &descriptor_void_pointer,
    .Base_Relative.offset = offsetof(Static_Memory_Heap, pointer),
  },
);
MASS_DEFINE_TYPE_VALUE(static_memory_heap);
MASS_DEFINE_STRUCT_DESCRIPTOR(34, static_memory, Static_Memory,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_static_memory_tag,
    .Base_Relative.offset = offsetof(Static_Memory, tag),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("U8"),
    .descriptor = &descriptor_static_memory_u8,
    .Base_Relative.offset = offsetof(Static_Memory, U8),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("U16"),
    .descriptor = &descriptor_static_memory_u16,
    .Base_Relative.offset = offsetof(Static_Memory, U16),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("U32"),
    .descriptor = &descriptor_static_memory_u32,
    .Base_Relative.offset = offsetof(Static_Memory, U32),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("U64"),
    .descriptor = &descriptor_static_memory_u64,
    .Base_Relative.offset = offsetof(Static_Memory, U64),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Heap"),
    .descriptor = &descriptor_static_memory_heap,
    .Base_Relative.offset = offsetof(Static_Memory, Heap),
  },
);
MASS_DEFINE_TYPE_VALUE(static_memory);
/*union struct end*/
/*union struct start */
MASS_DEFINE_OPAQUE_C_TYPE(array_storage_ptr, Array_Storage_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_storage, Array_Storage)
MASS_DEFINE_OPAQUE_C_TYPE(storage_tag, Storage_Tag)
static C_Enum_Item storage_tag_items[] = {
{ .name = slice_literal_fields("None"), .value = 0 },
{ .name = slice_literal_fields("Any"), .value = 1 },
{ .name = slice_literal_fields("Eflags"), .value = 2 },
{ .name = slice_literal_fields("Register"), .value = 3 },
{ .name = slice_literal_fields("Xmm"), .value = 4 },
{ .name = slice_literal_fields("Static"), .value = 5 },
{ .name = slice_literal_fields("Memory"), .value = 6 },
};
MASS_DEFINE_STRUCT_DESCRIPTOR(35, storage_eflags, Storage_Eflags,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("compare_type"),
    .descriptor = &descriptor_compare_type,
    .Base_Relative.offset = offsetof(Storage_Eflags, compare_type),
  },
);
MASS_DEFINE_TYPE_VALUE(storage_eflags);
MASS_DEFINE_STRUCT_DESCRIPTOR(36, storage_register, Storage_Register,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("index"),
    .descriptor = &descriptor_register,
    .Base_Relative.offset = offsetof(Storage_Register, index),
  },
);
MASS_DEFINE_TYPE_VALUE(storage_register);
MASS_DEFINE_STRUCT_DESCRIPTOR(37, storage_xmm, Storage_Xmm,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("index"),
    .descriptor = &descriptor_register,
    .Base_Relative.offset = offsetof(Storage_Xmm, index),
  },
);
MASS_DEFINE_TYPE_VALUE(storage_xmm);
MASS_DEFINE_STRUCT_DESCRIPTOR(38, storage_static, Storage_Static,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("memory"),
    .descriptor = &descriptor_static_memory,
    .Base_Relative.offset = offsetof(Storage_Static, memory),
  },
);
MASS_DEFINE_TYPE_VALUE(storage_static);
MASS_DEFINE_STRUCT_DESCRIPTOR(39, storage_memory, Storage_Memory,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("location"),
    .descriptor = &descriptor_memory_location,
    .Base_Relative.offset = offsetof(Storage_Memory, location),
  },
);
MASS_DEFINE_TYPE_VALUE(storage_memory);
MASS_DEFINE_STRUCT_DESCRIPTOR(40, storage, Storage,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_storage_tag,
    .Base_Relative.offset = offsetof(Storage, tag),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("byte_size"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Storage, byte_size),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Eflags"),
    .descriptor = &descriptor_storage_eflags,
    .Base_Relative.offset = offsetof(Storage, Eflags),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Register"),
    .descriptor = &descriptor_storage_register,
    .Base_Relative.offset = offsetof(Storage, Register),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Xmm"),
    .descriptor = &descriptor_storage_xmm,
    .Base_Relative.offset = offsetof(Storage, Xmm),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Static"),
    .descriptor = &descriptor_storage_static,
    .Base_Relative.offset = offsetof(Storage, Static),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Memory"),
    .descriptor = &descriptor_storage_memory,
    .Base_Relative.offset = offsetof(Storage, Memory),
  },
);
MASS_DEFINE_TYPE_VALUE(storage);
/*union struct end*/
MASS_DEFINE_OPAQUE_C_TYPE(array_relocation_ptr, Array_Relocation_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_relocation, Array_Relocation)
MASS_DEFINE_STRUCT_DESCRIPTOR(41, relocation, Relocation,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("patch_at"),
    .descriptor = &descriptor_storage,
    .Base_Relative.offset = offsetof(Relocation, patch_at),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("address_of"),
    .descriptor = &descriptor_storage,
    .Base_Relative.offset = offsetof(Relocation, address_of),
  },
);
MASS_DEFINE_TYPE_VALUE(relocation);
MASS_DEFINE_OPAQUE_C_TYPE(array_compiler_source_location_ptr, Array_Compiler_Source_Location_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_compiler_source_location, Array_Compiler_Source_Location)
MASS_DEFINE_STRUCT_DESCRIPTOR(42, compiler_source_location, Compiler_Source_Location,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("filename"),
    .descriptor = &descriptor_char_pointer,
    .Base_Relative.offset = offsetof(Compiler_Source_Location, filename),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("function_name"),
    .descriptor = &descriptor_char_pointer,
    .Base_Relative.offset = offsetof(Compiler_Source_Location, function_name),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("line_number"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Compiler_Source_Location, line_number),
  },
);
MASS_DEFINE_TYPE_VALUE(compiler_source_location);
/*union struct start */
MASS_DEFINE_OPAQUE_C_TYPE(array_instruction_ptr, Array_Instruction_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_instruction, Array_Instruction)
MASS_DEFINE_OPAQUE_C_TYPE(instruction_tag, Instruction_Tag)
static C_Enum_Item instruction_tag_items[] = {
{ .name = slice_literal_fields("Assembly"), .value = 0 },
{ .name = slice_literal_fields("Label"), .value = 1 },
{ .name = slice_literal_fields("Bytes"), .value = 2 },
{ .name = slice_literal_fields("Label_Patch"), .value = 3 },
};
MASS_DEFINE_STRUCT_DESCRIPTOR(43, instruction_assembly, Instruction_Assembly,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("mnemonic"),
    .descriptor = &descriptor_x64_mnemonic_pointer,
    .Base_Relative.offset = offsetof(Instruction_Assembly, mnemonic),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("operands"),
    .descriptor = &descriptor_storage_3,
    .Base_Relative.offset = offsetof(Instruction_Assembly, operands),
  },
);
MASS_DEFINE_TYPE_VALUE(instruction_assembly);
MASS_DEFINE_STRUCT_DESCRIPTOR(44, instruction_label, Instruction_Label,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("index"),
    .descriptor = &descriptor_label_index,
    .Base_Relative.offset = offsetof(Instruction_Label, index),
  },
);
MASS_DEFINE_TYPE_VALUE(instruction_label);
MASS_DEFINE_STRUCT_DESCRIPTOR(45, instruction_bytes, Instruction_Bytes,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("memory"),
    .descriptor = &descriptor_u8_15,
    .Base_Relative.offset = offsetof(Instruction_Bytes, memory),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("length"),
    .descriptor = &descriptor_u8,
    .Base_Relative.offset = offsetof(Instruction_Bytes, length),
  },
);
MASS_DEFINE_TYPE_VALUE(instruction_bytes);
MASS_DEFINE_STRUCT_DESCRIPTOR(46, instruction_label_patch, Instruction_Label_Patch,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("offset"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Instruction_Label_Patch, offset),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("label_index"),
    .descriptor = &descriptor_label_index,
    .Base_Relative.offset = offsetof(Instruction_Label_Patch, label_index),
  },
);
MASS_DEFINE_TYPE_VALUE(instruction_label_patch);
MASS_DEFINE_STRUCT_DESCRIPTOR(47, instruction, Instruction,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_instruction_tag,
    .Base_Relative.offset = offsetof(Instruction, tag),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("compiler_source_location"),
    .descriptor = &descriptor_compiler_source_location,
    .Base_Relative.offset = offsetof(Instruction, compiler_source_location),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("source_range"),
    .descriptor = &descriptor_source_range,
    .Base_Relative.offset = offsetof(Instruction, source_range),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("scope"),
    .descriptor = &descriptor_scope_pointer,
    .Base_Relative.offset = offsetof(Instruction, scope),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("encoded_byte_size"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Instruction, encoded_byte_size),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Assembly"),
    .descriptor = &descriptor_instruction_assembly,
    .Base_Relative.offset = offsetof(Instruction, Assembly),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Label"),
    .descriptor = &descriptor_instruction_label,
    .Base_Relative.offset = offsetof(Instruction, Label),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Bytes"),
    .descriptor = &descriptor_instruction_bytes,
    .Base_Relative.offset = offsetof(Instruction, Bytes),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Label_Patch"),
    .descriptor = &descriptor_instruction_label_patch,
    .Base_Relative.offset = offsetof(Instruction, Label_Patch),
  },
);
MASS_DEFINE_TYPE_VALUE(instruction);
/*union struct end*/
MASS_DEFINE_OPAQUE_C_TYPE(array_code_block_ptr, Array_Code_Block_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_code_block, Array_Code_Block)
MASS_DEFINE_STRUCT_DESCRIPTOR(48, code_block, Code_Block,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("start_label"),
    .descriptor = &descriptor_label_index,
    .Base_Relative.offset = offsetof(Code_Block, start_label),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("end_label"),
    .descriptor = &descriptor_label_index,
    .Base_Relative.offset = offsetof(Code_Block, end_label),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("instructions"),
    .descriptor = &descriptor_array_instruction,
    .Base_Relative.offset = offsetof(Code_Block, instructions),
  },
);
MASS_DEFINE_TYPE_VALUE(code_block);
MASS_DEFINE_OPAQUE_C_TYPE(array_function_builder_ptr, Array_Function_Builder_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_function_builder, Array_Function_Builder)
MASS_DEFINE_STRUCT_DESCRIPTOR(49, function_builder, Function_Builder,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("stack_reserve"),
    .descriptor = &descriptor_s32,
    .Base_Relative.offset = offsetof(Function_Builder, stack_reserve),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("max_call_parameters_stack_size"),
    .descriptor = &descriptor_u32,
    .Base_Relative.offset = offsetof(Function_Builder, max_call_parameters_stack_size),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("return_value"),
    .descriptor = &descriptor_value_pointer,
    .Base_Relative.offset = offsetof(Function_Builder, return_value),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("code_block"),
    .descriptor = &descriptor_code_block,
    .Base_Relative.offset = offsetof(Function_Builder, code_block),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("used_register_bitset"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Function_Builder, used_register_bitset),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("register_volatile_bitset"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Function_Builder, register_volatile_bitset),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("register_occupied_bitset"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Function_Builder, register_occupied_bitset),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("register_occupied_storage"),
    .descriptor = &descriptor_storage_pointer_32,
    .Base_Relative.offset = offsetof(Function_Builder, register_occupied_storage),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("source"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Function_Builder, source),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("function"),
    .descriptor = &descriptor_function_info_pointer,
    .Base_Relative.offset = offsetof(Function_Builder, function),
  },
);
MASS_DEFINE_TYPE_VALUE(function_builder);
MASS_DEFINE_OPAQUE_C_TYPE(function_argument_mode, Function_Argument_Mode)
static C_Enum_Item function_argument_mode_items[] = {
{ .name = slice_literal_fields("Call"), .value = 0 },
{ .name = slice_literal_fields("Body"), .value = 1 },
};
MASS_DEFINE_OPAQUE_C_TYPE(operator_fixity, Operator_Fixity)
static C_Enum_Item operator_fixity_items[] = {
{ .name = slice_literal_fields("Infix"), .value = 1 },
{ .name = slice_literal_fields("Prefix"), .value = 2 },
{ .name = slice_literal_fields("Postfix"), .value = 4 },
};
MASS_DEFINE_OPAQUE_C_TYPE(operator_associativity, Operator_Associativity)
static C_Enum_Item operator_associativity_items[] = {
{ .name = slice_literal_fields("Left"), .value = 0 },
{ .name = slice_literal_fields("Right"), .value = 1 },
};
MASS_DEFINE_OPAQUE_C_TYPE(array_function_layout_ptr, Array_Function_Layout_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_function_layout, Array_Function_Layout)
MASS_DEFINE_STRUCT_DESCRIPTOR(50, function_layout, Function_Layout,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("stack_reserve"),
    .descriptor = &descriptor_s32,
    .Base_Relative.offset = offsetof(Function_Layout, stack_reserve),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("stack_allocation_offset_in_prolog"),
    .descriptor = &descriptor_u8,
    .Base_Relative.offset = offsetof(Function_Layout, stack_allocation_offset_in_prolog),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("size_of_prolog"),
    .descriptor = &descriptor_u8,
    .Base_Relative.offset = offsetof(Function_Layout, size_of_prolog),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("_padding"),
    .descriptor = &descriptor_u16,
    .Base_Relative.offset = offsetof(Function_Layout, _padding),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("begin_rva"),
    .descriptor = &descriptor_u32,
    .Base_Relative.offset = offsetof(Function_Layout, begin_rva),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("end_rva"),
    .descriptor = &descriptor_u32,
    .Base_Relative.offset = offsetof(Function_Layout, end_rva),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("volatile_register_push_offsets"),
    .descriptor = &descriptor_u8_16,
    .Base_Relative.offset = offsetof(Function_Layout, volatile_register_push_offsets),
  },
);
MASS_DEFINE_TYPE_VALUE(function_layout);
MASS_DEFINE_OPAQUE_C_TYPE(execution_context_flags, Execution_Context_Flags)
static C_Enum_Item execution_context_flags_items[] = {
{ .name = slice_literal_fields("None"), .value = 0 },
{ .name = slice_literal_fields("Global"), .value = 1 },
};
MASS_DEFINE_OPAQUE_C_TYPE(array_execution_context_ptr, Array_Execution_Context_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_execution_context, Array_Execution_Context)
MASS_DEFINE_STRUCT_DESCRIPTOR(51, execution_context, Execution_Context,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("allocator"),
    .descriptor = &descriptor_allocator_pointer,
    .Base_Relative.offset = offsetof(Execution_Context, allocator),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("temp_allocator"),
    .descriptor = &descriptor_allocator_pointer,
    .Base_Relative.offset = offsetof(Execution_Context, temp_allocator),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("current_compile_time_function_call_target"),
    .descriptor = &descriptor_value_pointer,
    .Base_Relative.offset = offsetof(Execution_Context, current_compile_time_function_call_target),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("flags"),
    .descriptor = &descriptor_execution_context_flags,
    .Base_Relative.offset = offsetof(Execution_Context, flags),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("_flags_padding"),
    .descriptor = &descriptor_s32,
    .Base_Relative.offset = offsetof(Execution_Context, _flags_padding),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("compilation"),
    .descriptor = &descriptor_compilation_pointer,
    .Base_Relative.offset = offsetof(Execution_Context, compilation),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("epoch"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Execution_Context, epoch),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("program"),
    .descriptor = &descriptor_program_pointer,
    .Base_Relative.offset = offsetof(Execution_Context, program),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("scope"),
    .descriptor = &descriptor_scope_pointer,
    .Base_Relative.offset = offsetof(Execution_Context, scope),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("module"),
    .descriptor = &descriptor_module_pointer,
    .Base_Relative.offset = offsetof(Execution_Context, module),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("result"),
    .descriptor = &descriptor_mass_result_pointer,
    .Base_Relative.offset = offsetof(Execution_Context, result),
  },
);
MASS_DEFINE_TYPE_VALUE(execution_context);
MASS_DEFINE_OPAQUE_C_TYPE(array_user_defined_operator_ptr, Array_User_Defined_Operator_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_user_defined_operator, Array_User_Defined_Operator)
MASS_DEFINE_STRUCT_DESCRIPTOR(52, user_defined_operator, User_Defined_Operator,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("fixity"),
    .descriptor = &descriptor_operator_fixity,
    .Base_Relative.offset = offsetof(User_Defined_Operator, fixity),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("argument_count"),
    .descriptor = &descriptor_u32,
    .Base_Relative.offset = offsetof(User_Defined_Operator, argument_count),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("argument_names"),
    .descriptor = &descriptor_slice_2,
    .Base_Relative.offset = offsetof(User_Defined_Operator, argument_names),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("body"),
    .descriptor = &descriptor_value_pointer,
    .Base_Relative.offset = offsetof(User_Defined_Operator, body),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("scope"),
    .descriptor = &descriptor_scope_pointer,
    .Base_Relative.offset = offsetof(User_Defined_Operator, scope),
  },
);
MASS_DEFINE_TYPE_VALUE(user_defined_operator);
MASS_DEFINE_OPAQUE_C_TYPE(array_operator_ptr, Array_Operator_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_operator, Array_Operator)
MASS_DEFINE_STRUCT_DESCRIPTOR(53, operator, Operator,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("fixity"),
    .descriptor = &descriptor_operator_fixity,
    .Base_Relative.offset = offsetof(Operator, fixity),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("associativity"),
    .descriptor = &descriptor_operator_associativity,
    .Base_Relative.offset = offsetof(Operator, associativity),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("precedence"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Operator, precedence),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("argument_count"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Operator, argument_count),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("handler"),
    .descriptor = &descriptor_mass_handle_operator_proc,
    .Base_Relative.offset = offsetof(Operator, handler),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("handler_payload"),
    .descriptor = &descriptor_void_pointer,
    .Base_Relative.offset = offsetof(Operator, handler_payload),
  },
);
MASS_DEFINE_TYPE_VALUE(operator);
/*union struct start */
MASS_DEFINE_OPAQUE_C_TYPE(array_macro_pattern_ptr, Array_Macro_Pattern_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_macro_pattern, Array_Macro_Pattern)
MASS_DEFINE_OPAQUE_C_TYPE(macro_pattern_tag, Macro_Pattern_Tag)
static C_Enum_Item macro_pattern_tag_items[] = {
{ .name = slice_literal_fields("Any_Token_Sequence"), .value = 0 },
{ .name = slice_literal_fields("Single_Token"), .value = 1 },
};
MASS_DEFINE_STRUCT_DESCRIPTOR(54, macro_pattern_single_token, Macro_Pattern_Single_Token,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("token_pattern"),
    .descriptor = &descriptor_token_pattern,
    .Base_Relative.offset = offsetof(Macro_Pattern_Single_Token, token_pattern),
  },
);
MASS_DEFINE_TYPE_VALUE(macro_pattern_single_token);
MASS_DEFINE_STRUCT_DESCRIPTOR(55, macro_pattern, Macro_Pattern,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_macro_pattern_tag,
    .Base_Relative.offset = offsetof(Macro_Pattern, tag),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("capture_name"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Macro_Pattern, capture_name),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Single_Token"),
    .descriptor = &descriptor_macro_pattern_single_token,
    .Base_Relative.offset = offsetof(Macro_Pattern, Single_Token),
  },
);
MASS_DEFINE_TYPE_VALUE(macro_pattern);
/*union struct end*/
MASS_DEFINE_OPAQUE_C_TYPE(array_macro_ptr, Array_Macro_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_macro, Array_Macro)
MASS_DEFINE_STRUCT_DESCRIPTOR(56, macro, Macro,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("pattern"),
    .descriptor = &descriptor_array_macro_pattern,
    .Base_Relative.offset = offsetof(Macro, pattern),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("replacement"),
    .descriptor = &descriptor_value_view,
    .Base_Relative.offset = offsetof(Macro, replacement),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("scope"),
    .descriptor = &descriptor_scope_pointer,
    .Base_Relative.offset = offsetof(Macro, scope),
  },
);
MASS_DEFINE_TYPE_VALUE(macro);
MASS_DEFINE_OPAQUE_C_TYPE(array_token_statement_matcher_ptr, Array_Token_Statement_Matcher_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_token_statement_matcher, Array_Token_Statement_Matcher)
MASS_DEFINE_STRUCT_DESCRIPTOR(57, token_statement_matcher, Token_Statement_Matcher,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("proc"),
    .descriptor = &descriptor_token_statement_matcher_proc,
    .Base_Relative.offset = offsetof(Token_Statement_Matcher, proc),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("payload"),
    .descriptor = &descriptor_void_pointer,
    .Base_Relative.offset = offsetof(Token_Statement_Matcher, payload),
  },
);
MASS_DEFINE_TYPE_VALUE(token_statement_matcher);
/*union struct start */
MASS_DEFINE_OPAQUE_C_TYPE(array_scope_entry_ptr, Array_Scope_Entry_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_scope_entry, Array_Scope_Entry)
MASS_DEFINE_OPAQUE_C_TYPE(scope_entry_tag, Scope_Entry_Tag)
static C_Enum_Item scope_entry_tag_items[] = {
{ .name = slice_literal_fields("Value"), .value = 0 },
{ .name = slice_literal_fields("Operator"), .value = 1 },
};
MASS_DEFINE_STRUCT_DESCRIPTOR(58, scope_entry_value, Scope_Entry_Value,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("value"),
    .descriptor = &descriptor_value_pointer,
    .Base_Relative.offset = offsetof(Scope_Entry_Value, value),
  },
);
MASS_DEFINE_TYPE_VALUE(scope_entry_value);
MASS_DEFINE_STRUCT_DESCRIPTOR(59, scope_entry_operator, Scope_Entry_Operator,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("maybe_prefix"),
    .descriptor = &descriptor_operator_pointer,
    .Base_Relative.offset = offsetof(Scope_Entry_Operator, maybe_prefix),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("maybe_infix_or_postfix"),
    .descriptor = &descriptor_operator_pointer,
    .Base_Relative.offset = offsetof(Scope_Entry_Operator, maybe_infix_or_postfix),
  },
);
MASS_DEFINE_TYPE_VALUE(scope_entry_operator);
MASS_DEFINE_STRUCT_DESCRIPTOR(60, scope_entry, Scope_Entry,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_scope_entry_tag,
    .Base_Relative.offset = offsetof(Scope_Entry, tag),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("epoch"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Scope_Entry, epoch),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("source_range"),
    .descriptor = &descriptor_source_range,
    .Base_Relative.offset = offsetof(Scope_Entry, source_range),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Value"),
    .descriptor = &descriptor_scope_entry_value,
    .Base_Relative.offset = offsetof(Scope_Entry, Value),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Operator"),
    .descriptor = &descriptor_scope_entry_operator,
    .Base_Relative.offset = offsetof(Scope_Entry, Operator),
  },
);
MASS_DEFINE_TYPE_VALUE(scope_entry);
/*union struct end*/
MASS_DEFINE_OPAQUE_C_TYPE(array_scope_ptr, Array_Scope_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_scope, Array_Scope)
MASS_DEFINE_STRUCT_DESCRIPTOR(61, scope, Scope,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("allocator"),
    .descriptor = &descriptor_allocator_pointer,
    .Base_Relative.offset = offsetof(Scope, allocator),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("id"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Scope, id),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("parent"),
    .descriptor = &descriptor_scope_pointer,
    .Base_Relative.offset = offsetof(Scope, parent),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("map"),
    .descriptor = &descriptor_scope_map_pointer,
    .Base_Relative.offset = offsetof(Scope, map),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("macros"),
    .descriptor = &descriptor_array_macro_ptr,
    .Base_Relative.offset = offsetof(Scope, macros),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("statement_matchers"),
    .descriptor = &descriptor_array_token_statement_matcher,
    .Base_Relative.offset = offsetof(Scope, statement_matchers),
  },
);
MASS_DEFINE_TYPE_VALUE(scope);
MASS_DEFINE_OPAQUE_C_TYPE(array_value_ptr, Array_Value_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_value, Array_Value)
MASS_DEFINE_STRUCT_DESCRIPTOR(62, value, Value,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("descriptor"),
    .descriptor = &descriptor_descriptor_pointer,
    .Base_Relative.offset = offsetof(Value, descriptor),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("storage"),
    .descriptor = &descriptor_storage,
    .Base_Relative.offset = offsetof(Value, storage),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("next_overload"),
    .descriptor = &descriptor_value_pointer,
    .Base_Relative.offset = offsetof(Value, next_overload),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("is_temporary"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Value, is_temporary),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("source_range"),
    .descriptor = &descriptor_source_range,
    .Base_Relative.offset = offsetof(Value, source_range),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("compiler_source_location"),
    .descriptor = &descriptor_compiler_source_location,
    .Base_Relative.offset = offsetof(Value, compiler_source_location),
  },
);
MASS_DEFINE_TYPE_VALUE(value);
MASS_DEFINE_OPAQUE_C_TYPE(expected_result_storage, Expected_Result_Storage)
static C_Enum_Item expected_result_storage_items[] = {
{ .name = slice_literal_fields("None"), .value = 0 },
{ .name = slice_literal_fields("Static"), .value = 1 },
{ .name = slice_literal_fields("Memory"), .value = 2 },
{ .name = slice_literal_fields("Register"), .value = 4 },
{ .name = slice_literal_fields("Xmm"), .value = 8 },
{ .name = slice_literal_fields("Eflags"), .value = 16 },
};
/*union struct start */
MASS_DEFINE_OPAQUE_C_TYPE(array_expected_result_ptr, Array_Expected_Result_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_expected_result, Array_Expected_Result)
MASS_DEFINE_OPAQUE_C_TYPE(expected_result_tag, Expected_Result_Tag)
static C_Enum_Item expected_result_tag_items[] = {
{ .name = slice_literal_fields("Exact"), .value = 0 },
{ .name = slice_literal_fields("Flexible"), .value = 1 },
};
MASS_DEFINE_STRUCT_DESCRIPTOR(63, expected_result_exact, Expected_Result_Exact,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("value"),
    .descriptor = &descriptor_value_pointer,
    .Base_Relative.offset = offsetof(Expected_Result_Exact, value),
  },
);
MASS_DEFINE_TYPE_VALUE(expected_result_exact);
MASS_DEFINE_STRUCT_DESCRIPTOR(64, expected_result_flexible, Expected_Result_Flexible,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("descriptor"),
    .descriptor = &descriptor_descriptor_pointer,
    .Base_Relative.offset = offsetof(Expected_Result_Flexible, descriptor),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("storage"),
    .descriptor = &descriptor_expected_result_storage,
    .Base_Relative.offset = offsetof(Expected_Result_Flexible, storage),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("_storage_padding"),
    .descriptor = &descriptor_s32,
    .Base_Relative.offset = offsetof(Expected_Result_Flexible, _storage_padding),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("register_bit_set"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Expected_Result_Flexible, register_bit_set),
  },
);
MASS_DEFINE_TYPE_VALUE(expected_result_flexible);
MASS_DEFINE_STRUCT_DESCRIPTOR(65, expected_result, Expected_Result,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_expected_result_tag,
    .Base_Relative.offset = offsetof(Expected_Result, tag),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Exact"),
    .descriptor = &descriptor_expected_result_exact,
    .Base_Relative.offset = offsetof(Expected_Result, Exact),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Flexible"),
    .descriptor = &descriptor_expected_result_flexible,
    .Base_Relative.offset = offsetof(Expected_Result, Flexible),
  },
);
MASS_DEFINE_TYPE_VALUE(expected_result);
/*union struct end*/
MASS_DEFINE_OPAQUE_C_TYPE(array_lazy_value_ptr, Array_Lazy_Value_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_lazy_value, Array_Lazy_Value)
MASS_DEFINE_STRUCT_DESCRIPTOR(66, lazy_value, Lazy_Value,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("context"),
    .descriptor = &descriptor_execution_context,
    .Base_Relative.offset = offsetof(Lazy_Value, context),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("descriptor"),
    .descriptor = &descriptor_descriptor_pointer,
    .Base_Relative.offset = offsetof(Lazy_Value, descriptor),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("proc"),
    .descriptor = &descriptor_lazy_value_proc,
    .Base_Relative.offset = offsetof(Lazy_Value, proc),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("payload"),
    .descriptor = &descriptor_void_pointer,
    .Base_Relative.offset = offsetof(Lazy_Value, payload),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("epoch"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Lazy_Value, epoch),
  },
);
MASS_DEFINE_TYPE_VALUE(lazy_value);
MASS_DEFINE_OPAQUE_C_TYPE(array_lazy_static_value_ptr, Array_Lazy_Static_Value_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_lazy_static_value, Array_Lazy_Static_Value)
MASS_DEFINE_STRUCT_DESCRIPTOR(67, lazy_static_value, Lazy_Static_Value,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("context"),
    .descriptor = &descriptor_execution_context,
    .Base_Relative.offset = offsetof(Lazy_Static_Value, context),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("expression"),
    .descriptor = &descriptor_value_view,
    .Base_Relative.offset = offsetof(Lazy_Static_Value, expression),
  },
);
MASS_DEFINE_TYPE_VALUE(lazy_static_value);
MASS_DEFINE_OPAQUE_C_TYPE(memory_layout_item_flags, Memory_Layout_Item_Flags)
static C_Enum_Item memory_layout_item_flags_items[] = {
{ .name = slice_literal_fields("None"), .value = 0 },
{ .name = slice_literal_fields("Uninitialized"), .value = 1 },
};
/*union struct start */
MASS_DEFINE_OPAQUE_C_TYPE(array_memory_layout_item_ptr, Array_Memory_Layout_Item_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_memory_layout_item, Array_Memory_Layout_Item)
MASS_DEFINE_OPAQUE_C_TYPE(memory_layout_item_tag, Memory_Layout_Item_Tag)
static C_Enum_Item memory_layout_item_tag_items[] = {
{ .name = slice_literal_fields("Absolute"), .value = 0 },
{ .name = slice_literal_fields("Base_Relative"), .value = 1 },
};
MASS_DEFINE_STRUCT_DESCRIPTOR(68, memory_layout_item_absolute, Memory_Layout_Item_Absolute,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("storage"),
    .descriptor = &descriptor_storage,
    .Base_Relative.offset = offsetof(Memory_Layout_Item_Absolute, storage),
  },
);
MASS_DEFINE_TYPE_VALUE(memory_layout_item_absolute);
MASS_DEFINE_STRUCT_DESCRIPTOR(69, memory_layout_item_base_relative, Memory_Layout_Item_Base_Relative,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("offset"),
    .descriptor = &descriptor_s64,
    .Base_Relative.offset = offsetof(Memory_Layout_Item_Base_Relative, offset),
  },
);
MASS_DEFINE_TYPE_VALUE(memory_layout_item_base_relative);
MASS_DEFINE_STRUCT_DESCRIPTOR(70, memory_layout_item, Memory_Layout_Item,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_memory_layout_item_tag,
    .Base_Relative.offset = offsetof(Memory_Layout_Item, tag),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("flags"),
    .descriptor = &descriptor_memory_layout_item_flags,
    .Base_Relative.offset = offsetof(Memory_Layout_Item, flags),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("_flags_padding"),
    .descriptor = &descriptor_u32,
    .Base_Relative.offset = offsetof(Memory_Layout_Item, _flags_padding),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Memory_Layout_Item, name),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("descriptor"),
    .descriptor = &descriptor_descriptor_pointer,
    .Base_Relative.offset = offsetof(Memory_Layout_Item, descriptor),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("source_range"),
    .descriptor = &descriptor_source_range,
    .Base_Relative.offset = offsetof(Memory_Layout_Item, source_range),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Absolute"),
    .descriptor = &descriptor_memory_layout_item_absolute,
    .Base_Relative.offset = offsetof(Memory_Layout_Item, Absolute),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Base_Relative"),
    .descriptor = &descriptor_memory_layout_item_base_relative,
    .Base_Relative.offset = offsetof(Memory_Layout_Item, Base_Relative),
  },
);
MASS_DEFINE_TYPE_VALUE(memory_layout_item);
/*union struct end*/
MASS_DEFINE_OPAQUE_C_TYPE(array_memory_layout_ptr, Array_Memory_Layout_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_memory_layout, Array_Memory_Layout)
MASS_DEFINE_STRUCT_DESCRIPTOR(71, memory_layout, Memory_Layout,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("items"),
    .descriptor = &descriptor_array_memory_layout_item,
    .Base_Relative.offset = offsetof(Memory_Layout, items),
  },
);
MASS_DEFINE_TYPE_VALUE(memory_layout);
MASS_DEFINE_OPAQUE_C_TYPE(array_function_return_ptr, Array_Function_Return_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_function_return, Array_Function_Return)
MASS_DEFINE_STRUCT_DESCRIPTOR(72, function_return, Function_Return,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Function_Return, name),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("descriptor"),
    .descriptor = &descriptor_descriptor_pointer,
    .Base_Relative.offset = offsetof(Function_Return, descriptor),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("source_range"),
    .descriptor = &descriptor_source_range,
    .Base_Relative.offset = offsetof(Function_Return, source_range),
  },
);
MASS_DEFINE_TYPE_VALUE(function_return);
MASS_DEFINE_OPAQUE_C_TYPE(array_function_argument_ptr, Array_Function_Argument_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_function_argument, Array_Function_Argument)
MASS_DEFINE_STRUCT_DESCRIPTOR(73, function_argument, Function_Argument,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Function_Argument, name),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("descriptor"),
    .descriptor = &descriptor_descriptor_pointer,
    .Base_Relative.offset = offsetof(Function_Argument, descriptor),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("source_range"),
    .descriptor = &descriptor_source_range,
    .Base_Relative.offset = offsetof(Function_Argument, source_range),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("maybe_default_expression"),
    .descriptor = &descriptor_value_view,
    .Base_Relative.offset = offsetof(Function_Argument, maybe_default_expression),
  },
);
MASS_DEFINE_TYPE_VALUE(function_argument);
MASS_DEFINE_OPAQUE_C_TYPE(descriptor_function_flags, Descriptor_Function_Flags)
static C_Enum_Item descriptor_function_flags_items[] = {
{ .name = slice_literal_fields("None"), .value = 0 },
{ .name = slice_literal_fields("Macro"), .value = 1 },
{ .name = slice_literal_fields("Compile_Time"), .value = 4 },
};
MASS_DEFINE_OPAQUE_C_TYPE(array_function_info_ptr, Array_Function_Info_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_function_info, Array_Function_Info)
MASS_DEFINE_STRUCT_DESCRIPTOR(74, function_info, Function_Info,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("flags"),
    .descriptor = &descriptor_descriptor_function_flags,
    .Base_Relative.offset = offsetof(Function_Info, flags),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("_flags_padding"),
    .descriptor = &descriptor_u32,
    .Base_Relative.offset = offsetof(Function_Info, _flags_padding),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("arguments"),
    .descriptor = &descriptor_array_function_argument,
    .Base_Relative.offset = offsetof(Function_Info, arguments),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("scope"),
    .descriptor = &descriptor_scope_pointer,
    .Base_Relative.offset = offsetof(Function_Info, scope),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("returns"),
    .descriptor = &descriptor_function_return,
    .Base_Relative.offset = offsetof(Function_Info, returns),
  },
);
MASS_DEFINE_TYPE_VALUE(function_info);
MASS_DEFINE_OPAQUE_C_TYPE(array_function_literal_ptr, Array_Function_Literal_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_function_literal, Array_Function_Literal)
MASS_DEFINE_STRUCT_DESCRIPTOR(75, function_literal, Function_Literal,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("info"),
    .descriptor = &descriptor_function_info_pointer,
    .Base_Relative.offset = offsetof(Function_Literal, info),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("body"),
    .descriptor = &descriptor_value_pointer,
    .Base_Relative.offset = offsetof(Function_Literal, body),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("runtime_instance"),
    .descriptor = &descriptor_value_pointer,
    .Base_Relative.offset = offsetof(Function_Literal, runtime_instance),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("compile_time_instance"),
    .descriptor = &descriptor_value_pointer,
    .Base_Relative.offset = offsetof(Function_Literal, compile_time_instance),
  },
);
MASS_DEFINE_TYPE_VALUE(function_literal);
/*union struct start */
MASS_DEFINE_OPAQUE_C_TYPE(array_descriptor_ptr, Array_Descriptor_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_descriptor, Array_Descriptor)
MASS_DEFINE_OPAQUE_C_TYPE(descriptor_tag, Descriptor_Tag)
static C_Enum_Item descriptor_tag_items[] = {
{ .name = slice_literal_fields("Opaque"), .value = 0 },
{ .name = slice_literal_fields("Function_Instance"), .value = 1 },
{ .name = slice_literal_fields("Fixed_Size_Array"), .value = 2 },
{ .name = slice_literal_fields("Struct"), .value = 3 },
{ .name = slice_literal_fields("Pointer_To"), .value = 4 },
};
MASS_DEFINE_STRUCT_DESCRIPTOR(76, descriptor_function_instance, Descriptor_Function_Instance,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("info"),
    .descriptor = &descriptor_function_info_pointer,
    .Base_Relative.offset = offsetof(Descriptor_Function_Instance, info),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("calling_convention"),
    .descriptor = &descriptor_calling_convention_pointer,
    .Base_Relative.offset = offsetof(Descriptor_Function_Instance, calling_convention),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("arguments_layout"),
    .descriptor = &descriptor_memory_layout,
    .Base_Relative.offset = offsetof(Descriptor_Function_Instance, arguments_layout),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("return_value"),
    .descriptor = &descriptor_value_pointer,
    .Base_Relative.offset = offsetof(Descriptor_Function_Instance, return_value),
  },
);
MASS_DEFINE_TYPE_VALUE(descriptor_function_instance);
MASS_DEFINE_STRUCT_DESCRIPTOR(77, descriptor_fixed_size_array, Descriptor_Fixed_Size_Array,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("item"),
    .descriptor = &descriptor_descriptor_pointer,
    .Base_Relative.offset = offsetof(Descriptor_Fixed_Size_Array, item),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("length"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Descriptor_Fixed_Size_Array, length),
  },
);
MASS_DEFINE_TYPE_VALUE(descriptor_fixed_size_array);
MASS_DEFINE_STRUCT_DESCRIPTOR(78, descriptor_struct, Descriptor_Struct,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("id"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Descriptor_Struct, id),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("memory_layout"),
    .descriptor = &descriptor_memory_layout,
    .Base_Relative.offset = offsetof(Descriptor_Struct, memory_layout),
  },
);
MASS_DEFINE_TYPE_VALUE(descriptor_struct);
MASS_DEFINE_STRUCT_DESCRIPTOR(79, descriptor_pointer_to, Descriptor_Pointer_To,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("descriptor"),
    .descriptor = &descriptor_descriptor_pointer,
    .Base_Relative.offset = offsetof(Descriptor_Pointer_To, descriptor),
  },
);
MASS_DEFINE_TYPE_VALUE(descriptor_pointer_to);
MASS_DEFINE_STRUCT_DESCRIPTOR(80, descriptor, Descriptor,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_descriptor_tag,
    .Base_Relative.offset = offsetof(Descriptor, tag),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Descriptor, name),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("bit_size"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Descriptor, bit_size),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("bit_alignment"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Descriptor, bit_alignment),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Function_Instance"),
    .descriptor = &descriptor_descriptor_function_instance,
    .Base_Relative.offset = offsetof(Descriptor, Function_Instance),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Fixed_Size_Array"),
    .descriptor = &descriptor_descriptor_fixed_size_array,
    .Base_Relative.offset = offsetof(Descriptor, Fixed_Size_Array),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Struct"),
    .descriptor = &descriptor_descriptor_struct,
    .Base_Relative.offset = offsetof(Descriptor, Struct),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Pointer_To"),
    .descriptor = &descriptor_descriptor_pointer_to,
    .Base_Relative.offset = offsetof(Descriptor, Pointer_To),
  },
);
MASS_DEFINE_TYPE_VALUE(descriptor);
/*union struct end*/
/*union struct start */
MASS_DEFINE_OPAQUE_C_TYPE(array_mass_error_ptr, Array_Mass_Error_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_mass_error, Array_Mass_Error)
MASS_DEFINE_OPAQUE_C_TYPE(mass_error_tag, Mass_Error_Tag)
static C_Enum_Item mass_error_tag_items[] = {
{ .name = slice_literal_fields("Unimplemented"), .value = 0 },
{ .name = slice_literal_fields("User_Defined"), .value = 1 },
{ .name = slice_literal_fields("Parse"), .value = 2 },
{ .name = slice_literal_fields("Non_Trailing_Default_Argument"), .value = 3 },
{ .name = slice_literal_fields("Expected_Static"), .value = 4 },
{ .name = slice_literal_fields("Integer_Range"), .value = 5 },
{ .name = slice_literal_fields("File_Open"), .value = 6 },
{ .name = slice_literal_fields("Unexpected_Token"), .value = 7 },
{ .name = slice_literal_fields("Operator_Infix_Suffix_Conflict"), .value = 8 },
{ .name = slice_literal_fields("Operator_Prefix_Conflict"), .value = 9 },
{ .name = slice_literal_fields("Undefined_Variable"), .value = 10 },
{ .name = slice_literal_fields("Redifinition"), .value = 11 },
{ .name = slice_literal_fields("Unknown_Field"), .value = 12 },
{ .name = slice_literal_fields("Invalid_Identifier"), .value = 13 },
{ .name = slice_literal_fields("Type_Mismatch"), .value = 14 },
{ .name = slice_literal_fields("Epoch_Mismatch"), .value = 15 },
{ .name = slice_literal_fields("No_Matching_Overload"), .value = 16 },
{ .name = slice_literal_fields("Undecidable_Overload"), .value = 17 },
};
MASS_DEFINE_STRUCT_DESCRIPTOR(81, mass_error_user_defined, Mass_Error_User_Defined,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Mass_Error_User_Defined, name),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_user_defined);
MASS_DEFINE_STRUCT_DESCRIPTOR(82, mass_error_integer_range, Mass_Error_Integer_Range,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("descriptor"),
    .descriptor = &descriptor_descriptor_pointer,
    .Base_Relative.offset = offsetof(Mass_Error_Integer_Range, descriptor),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_integer_range);
MASS_DEFINE_STRUCT_DESCRIPTOR(83, mass_error_file_open, Mass_Error_File_Open,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("path"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Mass_Error_File_Open, path),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_file_open);
MASS_DEFINE_STRUCT_DESCRIPTOR(84, mass_error_unexpected_token, Mass_Error_Unexpected_Token,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("expected"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Mass_Error_Unexpected_Token, expected),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_unexpected_token);
MASS_DEFINE_STRUCT_DESCRIPTOR(85, mass_error_operator_infix_suffix_conflict, Mass_Error_Operator_Infix_Suffix_Conflict,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("symbol"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Mass_Error_Operator_Infix_Suffix_Conflict, symbol),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_operator_infix_suffix_conflict);
MASS_DEFINE_STRUCT_DESCRIPTOR(86, mass_error_operator_prefix_conflict, Mass_Error_Operator_Prefix_Conflict,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("symbol"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Mass_Error_Operator_Prefix_Conflict, symbol),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_operator_prefix_conflict);
MASS_DEFINE_STRUCT_DESCRIPTOR(87, mass_error_undefined_variable, Mass_Error_Undefined_Variable,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Mass_Error_Undefined_Variable, name),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("is_operator"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Mass_Error_Undefined_Variable, is_operator),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_undefined_variable);
MASS_DEFINE_STRUCT_DESCRIPTOR(88, mass_error_redifinition, Mass_Error_Redifinition,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Mass_Error_Redifinition, name),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("previous_source_range"),
    .descriptor = &descriptor_source_range,
    .Base_Relative.offset = offsetof(Mass_Error_Redifinition, previous_source_range),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_redifinition);
MASS_DEFINE_STRUCT_DESCRIPTOR(89, mass_error_unknown_field, Mass_Error_Unknown_Field,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("type"),
    .descriptor = &descriptor_descriptor_pointer,
    .Base_Relative.offset = offsetof(Mass_Error_Unknown_Field, type),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Mass_Error_Unknown_Field, name),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_unknown_field);
MASS_DEFINE_STRUCT_DESCRIPTOR(90, mass_error_invalid_identifier, Mass_Error_Invalid_Identifier,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("id"),
    .descriptor = &descriptor_value_pointer,
    .Base_Relative.offset = offsetof(Mass_Error_Invalid_Identifier, id),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_invalid_identifier);
MASS_DEFINE_STRUCT_DESCRIPTOR(91, mass_error_type_mismatch, Mass_Error_Type_Mismatch,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("expected"),
    .descriptor = &descriptor_descriptor_pointer,
    .Base_Relative.offset = offsetof(Mass_Error_Type_Mismatch, expected),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("actual"),
    .descriptor = &descriptor_descriptor_pointer,
    .Base_Relative.offset = offsetof(Mass_Error_Type_Mismatch, actual),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_type_mismatch);
MASS_DEFINE_STRUCT_DESCRIPTOR(92, mass_error_no_matching_overload, Mass_Error_No_Matching_Overload,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("target"),
    .descriptor = &descriptor_value_pointer,
    .Base_Relative.offset = offsetof(Mass_Error_No_Matching_Overload, target),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("arguments"),
    .descriptor = &descriptor_array_value_ptr,
    .Base_Relative.offset = offsetof(Mass_Error_No_Matching_Overload, arguments),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_no_matching_overload);
MASS_DEFINE_STRUCT_DESCRIPTOR(93, mass_error_undecidable_overload, Mass_Error_Undecidable_Overload,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("a"),
    .descriptor = &descriptor_value_pointer,
    .Base_Relative.offset = offsetof(Mass_Error_Undecidable_Overload, a),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("b"),
    .descriptor = &descriptor_value_pointer,
    .Base_Relative.offset = offsetof(Mass_Error_Undecidable_Overload, b),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error_undecidable_overload);
MASS_DEFINE_STRUCT_DESCRIPTOR(94, mass_error, Mass_Error,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_mass_error_tag,
    .Base_Relative.offset = offsetof(Mass_Error, tag),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("detailed_message"),
    .descriptor = &descriptor_slice,
    .Base_Relative.offset = offsetof(Mass_Error, detailed_message),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("source_range"),
    .descriptor = &descriptor_source_range,
    .Base_Relative.offset = offsetof(Mass_Error, source_range),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("User_Defined"),
    .descriptor = &descriptor_mass_error_user_defined,
    .Base_Relative.offset = offsetof(Mass_Error, User_Defined),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Integer_Range"),
    .descriptor = &descriptor_mass_error_integer_range,
    .Base_Relative.offset = offsetof(Mass_Error, Integer_Range),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("File_Open"),
    .descriptor = &descriptor_mass_error_file_open,
    .Base_Relative.offset = offsetof(Mass_Error, File_Open),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Unexpected_Token"),
    .descriptor = &descriptor_mass_error_unexpected_token,
    .Base_Relative.offset = offsetof(Mass_Error, Unexpected_Token),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Operator_Infix_Suffix_Conflict"),
    .descriptor = &descriptor_mass_error_operator_infix_suffix_conflict,
    .Base_Relative.offset = offsetof(Mass_Error, Operator_Infix_Suffix_Conflict),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Operator_Prefix_Conflict"),
    .descriptor = &descriptor_mass_error_operator_prefix_conflict,
    .Base_Relative.offset = offsetof(Mass_Error, Operator_Prefix_Conflict),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Undefined_Variable"),
    .descriptor = &descriptor_mass_error_undefined_variable,
    .Base_Relative.offset = offsetof(Mass_Error, Undefined_Variable),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Redifinition"),
    .descriptor = &descriptor_mass_error_redifinition,
    .Base_Relative.offset = offsetof(Mass_Error, Redifinition),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Unknown_Field"),
    .descriptor = &descriptor_mass_error_unknown_field,
    .Base_Relative.offset = offsetof(Mass_Error, Unknown_Field),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Invalid_Identifier"),
    .descriptor = &descriptor_mass_error_invalid_identifier,
    .Base_Relative.offset = offsetof(Mass_Error, Invalid_Identifier),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Type_Mismatch"),
    .descriptor = &descriptor_mass_error_type_mismatch,
    .Base_Relative.offset = offsetof(Mass_Error, Type_Mismatch),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("No_Matching_Overload"),
    .descriptor = &descriptor_mass_error_no_matching_overload,
    .Base_Relative.offset = offsetof(Mass_Error, No_Matching_Overload),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Undecidable_Overload"),
    .descriptor = &descriptor_mass_error_undecidable_overload,
    .Base_Relative.offset = offsetof(Mass_Error, Undecidable_Overload),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_error);
/*union struct end*/
/*union struct start */
MASS_DEFINE_OPAQUE_C_TYPE(array_mass_result_ptr, Array_Mass_Result_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_mass_result, Array_Mass_Result)
MASS_DEFINE_OPAQUE_C_TYPE(mass_result_tag, Mass_Result_Tag)
static C_Enum_Item mass_result_tag_items[] = {
{ .name = slice_literal_fields("Success"), .value = 0 },
{ .name = slice_literal_fields("Error"), .value = 1 },
};
MASS_DEFINE_STRUCT_DESCRIPTOR(95, mass_result_error, Mass_Result_Error,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("error"),
    .descriptor = &descriptor_mass_error,
    .Base_Relative.offset = offsetof(Mass_Result_Error, error),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_result_error);
MASS_DEFINE_STRUCT_DESCRIPTOR(96, mass_result, Mass_Result,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_mass_result_tag,
    .Base_Relative.offset = offsetof(Mass_Result, tag),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("Error"),
    .descriptor = &descriptor_mass_result_error,
    .Base_Relative.offset = offsetof(Mass_Result, Error),
  },
);
MASS_DEFINE_TYPE_VALUE(mass_result);
/*union struct end*/
MASS_DEFINE_OPAQUE_C_TYPE(array_program_ptr, Array_Program_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_program, Array_Program)
MASS_DEFINE_STRUCT_DESCRIPTOR(97, program, Program,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("import_libraries"),
    .descriptor = &descriptor_array_import_library,
    .Base_Relative.offset = offsetof(Program, import_libraries),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("labels"),
    .descriptor = &descriptor_array_label,
    .Base_Relative.offset = offsetof(Program, labels),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("patch_info_array"),
    .descriptor = &descriptor_array_label_location_diff_patch_info,
    .Base_Relative.offset = offsetof(Program, patch_info_array),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("startup_functions"),
    .descriptor = &descriptor_array_value_ptr,
    .Base_Relative.offset = offsetof(Program, startup_functions),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("relocations"),
    .descriptor = &descriptor_array_relocation,
    .Base_Relative.offset = offsetof(Program, relocations),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("entry_point"),
    .descriptor = &descriptor_value_pointer,
    .Base_Relative.offset = offsetof(Program, entry_point),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("functions"),
    .descriptor = &descriptor_array_function_builder,
    .Base_Relative.offset = offsetof(Program, functions),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("memory"),
    .descriptor = &descriptor_program_memory,
    .Base_Relative.offset = offsetof(Program, memory),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("default_calling_convention"),
    .descriptor = &descriptor_calling_convention_pointer,
    .Base_Relative.offset = offsetof(Program, default_calling_convention),
  },
);
MASS_DEFINE_TYPE_VALUE(program);
MASS_DEFINE_OPAQUE_C_TYPE(array_calling_convention_ptr, Array_Calling_Convention_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_calling_convention, Array_Calling_Convention)
MASS_DEFINE_STRUCT_DESCRIPTOR(98, calling_convention, Calling_Convention,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("register_volatile_bitset"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Calling_Convention, register_volatile_bitset),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("body_end_proc"),
    .descriptor = &descriptor_calling_convention_body_end_proc,
    .Base_Relative.offset = offsetof(Calling_Convention, body_end_proc),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("arguments_layout_proc"),
    .descriptor = &descriptor_calling_convention_arguments_layout_proc,
    .Base_Relative.offset = offsetof(Calling_Convention, arguments_layout_proc),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("return_proc"),
    .descriptor = &descriptor_calling_convention_return_proc,
    .Base_Relative.offset = offsetof(Calling_Convention, return_proc),
  },
);
MASS_DEFINE_TYPE_VALUE(calling_convention);
MASS_DEFINE_OPAQUE_C_TYPE(array_jit_counters_ptr, Array_Jit_Counters_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_jit_counters, Array_Jit_Counters)
MASS_DEFINE_STRUCT_DESCRIPTOR(99, jit_counters, Jit_Counters,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("functions"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Jit_Counters, functions),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("imports"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Jit_Counters, imports),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("startup"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Jit_Counters, startup),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("relocations"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Jit_Counters, relocations),
  },
);
MASS_DEFINE_TYPE_VALUE(jit_counters);
MASS_DEFINE_OPAQUE_C_TYPE(array_jit_ptr, Array_Jit_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_jit, Array_Jit)
MASS_DEFINE_STRUCT_DESCRIPTOR(100, jit, Jit,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("is_stack_unwinding_in_progress"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Jit, is_stack_unwinding_in_progress),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("program"),
    .descriptor = &descriptor_program_pointer,
    .Base_Relative.offset = offsetof(Jit, program),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("import_library_handles"),
    .descriptor = &descriptor_jit_import_library_handle_map_pointer,
    .Base_Relative.offset = offsetof(Jit, import_library_handles),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("previous_counts"),
    .descriptor = &descriptor_jit_counters,
    .Base_Relative.offset = offsetof(Jit, previous_counts),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("platform_specific_payload"),
    .descriptor = &descriptor_void_pointer,
    .Base_Relative.offset = offsetof(Jit, platform_specific_payload),
  },
);
MASS_DEFINE_TYPE_VALUE(jit);
MASS_DEFINE_OPAQUE_C_TYPE(array_compilation_ptr, Array_Compilation_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_compilation, Array_Compilation)
MASS_DEFINE_STRUCT_DESCRIPTOR(101, compilation, Compilation,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("temp_buffer"),
    .descriptor = &descriptor_virtual_memory_buffer,
    .Base_Relative.offset = offsetof(Compilation, temp_buffer),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("temp_allocator"),
    .descriptor = &descriptor_allocator_pointer,
    .Base_Relative.offset = offsetof(Compilation, temp_allocator),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("allocation_buffer"),
    .descriptor = &descriptor_virtual_memory_buffer,
    .Base_Relative.offset = offsetof(Compilation, allocation_buffer),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("allocator"),
    .descriptor = &descriptor_allocator_pointer,
    .Base_Relative.offset = offsetof(Compilation, allocator),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("jit"),
    .descriptor = &descriptor_jit,
    .Base_Relative.offset = offsetof(Compilation, jit),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("compiler_module"),
    .descriptor = &descriptor_module,
    .Base_Relative.offset = offsetof(Compilation, compiler_module),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("static_pointer_map"),
    .descriptor = &descriptor_static_pointer_map_pointer,
    .Base_Relative.offset = offsetof(Compilation, static_pointer_map),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("module_map"),
    .descriptor = &descriptor_imported_module_map_pointer,
    .Base_Relative.offset = offsetof(Compilation, module_map),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("root_scope"),
    .descriptor = &descriptor_scope_pointer,
    .Base_Relative.offset = offsetof(Compilation, root_scope),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("runtime_program"),
    .descriptor = &descriptor_program_pointer,
    .Base_Relative.offset = offsetof(Compilation, runtime_program),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("result"),
    .descriptor = &descriptor_mass_result_pointer,
    .Base_Relative.offset = offsetof(Compilation, result),
  },
);
MASS_DEFINE_TYPE_VALUE(compilation);
MASS_DEFINE_OPAQUE_C_TYPE(u8, u8)
MASS_DEFINE_OPAQUE_C_TYPE(array_u8, Array_u8)
MASS_DEFINE_OPAQUE_C_TYPE(u16, u16)
MASS_DEFINE_OPAQUE_C_TYPE(array_u16, Array_u16)
MASS_DEFINE_OPAQUE_C_TYPE(u32, u32)
MASS_DEFINE_OPAQUE_C_TYPE(array_u32, Array_u32)
MASS_DEFINE_OPAQUE_C_TYPE(u64, u64)
MASS_DEFINE_OPAQUE_C_TYPE(array_u64, Array_u64)
MASS_DEFINE_OPAQUE_C_TYPE(s8, s8)
MASS_DEFINE_OPAQUE_C_TYPE(array_s8, Array_s8)
MASS_DEFINE_OPAQUE_C_TYPE(s16, s16)
MASS_DEFINE_OPAQUE_C_TYPE(array_s16, Array_s16)
MASS_DEFINE_OPAQUE_C_TYPE(s32, s32)
MASS_DEFINE_OPAQUE_C_TYPE(array_s32, Array_s32)
MASS_DEFINE_OPAQUE_C_TYPE(s64, s64)
MASS_DEFINE_OPAQUE_C_TYPE(array_s64, Array_s64)
MASS_DEFINE_OPAQUE_C_TYPE(f32, f32)
MASS_DEFINE_OPAQUE_C_TYPE(array_f32, Array_f32)
MASS_DEFINE_OPAQUE_C_TYPE(f64, f64)
MASS_DEFINE_OPAQUE_C_TYPE(array_f64, Array_f64)
MASS_DEFINE_OPAQUE_C_TYPE(array_slice_ptr, Array_Slice_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_slice, Array_Slice)
MASS_DEFINE_STRUCT_DESCRIPTOR(102, slice, Slice,
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("bytes"),
    .descriptor = &descriptor_u8_pointer,
    .Base_Relative.offset = offsetof(Slice, bytes),
  },
  {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = slice_literal_fields("length"),
    .descriptor = &descriptor_u64,
    .Base_Relative.offset = offsetof(Slice, length),
  },
);
MASS_DEFINE_TYPE_VALUE(slice);

#endif // GENERATED_TYPES_H
