#ifndef GENERATED_TYPES_H
#define GENERATED_TYPES_H
_Pragma("warning (push)") _Pragma("warning (default: 4820)")
typedef void(*fn_type_opaque)();

typedef struct Scope Scope;

typedef struct Function_Builder Function_Builder;

typedef struct Program Program;

typedef struct Compilation Compilation;

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

typedef enum Module_Flags Module_Flags;

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

typedef struct Maybe_Register Maybe_Register;
typedef dyn_array_type(Maybe_Register *) Array_Maybe_Register_Ptr;
typedef dyn_array_type(const Maybe_Register *) Array_Const_Maybe_Register_Ptr;

typedef struct Memory_Location Memory_Location;
typedef dyn_array_type(Memory_Location *) Array_Memory_Location_Ptr;
typedef dyn_array_type(const Memory_Location *) Array_Const_Memory_Location_Ptr;

typedef struct Static_Memory Static_Memory;
typedef dyn_array_type(Static_Memory *) Array_Static_Memory_Ptr;
typedef dyn_array_type(const Static_Memory *) Array_Const_Static_Memory_Ptr;

typedef struct Storage Storage;
typedef dyn_array_type(Storage *) Array_Storage_Ptr;
typedef dyn_array_type(const Storage *) Array_Const_Storage_Ptr;

typedef struct Compiler_Source_Location Compiler_Source_Location;
typedef dyn_array_type(Compiler_Source_Location *) Array_Compiler_Source_Location_Ptr;
typedef dyn_array_type(const Compiler_Source_Location *) Array_Const_Compiler_Source_Location_Ptr;

typedef enum Operator_Fixity Operator_Fixity;

typedef enum Operator_Associativity Operator_Associativity;

typedef struct Execution_Context Execution_Context;
typedef dyn_array_type(Execution_Context *) Array_Execution_Context_Ptr;
typedef dyn_array_type(const Execution_Context *) Array_Const_Execution_Context_Ptr;

typedef struct Operator Operator;
typedef dyn_array_type(Operator *) Array_Operator_Ptr;
typedef dyn_array_type(const Operator *) Array_Const_Operator_Ptr;

typedef struct Scope_Entry Scope_Entry;
typedef dyn_array_type(Scope_Entry *) Array_Scope_Entry_Ptr;
typedef dyn_array_type(const Scope_Entry *) Array_Const_Scope_Entry_Ptr;

typedef struct Value Value;
typedef dyn_array_type(Value *) Array_Value_Ptr;
typedef dyn_array_type(const Value *) Array_Const_Value_Ptr;

typedef enum Expected_Result_Storage Expected_Result_Storage;

typedef struct Expected_Result Expected_Result;
typedef dyn_array_type(Expected_Result *) Array_Expected_Result_Ptr;
typedef dyn_array_type(const Expected_Result *) Array_Const_Expected_Result_Ptr;

typedef Value * (*Lazy_Value_Proc)
  (Execution_Context * context, const Expected_Result * expected_result, void * payload);

typedef struct Lazy_Value Lazy_Value;
typedef dyn_array_type(Lazy_Value *) Array_Lazy_Value_Ptr;
typedef dyn_array_type(const Lazy_Value *) Array_Const_Lazy_Value_Ptr;

typedef Value * (*Mass_Handle_Operator_Proc)
  (Execution_Context * context, Value_View view, void * payload);

typedef enum Descriptor_Function_Flags Descriptor_Function_Flags;

typedef struct Descriptor_Struct_Field Descriptor_Struct_Field;
typedef dyn_array_type(Descriptor_Struct_Field *) Array_Descriptor_Struct_Field_Ptr;
typedef dyn_array_type(const Descriptor_Struct_Field *) Array_Const_Descriptor_Struct_Field_Ptr;

typedef struct Function_Argument Function_Argument;
typedef dyn_array_type(Function_Argument *) Array_Function_Argument_Ptr;
typedef dyn_array_type(const Function_Argument *) Array_Const_Function_Argument_Ptr;

typedef struct Function_Return Function_Return;
typedef dyn_array_type(Function_Return *) Array_Function_Return_Ptr;
typedef dyn_array_type(const Function_Return *) Array_Const_Function_Return_Ptr;

typedef struct Function_Info Function_Info;
typedef dyn_array_type(Function_Info *) Array_Function_Info_Ptr;
typedef dyn_array_type(const Function_Info *) Array_Const_Function_Info_Ptr;

typedef struct Descriptor Descriptor;
typedef dyn_array_type(Descriptor *) Array_Descriptor_Ptr;
typedef dyn_array_type(const Descriptor *) Array_Const_Descriptor_Ptr;

typedef struct Mass_Result Mass_Result;
typedef dyn_array_type(Mass_Result *) Array_Mass_Result_Ptr;
typedef dyn_array_type(const Mass_Result *) Array_Const_Mass_Result_Ptr;


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

typedef enum Module_Flags {
  Module_Flags_Has_Exports = 1,
} Module_Flags;

#define Module_Flags__Max 1
#define Module_Flags__Min 1

const char *module_flags_name(Module_Flags value) {
  if (value == 1) return "Module_Flags_Has_Exports";
  assert(!"Unexpected value for enum Module_Flags");
  return 0;
};

typedef struct Module {
  Module_Flags flags;
  u32 _flags_padding;
  Source_File source_file;
  Source_Range exports_source_range;
  Scope * own_scope;
  Scope * export_scope;
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

#define Group_Tag__Max 3
#define Group_Tag__Min 1

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

#define Symbol_Type__Max 2
#define Symbol_Type__Min 1

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

typedef struct {
  Slice name;
} Token_Pattern_Symbol;
typedef struct {
  Group_Tag tag;
} Token_Pattern_Group;
typedef struct {
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

#define Section_Permissions__Max 4
#define Section_Permissions__Min 1

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

typedef enum Register {
  Register_A = 0,
  Register_C = 1,
  Register_D = 2,
  Register_B = 3,
  Register_SP = 4,
  Register_AH = 4,
  Register_BP = 5,
  Register_CH = 4,
  Register_SI = 6,
  Register_DH = 4,
  Register_DI = 7,
  Register_BH = 4,
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

#define Register__Max 31
#define Register__Min 0

const char *register_name(Register value) {
  if (value == 0) return "Register_A";
  if (value == 1) return "Register_C";
  if (value == 2) return "Register_D";
  if (value == 3) return "Register_B";
  if (value == 4) return "Register_SP";
  if (value == 4) return "Register_AH";
  if (value == 5) return "Register_BP";
  if (value == 4) return "Register_CH";
  if (value == 6) return "Register_SI";
  if (value == 4) return "Register_DH";
  if (value == 7) return "Register_DI";
  if (value == 4) return "Register_BH";
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

#define Number_Base__Max 16
#define Number_Base__Min 2

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

#define Compare_Type__Max 10
#define Compare_Type__Min 1

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

typedef struct Maybe_Register {
  Register index;
  u32 has_value;
} Maybe_Register;
typedef dyn_array_type(Maybe_Register) Array_Maybe_Register;

typedef enum {
  Memory_Location_Tag_Instruction_Pointer_Relative = 0,
  Memory_Location_Tag_Indirect = 1,
} Memory_Location_Tag;

typedef struct {
  Label_Index label_index;
} Memory_Location_Instruction_Pointer_Relative;
typedef struct {
  Register base_register;
  u32 _base_register_padding;
  Maybe_Register maybe_index_register;
  s64 offset;
} Memory_Location_Indirect;
typedef struct Memory_Location {
  Memory_Location_Tag tag;
  char _tag_padding[4];
  union {
    Memory_Location_Instruction_Pointer_Relative Instruction_Pointer_Relative;
    Memory_Location_Indirect Indirect;
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

typedef struct {
  u8 value;
} Static_Memory_U8;
typedef struct {
  u16 value;
} Static_Memory_U16;
typedef struct {
  u32 value;
} Static_Memory_U32;
typedef struct {
  u64 value;
} Static_Memory_U64;
typedef struct {
  void * pointer;
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

typedef struct {
  Compare_Type compare_type;
} Storage_Eflags;
typedef struct {
  Register index;
} Storage_Register;
typedef struct {
  Register index;
} Storage_Xmm;
typedef struct {
  Static_Memory memory;
} Storage_Static;
typedef struct {
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
typedef struct Compiler_Source_Location {
  const char * filename;
  const char * function_name;
  u64 line_number;
} Compiler_Source_Location;
typedef dyn_array_type(Compiler_Source_Location) Array_Compiler_Source_Location;

typedef enum Operator_Fixity {
  Operator_Fixity_Infix = 1,
  Operator_Fixity_Prefix = 2,
  Operator_Fixity_Postfix = 4,
} Operator_Fixity;

#define Operator_Fixity__Max 4
#define Operator_Fixity__Min 1

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

#define Operator_Associativity__Max 1
#define Operator_Associativity__Min 0

const char *operator_associativity_name(Operator_Associativity value) {
  if (value == 0) return "Operator_Associativity_Left";
  if (value == 1) return "Operator_Associativity_Right";
  assert(!"Unexpected value for enum Operator_Associativity");
  return 0;
};

typedef struct Execution_Context {
  Allocator * allocator;
  Compilation * compilation;
  u64 epoch;
  Program * program;
  Scope * scope;
  Function_Builder * builder;
  Module * module;
  Mass_Result * result;
} Execution_Context;
typedef dyn_array_type(Execution_Context) Array_Execution_Context;

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
  Scope_Entry_Tag_Value = 0,
  Scope_Entry_Tag_Operator = 1,
} Scope_Entry_Tag;

typedef struct {
  Value * value;
} Scope_Entry_Value;
typedef struct {
  Operator * maybe_prefix;
  Operator * maybe_infix_or_postfix;
} Scope_Entry_Operator;
typedef struct Scope_Entry {
  Scope_Entry_Tag tag;
  char _tag_padding[4];
  Source_Range source_range;
  union {
    Scope_Entry_Value Value;
    Scope_Entry_Operator Operator;
  };
} Scope_Entry;
typedef dyn_array_type(Scope_Entry) Array_Scope_Entry;
typedef struct Value {
  const Descriptor * descriptor;
  Storage storage;
  Value * next_overload;
  u64 epoch;
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

#define Expected_Result_Storage__Max 16
#define Expected_Result_Storage__Min 0

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

typedef struct {
  Value * value;
} Expected_Result_Exact;
typedef struct {
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
} Lazy_Value;
typedef dyn_array_type(Lazy_Value) Array_Lazy_Value;

typedef enum Descriptor_Function_Flags {
  Descriptor_Function_Flags_None = 0,
  Descriptor_Function_Flags_Macro = 1,
  Descriptor_Function_Flags_No_Own_Scope = 2,
  Descriptor_Function_Flags_No_Own_Return = 4,
  Descriptor_Function_Flags_Compile_Time = 8,
} Descriptor_Function_Flags;

#define Descriptor_Function_Flags__Max 8
#define Descriptor_Function_Flags__Min 0

const char *descriptor_function_flags_name(Descriptor_Function_Flags value) {
  if (value == 0) return "Descriptor_Function_Flags_None";
  if (value == 1) return "Descriptor_Function_Flags_Macro";
  if (value == 2) return "Descriptor_Function_Flags_No_Own_Scope";
  if (value == 4) return "Descriptor_Function_Flags_No_Own_Return";
  if (value == 8) return "Descriptor_Function_Flags_Compile_Time";
  assert(!"Unexpected value for enum Descriptor_Function_Flags");
  return 0;
};

typedef struct Descriptor_Struct_Field {
  Slice name;
  const Descriptor * descriptor;
  u64 offset;
} Descriptor_Struct_Field;
typedef dyn_array_type(Descriptor_Struct_Field) Array_Descriptor_Struct_Field;

typedef struct Function_Argument {
  Slice name;
  Value * value;
  Value_View maybe_default_expression;
} Function_Argument;
typedef dyn_array_type(Function_Argument) Array_Function_Argument;

typedef struct Function_Return {
  Slice name;
  const Descriptor * descriptor;
} Function_Return;
typedef dyn_array_type(Function_Return) Array_Function_Return;

typedef struct Function_Info {
  Descriptor_Function_Flags flags;
  u32 _flags_padding;
  Array_Function_Argument arguments;
  Value * body;
  Scope * scope;
  Function_Return returns;
} Function_Info;
typedef dyn_array_type(Function_Info) Array_Function_Info;

typedef enum {
  Descriptor_Tag_Void = 0,
  Descriptor_Tag_Opaque = 1,
  Descriptor_Tag_Function = 2,
  Descriptor_Tag_Fixed_Size_Array = 3,
  Descriptor_Tag_Struct = 4,
  Descriptor_Tag_Pointer = 5,
} Descriptor_Tag;

typedef struct {
  u64 bit_size;
} Descriptor_Opaque;
typedef struct {
  Function_Info info;
} Descriptor_Function;
typedef struct {
  const Descriptor * item;
  u64 length;
} Descriptor_Fixed_Size_Array;
typedef struct {
  Slice name;
  Array_Descriptor_Struct_Field fields;
} Descriptor_Struct;
typedef struct {
  const Descriptor * to;
} Descriptor_Pointer;
typedef struct Descriptor {
  Descriptor_Tag tag;
  char _tag_padding[4];
  Slice name;
  union {
    Descriptor_Opaque Opaque;
    Descriptor_Function Function;
    Descriptor_Fixed_Size_Array Fixed_Size_Array;
    Descriptor_Struct Struct;
    Descriptor_Pointer Pointer;
  };
} Descriptor;
typedef dyn_array_type(Descriptor) Array_Descriptor;
typedef enum {
  Mass_Result_Tag_Success = 0,
  Mass_Result_Tag_Error = 1,
} Mass_Result_Tag;

typedef struct {
  Parse_Error details;
} Mass_Result_Error;
typedef struct Mass_Result {
  Mass_Result_Tag tag;
  char _tag_padding[4];
  union {
    Mass_Result_Error Error;
  };
} Mass_Result;
typedef dyn_array_type(Mass_Result) Array_Mass_Result;
_Pragma("warning (pop)")

// Mass Type Reflection

static Descriptor descriptor_function_builder_pointer;
static Descriptor descriptor_program_pointer;
static Descriptor descriptor_scope_pointer;
static Descriptor descriptor_compilation_pointer;
static Descriptor descriptor_void;
static Descriptor descriptor_void_pointer;
static Descriptor descriptor_char;
static Descriptor descriptor_char_pointer;
MASS_DEFINE_OPAQUE_C_TYPE(type, Descriptor);
MASS_DEFINE_OPAQUE_C_TYPE(allocator, Allocator);
MASS_DEFINE_OPAQUE_C_TYPE(virtual_memory_buffer, Virtual_Memory_Buffer);
MASS_DEFINE_OPAQUE_C_TYPE(range_u64, Range_u64);
MASS_DEFINE_OPAQUE_C_TYPE(array_range_u64, Array_Range_u64);
#define MASS_PROCESS_BUILT_IN_TYPE(_NAME_, _BIT_SIZE_)\
  MASS_DEFINE_OPAQUE_TYPE(_NAME_, _BIT_SIZE_)
MASS_ENUMERATE_BUILT_IN_TYPES
#undef MASS_PROCESS_BUILT_IN_TYPE

typedef dyn_array_type(Slice *) Array_Slice_Ptr;
static Descriptor descriptor_source_position;
static Descriptor descriptor_array_source_position_ptr;
static Descriptor descriptor_source_position_pointer;
static Descriptor descriptor_source_position_pointer_pointer;
static Descriptor descriptor_source_file;
static Descriptor descriptor_array_source_file_ptr;
static Descriptor descriptor_source_file_pointer;
static Descriptor descriptor_source_file_pointer_pointer;
static Descriptor descriptor_source_range;
static Descriptor descriptor_array_source_range_ptr;
static Descriptor descriptor_source_range_pointer;
static Descriptor descriptor_source_range_pointer_pointer;
static Descriptor descriptor_module_flags;
static Descriptor descriptor_array_module_flags_ptr;
static Descriptor descriptor_module_flags_pointer;
static Descriptor descriptor_module_flags_pointer_pointer;
static Descriptor descriptor_module;
static Descriptor descriptor_array_module_ptr;
static Descriptor descriptor_module_pointer;
static Descriptor descriptor_module_pointer_pointer;
static Descriptor descriptor_parse_error;
static Descriptor descriptor_array_parse_error_ptr;
static Descriptor descriptor_parse_error_pointer;
static Descriptor descriptor_parse_error_pointer_pointer;
static Descriptor descriptor_group_tag;
static Descriptor descriptor_array_group_tag_ptr;
static Descriptor descriptor_group_tag_pointer;
static Descriptor descriptor_group_tag_pointer_pointer;
static Descriptor descriptor_value_view;
static Descriptor descriptor_array_value_view_ptr;
static Descriptor descriptor_value_view_pointer;
static Descriptor descriptor_value_view_pointer_pointer;
static Descriptor descriptor_symbol_type;
static Descriptor descriptor_array_symbol_type_ptr;
static Descriptor descriptor_symbol_type_pointer;
static Descriptor descriptor_symbol_type_pointer_pointer;
static Descriptor descriptor_symbol;
static Descriptor descriptor_array_symbol_ptr;
static Descriptor descriptor_symbol_pointer;
static Descriptor descriptor_symbol_pointer_pointer;
static Descriptor descriptor_group;
static Descriptor descriptor_array_group_ptr;
static Descriptor descriptor_group_pointer;
static Descriptor descriptor_group_pointer_pointer;
static Descriptor descriptor_token_pattern;
static Descriptor descriptor_array_token_pattern_ptr;
static Descriptor descriptor_token_pattern_pointer;
static Descriptor descriptor_token_pattern_pointer_pointer;
MASS_DEFINE_OPAQUE_C_TYPE(token_pattern_tag, Token_Pattern_Tag)
static Descriptor descriptor_section_permissions;
static Descriptor descriptor_array_section_permissions_ptr;
static Descriptor descriptor_section_permissions_pointer;
static Descriptor descriptor_section_permissions_pointer_pointer;
static Descriptor descriptor_section;
static Descriptor descriptor_array_section_ptr;
static Descriptor descriptor_section_pointer;
static Descriptor descriptor_section_pointer_pointer;
static Descriptor descriptor_register;
static Descriptor descriptor_array_register_ptr;
static Descriptor descriptor_register_pointer;
static Descriptor descriptor_register_pointer_pointer;
static Descriptor descriptor_label_index;
static Descriptor descriptor_array_label_index_ptr;
static Descriptor descriptor_label_index_pointer;
static Descriptor descriptor_label_index_pointer_pointer;
static Descriptor descriptor_label;
static Descriptor descriptor_array_label_ptr;
static Descriptor descriptor_label_pointer;
static Descriptor descriptor_label_pointer_pointer;
static Descriptor descriptor_label_location_diff_patch_info;
static Descriptor descriptor_array_label_location_diff_patch_info_ptr;
static Descriptor descriptor_label_location_diff_patch_info_pointer;
static Descriptor descriptor_label_location_diff_patch_info_pointer_pointer;
static Descriptor descriptor_number_base;
static Descriptor descriptor_array_number_base_ptr;
static Descriptor descriptor_number_base_pointer;
static Descriptor descriptor_number_base_pointer_pointer;
static Descriptor descriptor_number_literal;
static Descriptor descriptor_array_number_literal_ptr;
static Descriptor descriptor_number_literal_pointer;
static Descriptor descriptor_number_literal_pointer_pointer;
static Descriptor descriptor_external_symbol;
static Descriptor descriptor_array_external_symbol_ptr;
static Descriptor descriptor_external_symbol_pointer;
static Descriptor descriptor_external_symbol_pointer_pointer;
static Descriptor descriptor_import_symbol;
static Descriptor descriptor_array_import_symbol_ptr;
static Descriptor descriptor_import_symbol_pointer;
static Descriptor descriptor_import_symbol_pointer_pointer;
static Descriptor descriptor_import_library;
static Descriptor descriptor_array_import_library_ptr;
static Descriptor descriptor_import_library_pointer;
static Descriptor descriptor_import_library_pointer_pointer;
static Descriptor descriptor_compare_type;
static Descriptor descriptor_array_compare_type_ptr;
static Descriptor descriptor_compare_type_pointer;
static Descriptor descriptor_compare_type_pointer_pointer;
static Descriptor descriptor_maybe_register;
static Descriptor descriptor_array_maybe_register_ptr;
static Descriptor descriptor_maybe_register_pointer;
static Descriptor descriptor_maybe_register_pointer_pointer;
static Descriptor descriptor_memory_location;
static Descriptor descriptor_array_memory_location_ptr;
static Descriptor descriptor_memory_location_pointer;
static Descriptor descriptor_memory_location_pointer_pointer;
MASS_DEFINE_OPAQUE_C_TYPE(memory_location_tag, Memory_Location_Tag)
static Descriptor descriptor_static_memory;
static Descriptor descriptor_array_static_memory_ptr;
static Descriptor descriptor_static_memory_pointer;
static Descriptor descriptor_static_memory_pointer_pointer;
MASS_DEFINE_OPAQUE_C_TYPE(static_memory_tag, Static_Memory_Tag)
static Descriptor descriptor_storage;
static Descriptor descriptor_array_storage_ptr;
static Descriptor descriptor_storage_pointer;
static Descriptor descriptor_storage_pointer_pointer;
MASS_DEFINE_OPAQUE_C_TYPE(storage_tag, Storage_Tag)
static Descriptor descriptor_compiler_source_location;
static Descriptor descriptor_array_compiler_source_location_ptr;
static Descriptor descriptor_compiler_source_location_pointer;
static Descriptor descriptor_compiler_source_location_pointer_pointer;
static Descriptor descriptor_operator_fixity;
static Descriptor descriptor_array_operator_fixity_ptr;
static Descriptor descriptor_operator_fixity_pointer;
static Descriptor descriptor_operator_fixity_pointer_pointer;
static Descriptor descriptor_operator_associativity;
static Descriptor descriptor_array_operator_associativity_ptr;
static Descriptor descriptor_operator_associativity_pointer;
static Descriptor descriptor_operator_associativity_pointer_pointer;
static Descriptor descriptor_execution_context;
static Descriptor descriptor_array_execution_context_ptr;
static Descriptor descriptor_execution_context_pointer;
static Descriptor descriptor_execution_context_pointer_pointer;
static Descriptor descriptor_operator;
static Descriptor descriptor_array_operator_ptr;
static Descriptor descriptor_operator_pointer;
static Descriptor descriptor_operator_pointer_pointer;
static Descriptor descriptor_scope_entry;
static Descriptor descriptor_array_scope_entry_ptr;
static Descriptor descriptor_scope_entry_pointer;
static Descriptor descriptor_scope_entry_pointer_pointer;
MASS_DEFINE_OPAQUE_C_TYPE(scope_entry_tag, Scope_Entry_Tag)
static Descriptor descriptor_value;
static Descriptor descriptor_array_value_ptr;
static Descriptor descriptor_value_pointer;
static Descriptor descriptor_value_pointer_pointer;
static Descriptor descriptor_expected_result_storage;
static Descriptor descriptor_array_expected_result_storage_ptr;
static Descriptor descriptor_expected_result_storage_pointer;
static Descriptor descriptor_expected_result_storage_pointer_pointer;
static Descriptor descriptor_expected_result;
static Descriptor descriptor_array_expected_result_ptr;
static Descriptor descriptor_expected_result_pointer;
static Descriptor descriptor_expected_result_pointer_pointer;
MASS_DEFINE_OPAQUE_C_TYPE(expected_result_tag, Expected_Result_Tag)
static Descriptor descriptor_lazy_value_proc;
static Descriptor descriptor_lazy_value;
static Descriptor descriptor_array_lazy_value_ptr;
static Descriptor descriptor_lazy_value_pointer;
static Descriptor descriptor_lazy_value_pointer_pointer;
static Descriptor descriptor_mass_handle_operator_proc;
static Descriptor descriptor_descriptor_function_flags;
static Descriptor descriptor_array_descriptor_function_flags_ptr;
static Descriptor descriptor_descriptor_function_flags_pointer;
static Descriptor descriptor_descriptor_function_flags_pointer_pointer;
static Descriptor descriptor_descriptor_struct_field;
static Descriptor descriptor_array_descriptor_struct_field_ptr;
static Descriptor descriptor_descriptor_struct_field_pointer;
static Descriptor descriptor_descriptor_struct_field_pointer_pointer;
static Descriptor descriptor_function_argument;
static Descriptor descriptor_array_function_argument_ptr;
static Descriptor descriptor_function_argument_pointer;
static Descriptor descriptor_function_argument_pointer_pointer;
static Descriptor descriptor_function_return;
static Descriptor descriptor_array_function_return_ptr;
static Descriptor descriptor_function_return_pointer;
static Descriptor descriptor_function_return_pointer_pointer;
static Descriptor descriptor_function_info;
static Descriptor descriptor_array_function_info_ptr;
static Descriptor descriptor_function_info_pointer;
static Descriptor descriptor_function_info_pointer_pointer;
static Descriptor descriptor_descriptor;
static Descriptor descriptor_array_descriptor_ptr;
static Descriptor descriptor_descriptor_pointer;
static Descriptor descriptor_descriptor_pointer_pointer;
MASS_DEFINE_OPAQUE_C_TYPE(descriptor_tag, Descriptor_Tag)
static Descriptor descriptor_mass_result;
static Descriptor descriptor_array_mass_result_ptr;
static Descriptor descriptor_mass_result_pointer;
static Descriptor descriptor_mass_result_pointer_pointer;
MASS_DEFINE_OPAQUE_C_TYPE(mass_result_tag, Mass_Result_Tag)
static Descriptor descriptor_slice;
static Descriptor descriptor_array_slice_ptr;
static Descriptor descriptor_slice_pointer;
static Descriptor descriptor_slice_pointer_pointer;
MASS_DEFINE_OPAQUE_C_TYPE(array_source_position_ptr, Array_Source_Position_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_source_position, Array_Source_Position)
MASS_DEFINE_STRUCT_DESCRIPTOR(source_position,
  {
    .name = slice_literal_fields("line"),
    .descriptor = &descriptor_u64,
    .offset = offsetof(Source_Position, line),
  },
  {
    .name = slice_literal_fields("column"),
    .descriptor = &descriptor_u64,
    .offset = offsetof(Source_Position, column),
  },
);
MASS_DEFINE_TYPE_VALUE(source_position);
MASS_DEFINE_OPAQUE_C_TYPE(array_source_file_ptr, Array_Source_File_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_source_file, Array_Source_File)
MASS_DEFINE_STRUCT_DESCRIPTOR(source_file,
  {
    .name = slice_literal_fields("path"),
    .descriptor = &descriptor_slice,
    .offset = offsetof(Source_File, path),
  },
  {
    .name = slice_literal_fields("text"),
    .descriptor = &descriptor_slice,
    .offset = offsetof(Source_File, text),
  },
  {
    .name = slice_literal_fields("line_ranges"),
    .descriptor = &descriptor_array_range_u64,
    .offset = offsetof(Source_File, line_ranges),
  },
);
MASS_DEFINE_TYPE_VALUE(source_file);
MASS_DEFINE_OPAQUE_C_TYPE(array_source_range_ptr, Array_Source_Range_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_source_range, Array_Source_Range)
MASS_DEFINE_STRUCT_DESCRIPTOR(source_range,
  {
    .name = slice_literal_fields("file"),
    .descriptor = &descriptor_source_file_pointer,
    .offset = offsetof(Source_Range, file),
  },
  {
    .name = slice_literal_fields("offsets"),
    .descriptor = &descriptor_range_u64,
    .offset = offsetof(Source_Range, offsets),
  },
);
MASS_DEFINE_TYPE_VALUE(source_range);
MASS_DEFINE_OPAQUE_C_TYPE(module_flags, Module_Flags)
MASS_DEFINE_OPAQUE_C_TYPE(array_module_ptr, Array_Module_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_module, Array_Module)
MASS_DEFINE_STRUCT_DESCRIPTOR(module,
  {
    .name = slice_literal_fields("flags"),
    .descriptor = &descriptor_module_flags,
    .offset = offsetof(Module, flags),
  },
  {
    .name = slice_literal_fields("_flags_padding"),
    .descriptor = &descriptor_u32,
    .offset = offsetof(Module, _flags_padding),
  },
  {
    .name = slice_literal_fields("source_file"),
    .descriptor = &descriptor_source_file,
    .offset = offsetof(Module, source_file),
  },
  {
    .name = slice_literal_fields("exports_source_range"),
    .descriptor = &descriptor_source_range,
    .offset = offsetof(Module, exports_source_range),
  },
  {
    .name = slice_literal_fields("own_scope"),
    .descriptor = &descriptor_scope_pointer,
    .offset = offsetof(Module, own_scope),
  },
  {
    .name = slice_literal_fields("export_scope"),
    .descriptor = &descriptor_scope_pointer,
    .offset = offsetof(Module, export_scope),
  },
);
MASS_DEFINE_TYPE_VALUE(module);
MASS_DEFINE_OPAQUE_C_TYPE(array_parse_error_ptr, Array_Parse_Error_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_parse_error, Array_Parse_Error)
MASS_DEFINE_STRUCT_DESCRIPTOR(parse_error,
  {
    .name = slice_literal_fields("message"),
    .descriptor = &descriptor_slice,
    .offset = offsetof(Parse_Error, message),
  },
  {
    .name = slice_literal_fields("source_range"),
    .descriptor = &descriptor_source_range,
    .offset = offsetof(Parse_Error, source_range),
  },
);
MASS_DEFINE_TYPE_VALUE(parse_error);
MASS_DEFINE_OPAQUE_C_TYPE(group_tag, Group_Tag)
MASS_DEFINE_OPAQUE_C_TYPE(array_value_view_ptr, Array_Value_View_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_value_view, Array_Value_View)
MASS_DEFINE_STRUCT_DESCRIPTOR(value_view,
  {
    .name = slice_literal_fields("values"),
    .descriptor = &descriptor_value_pointer_pointer,
    .offset = offsetof(Value_View, values),
  },
  {
    .name = slice_literal_fields("length"),
    .descriptor = &descriptor_u64,
    .offset = offsetof(Value_View, length),
  },
  {
    .name = slice_literal_fields("source_range"),
    .descriptor = &descriptor_source_range,
    .offset = offsetof(Value_View, source_range),
  },
);
MASS_DEFINE_TYPE_VALUE(value_view);
MASS_DEFINE_OPAQUE_C_TYPE(symbol_type, Symbol_Type)
MASS_DEFINE_OPAQUE_C_TYPE(array_symbol_ptr, Array_Symbol_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_symbol, Array_Symbol)
MASS_DEFINE_STRUCT_DESCRIPTOR(symbol,
  {
    .name = slice_literal_fields("type"),
    .descriptor = &descriptor_symbol_type,
    .offset = offsetof(Symbol, type),
  },
  {
    .name = slice_literal_fields("_type_padding"),
    .descriptor = &descriptor_u32,
    .offset = offsetof(Symbol, _type_padding),
  },
  {
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .offset = offsetof(Symbol, name),
  },
);
MASS_DEFINE_TYPE_VALUE(symbol);
MASS_DEFINE_OPAQUE_C_TYPE(array_group_ptr, Array_Group_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_group, Array_Group)
MASS_DEFINE_STRUCT_DESCRIPTOR(group,
  {
    .name = slice_literal_fields("tag"),
    .descriptor = &descriptor_group_tag,
    .offset = offsetof(Group, tag),
  },
  {
    .name = slice_literal_fields("_tag_padding"),
    .descriptor = &descriptor_u32,
    .offset = offsetof(Group, _tag_padding),
  },
  {
    .name = slice_literal_fields("children"),
    .descriptor = &descriptor_value_view,
    .offset = offsetof(Group, children),
  },
);
MASS_DEFINE_TYPE_VALUE(group);
MASS_DEFINE_OPAQUE_C_TYPE(token_pattern, Token_Pattern)
MASS_DEFINE_OPAQUE_C_TYPE(section_permissions, Section_Permissions)
MASS_DEFINE_OPAQUE_C_TYPE(array_section_ptr, Array_Section_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_section, Array_Section)
MASS_DEFINE_STRUCT_DESCRIPTOR(section,
  {
    .name = slice_literal_fields("buffer"),
    .descriptor = &descriptor_virtual_memory_buffer,
    .offset = offsetof(Section, buffer),
  },
  {
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .offset = offsetof(Section, name),
  },
  {
    .name = slice_literal_fields("base_rva"),
    .descriptor = &descriptor_u32,
    .offset = offsetof(Section, base_rva),
  },
  {
    .name = slice_literal_fields("permissions"),
    .descriptor = &descriptor_section_permissions,
    .offset = offsetof(Section, permissions),
  },
);
MASS_DEFINE_TYPE_VALUE(section);
MASS_DEFINE_OPAQUE_C_TYPE(register, Register)
MASS_DEFINE_OPAQUE_C_TYPE(array_label_index_ptr, Array_Label_Index_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_label_index, Array_Label_Index)
MASS_DEFINE_STRUCT_DESCRIPTOR(label_index,
  {
    .name = slice_literal_fields("value"),
    .descriptor = &descriptor_u64,
    .offset = offsetof(Label_Index, value),
  },
);
MASS_DEFINE_TYPE_VALUE(label_index);
MASS_DEFINE_OPAQUE_C_TYPE(array_label_ptr, Array_Label_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_label, Array_Label)
MASS_DEFINE_STRUCT_DESCRIPTOR(label,
  {
    .name = slice_literal_fields("resolved"),
    .descriptor = &descriptor_u32,
    .offset = offsetof(Label, resolved),
  },
  {
    .name = slice_literal_fields("offset_in_section"),
    .descriptor = &descriptor_u32,
    .offset = offsetof(Label, offset_in_section),
  },
  {
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .offset = offsetof(Label, name),
  },
  {
    .name = slice_literal_fields("section"),
    .descriptor = &descriptor_section_pointer,
    .offset = offsetof(Label, section),
  },
);
MASS_DEFINE_TYPE_VALUE(label);
MASS_DEFINE_OPAQUE_C_TYPE(array_label_location_diff_patch_info_ptr, Array_Label_Location_Diff_Patch_Info_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_label_location_diff_patch_info, Array_Label_Location_Diff_Patch_Info)
MASS_DEFINE_STRUCT_DESCRIPTOR(label_location_diff_patch_info,
  {
    .name = slice_literal_fields("target_label_index"),
    .descriptor = &descriptor_label_index,
    .offset = offsetof(Label_Location_Diff_Patch_Info, target_label_index),
  },
  {
    .name = slice_literal_fields("from"),
    .descriptor = &descriptor_label,
    .offset = offsetof(Label_Location_Diff_Patch_Info, from),
  },
  {
    .name = slice_literal_fields("patch_target"),
    .descriptor = &descriptor_s32_pointer,
    .offset = offsetof(Label_Location_Diff_Patch_Info, patch_target),
  },
);
MASS_DEFINE_TYPE_VALUE(label_location_diff_patch_info);
MASS_DEFINE_OPAQUE_C_TYPE(number_base, Number_Base)
MASS_DEFINE_OPAQUE_C_TYPE(array_number_literal_ptr, Array_Number_Literal_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_number_literal, Array_Number_Literal)
MASS_DEFINE_STRUCT_DESCRIPTOR(number_literal,
  {
    .name = slice_literal_fields("base"),
    .descriptor = &descriptor_number_base,
    .offset = offsetof(Number_Literal, base),
  },
  {
    .name = slice_literal_fields("negative"),
    .descriptor = &descriptor_u32,
    .offset = offsetof(Number_Literal, negative),
  },
  {
    .name = slice_literal_fields("bits"),
    .descriptor = &descriptor_u64,
    .offset = offsetof(Number_Literal, bits),
  },
);
MASS_DEFINE_TYPE_VALUE(number_literal);
MASS_DEFINE_OPAQUE_C_TYPE(array_external_symbol_ptr, Array_External_Symbol_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_external_symbol, Array_External_Symbol)
MASS_DEFINE_STRUCT_DESCRIPTOR(external_symbol,
  {
    .name = slice_literal_fields("library_name"),
    .descriptor = &descriptor_slice,
    .offset = offsetof(External_Symbol, library_name),
  },
  {
    .name = slice_literal_fields("symbol_name"),
    .descriptor = &descriptor_slice,
    .offset = offsetof(External_Symbol, symbol_name),
  },
);
MASS_DEFINE_TYPE_VALUE(external_symbol);
MASS_DEFINE_OPAQUE_C_TYPE(array_import_symbol_ptr, Array_Import_Symbol_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_import_symbol, Array_Import_Symbol)
MASS_DEFINE_STRUCT_DESCRIPTOR(import_symbol,
  {
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .offset = offsetof(Import_Symbol, name),
  },
  {
    .name = slice_literal_fields("label32"),
    .descriptor = &descriptor_label_index,
    .offset = offsetof(Import_Symbol, label32),
  },
);
MASS_DEFINE_TYPE_VALUE(import_symbol);
MASS_DEFINE_OPAQUE_C_TYPE(array_import_library_ptr, Array_Import_Library_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_import_library, Array_Import_Library)
MASS_DEFINE_STRUCT_DESCRIPTOR(import_library,
  {
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .offset = offsetof(Import_Library, name),
  },
  {
    .name = slice_literal_fields("symbols"),
    .descriptor = &descriptor_array_import_symbol,
    .offset = offsetof(Import_Library, symbols),
  },
);
MASS_DEFINE_TYPE_VALUE(import_library);
MASS_DEFINE_OPAQUE_C_TYPE(compare_type, Compare_Type)
MASS_DEFINE_OPAQUE_C_TYPE(array_maybe_register_ptr, Array_Maybe_Register_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_maybe_register, Array_Maybe_Register)
MASS_DEFINE_STRUCT_DESCRIPTOR(maybe_register,
  {
    .name = slice_literal_fields("index"),
    .descriptor = &descriptor_register,
    .offset = offsetof(Maybe_Register, index),
  },
  {
    .name = slice_literal_fields("has_value"),
    .descriptor = &descriptor_u32,
    .offset = offsetof(Maybe_Register, has_value),
  },
);
MASS_DEFINE_TYPE_VALUE(maybe_register);
MASS_DEFINE_OPAQUE_C_TYPE(memory_location, Memory_Location)
MASS_DEFINE_OPAQUE_C_TYPE(static_memory, Static_Memory)
MASS_DEFINE_OPAQUE_C_TYPE(storage, Storage)
MASS_DEFINE_OPAQUE_C_TYPE(array_compiler_source_location_ptr, Array_Compiler_Source_Location_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_compiler_source_location, Array_Compiler_Source_Location)
MASS_DEFINE_STRUCT_DESCRIPTOR(compiler_source_location,
  {
    .name = slice_literal_fields("filename"),
    .descriptor = &descriptor_char_pointer,
    .offset = offsetof(Compiler_Source_Location, filename),
  },
  {
    .name = slice_literal_fields("function_name"),
    .descriptor = &descriptor_char_pointer,
    .offset = offsetof(Compiler_Source_Location, function_name),
  },
  {
    .name = slice_literal_fields("line_number"),
    .descriptor = &descriptor_u64,
    .offset = offsetof(Compiler_Source_Location, line_number),
  },
);
MASS_DEFINE_TYPE_VALUE(compiler_source_location);
MASS_DEFINE_OPAQUE_C_TYPE(operator_fixity, Operator_Fixity)
MASS_DEFINE_OPAQUE_C_TYPE(operator_associativity, Operator_Associativity)
MASS_DEFINE_OPAQUE_C_TYPE(array_execution_context_ptr, Array_Execution_Context_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_execution_context, Array_Execution_Context)
MASS_DEFINE_STRUCT_DESCRIPTOR(execution_context,
  {
    .name = slice_literal_fields("allocator"),
    .descriptor = &descriptor_allocator_pointer,
    .offset = offsetof(Execution_Context, allocator),
  },
  {
    .name = slice_literal_fields("compilation"),
    .descriptor = &descriptor_compilation_pointer,
    .offset = offsetof(Execution_Context, compilation),
  },
  {
    .name = slice_literal_fields("epoch"),
    .descriptor = &descriptor_u64,
    .offset = offsetof(Execution_Context, epoch),
  },
  {
    .name = slice_literal_fields("program"),
    .descriptor = &descriptor_program_pointer,
    .offset = offsetof(Execution_Context, program),
  },
  {
    .name = slice_literal_fields("scope"),
    .descriptor = &descriptor_scope_pointer,
    .offset = offsetof(Execution_Context, scope),
  },
  {
    .name = slice_literal_fields("builder"),
    .descriptor = &descriptor_function_builder_pointer,
    .offset = offsetof(Execution_Context, builder),
  },
  {
    .name = slice_literal_fields("module"),
    .descriptor = &descriptor_module_pointer,
    .offset = offsetof(Execution_Context, module),
  },
  {
    .name = slice_literal_fields("result"),
    .descriptor = &descriptor_mass_result_pointer,
    .offset = offsetof(Execution_Context, result),
  },
);
MASS_DEFINE_TYPE_VALUE(execution_context);
MASS_DEFINE_OPAQUE_C_TYPE(array_operator_ptr, Array_Operator_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_operator, Array_Operator)
MASS_DEFINE_STRUCT_DESCRIPTOR(operator,
  {
    .name = slice_literal_fields("fixity"),
    .descriptor = &descriptor_operator_fixity,
    .offset = offsetof(Operator, fixity),
  },
  {
    .name = slice_literal_fields("associativity"),
    .descriptor = &descriptor_operator_associativity,
    .offset = offsetof(Operator, associativity),
  },
  {
    .name = slice_literal_fields("precedence"),
    .descriptor = &descriptor_u64,
    .offset = offsetof(Operator, precedence),
  },
  {
    .name = slice_literal_fields("argument_count"),
    .descriptor = &descriptor_u64,
    .offset = offsetof(Operator, argument_count),
  },
  {
    .name = slice_literal_fields("handler"),
    .descriptor = &descriptor_mass_handle_operator_proc,
    .offset = offsetof(Operator, handler),
  },
  {
    .name = slice_literal_fields("handler_payload"),
    .descriptor = &descriptor_void_pointer,
    .offset = offsetof(Operator, handler_payload),
  },
);
MASS_DEFINE_TYPE_VALUE(operator);
MASS_DEFINE_OPAQUE_C_TYPE(scope_entry, Scope_Entry)
MASS_DEFINE_OPAQUE_C_TYPE(array_value_ptr, Array_Value_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_value, Array_Value)
MASS_DEFINE_STRUCT_DESCRIPTOR(value,
  {
    .name = slice_literal_fields("descriptor"),
    .descriptor = &descriptor_descriptor_pointer,
    .offset = offsetof(Value, descriptor),
  },
  {
    .name = slice_literal_fields("storage"),
    .descriptor = &descriptor_storage,
    .offset = offsetof(Value, storage),
  },
  {
    .name = slice_literal_fields("next_overload"),
    .descriptor = &descriptor_value_pointer,
    .offset = offsetof(Value, next_overload),
  },
  {
    .name = slice_literal_fields("epoch"),
    .descriptor = &descriptor_u64,
    .offset = offsetof(Value, epoch),
  },
  {
    .name = slice_literal_fields("is_temporary"),
    .descriptor = &descriptor_u64,
    .offset = offsetof(Value, is_temporary),
  },
  {
    .name = slice_literal_fields("source_range"),
    .descriptor = &descriptor_source_range,
    .offset = offsetof(Value, source_range),
  },
  {
    .name = slice_literal_fields("compiler_source_location"),
    .descriptor = &descriptor_compiler_source_location,
    .offset = offsetof(Value, compiler_source_location),
  },
);
MASS_DEFINE_TYPE_VALUE(value);
MASS_DEFINE_OPAQUE_C_TYPE(expected_result_storage, Expected_Result_Storage)
MASS_DEFINE_OPAQUE_C_TYPE(expected_result, Expected_Result)
MASS_DEFINE_OPAQUE_C_TYPE(array_lazy_value_ptr, Array_Lazy_Value_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_lazy_value, Array_Lazy_Value)
MASS_DEFINE_STRUCT_DESCRIPTOR(lazy_value,
  {
    .name = slice_literal_fields("context"),
    .descriptor = &descriptor_execution_context,
    .offset = offsetof(Lazy_Value, context),
  },
  {
    .name = slice_literal_fields("descriptor"),
    .descriptor = &descriptor_descriptor_pointer,
    .offset = offsetof(Lazy_Value, descriptor),
  },
  {
    .name = slice_literal_fields("proc"),
    .descriptor = &descriptor_lazy_value_proc,
    .offset = offsetof(Lazy_Value, proc),
  },
  {
    .name = slice_literal_fields("payload"),
    .descriptor = &descriptor_void_pointer,
    .offset = offsetof(Lazy_Value, payload),
  },
);
MASS_DEFINE_TYPE_VALUE(lazy_value);
MASS_DEFINE_OPAQUE_C_TYPE(descriptor_function_flags, Descriptor_Function_Flags)
MASS_DEFINE_OPAQUE_C_TYPE(array_descriptor_struct_field_ptr, Array_Descriptor_Struct_Field_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_descriptor_struct_field, Array_Descriptor_Struct_Field)
MASS_DEFINE_STRUCT_DESCRIPTOR(descriptor_struct_field,
  {
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .offset = offsetof(Descriptor_Struct_Field, name),
  },
  {
    .name = slice_literal_fields("descriptor"),
    .descriptor = &descriptor_descriptor_pointer,
    .offset = offsetof(Descriptor_Struct_Field, descriptor),
  },
  {
    .name = slice_literal_fields("offset"),
    .descriptor = &descriptor_u64,
    .offset = offsetof(Descriptor_Struct_Field, offset),
  },
);
MASS_DEFINE_TYPE_VALUE(descriptor_struct_field);
MASS_DEFINE_OPAQUE_C_TYPE(array_function_argument_ptr, Array_Function_Argument_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_function_argument, Array_Function_Argument)
MASS_DEFINE_STRUCT_DESCRIPTOR(function_argument,
  {
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .offset = offsetof(Function_Argument, name),
  },
  {
    .name = slice_literal_fields("value"),
    .descriptor = &descriptor_value_pointer,
    .offset = offsetof(Function_Argument, value),
  },
  {
    .name = slice_literal_fields("maybe_default_expression"),
    .descriptor = &descriptor_value_view,
    .offset = offsetof(Function_Argument, maybe_default_expression),
  },
);
MASS_DEFINE_TYPE_VALUE(function_argument);
MASS_DEFINE_OPAQUE_C_TYPE(array_function_return_ptr, Array_Function_Return_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_function_return, Array_Function_Return)
MASS_DEFINE_STRUCT_DESCRIPTOR(function_return,
  {
    .name = slice_literal_fields("name"),
    .descriptor = &descriptor_slice,
    .offset = offsetof(Function_Return, name),
  },
  {
    .name = slice_literal_fields("descriptor"),
    .descriptor = &descriptor_descriptor_pointer,
    .offset = offsetof(Function_Return, descriptor),
  },
);
MASS_DEFINE_TYPE_VALUE(function_return);
MASS_DEFINE_OPAQUE_C_TYPE(array_function_info_ptr, Array_Function_Info_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_function_info, Array_Function_Info)
MASS_DEFINE_STRUCT_DESCRIPTOR(function_info,
  {
    .name = slice_literal_fields("flags"),
    .descriptor = &descriptor_descriptor_function_flags,
    .offset = offsetof(Function_Info, flags),
  },
  {
    .name = slice_literal_fields("_flags_padding"),
    .descriptor = &descriptor_u32,
    .offset = offsetof(Function_Info, _flags_padding),
  },
  {
    .name = slice_literal_fields("arguments"),
    .descriptor = &descriptor_array_function_argument,
    .offset = offsetof(Function_Info, arguments),
  },
  {
    .name = slice_literal_fields("body"),
    .descriptor = &descriptor_value_pointer,
    .offset = offsetof(Function_Info, body),
  },
  {
    .name = slice_literal_fields("scope"),
    .descriptor = &descriptor_scope_pointer,
    .offset = offsetof(Function_Info, scope),
  },
  {
    .name = slice_literal_fields("returns"),
    .descriptor = &descriptor_function_return,
    .offset = offsetof(Function_Info, returns),
  },
);
MASS_DEFINE_TYPE_VALUE(function_info);
MASS_DEFINE_OPAQUE_C_TYPE(descriptor, Descriptor)
MASS_DEFINE_OPAQUE_C_TYPE(mass_result, Mass_Result)
MASS_DEFINE_OPAQUE_C_TYPE(array_slice_ptr, Array_Slice_Ptr)
MASS_DEFINE_OPAQUE_C_TYPE(array_slice, Array_Slice)
MASS_DEFINE_STRUCT_DESCRIPTOR(slice,
  {
    .name = slice_literal_fields("bytes"),
    .descriptor = &descriptor_u8_pointer,
    .offset = offsetof(Slice, bytes),
  },
  {
    .name = slice_literal_fields("length"),
    .descriptor = &descriptor_u64,
    .offset = offsetof(Slice, length),
  },
);
MASS_DEFINE_TYPE_VALUE(slice);

#endif // GENERATED_TYPES_H
