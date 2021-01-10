// Forward declarations

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

typedef struct Operand Operand;
typedef dyn_array_type(Operand *) Array_Operand_Ptr;
typedef dyn_array_type(const Operand *) Array_Const_Operand_Ptr;

typedef struct Compiler_Source_Location Compiler_Source_Location;
typedef dyn_array_type(Compiler_Source_Location *) Array_Compiler_Source_Location_Ptr;
typedef dyn_array_type(const Compiler_Source_Location *) Array_Const_Compiler_Source_Location_Ptr;

typedef struct Value Value;
typedef dyn_array_type(Value *) Array_Value_Ptr;
typedef dyn_array_type(const Value *) Array_Const_Value_Ptr;

typedef enum Descriptor_Function_Flags Descriptor_Function_Flags;

typedef struct Descriptor_Struct_Field Descriptor_Struct_Field;
typedef dyn_array_type(Descriptor_Struct_Field *) Array_Descriptor_Struct_Field_Ptr;
typedef dyn_array_type(const Descriptor_Struct_Field *) Array_Const_Descriptor_Struct_Field_Ptr;

typedef struct Descriptor Descriptor;
typedef dyn_array_type(Descriptor *) Array_Descriptor_Ptr;
typedef dyn_array_type(const Descriptor *) Array_Const_Descriptor_Ptr;

typedef struct Source_Position Source_Position;
typedef dyn_array_type(Source_Position *) Array_Source_Position_Ptr;
typedef dyn_array_type(const Source_Position *) Array_Const_Source_Position_Ptr;

typedef struct Source_File Source_File;
typedef dyn_array_type(Source_File *) Array_Source_File_Ptr;
typedef dyn_array_type(const Source_File *) Array_Const_Source_File_Ptr;

typedef struct Source_Range Source_Range;
typedef dyn_array_type(Source_Range *) Array_Source_Range_Ptr;
typedef dyn_array_type(const Source_Range *) Array_Const_Source_Range_Ptr;

typedef struct Parse_Error Parse_Error;
typedef dyn_array_type(Parse_Error *) Array_Parse_Error_Ptr;
typedef dyn_array_type(const Parse_Error *) Array_Const_Parse_Error_Ptr;

typedef enum Token_Group_Tag Token_Group_Tag;

typedef struct Token Token;
typedef dyn_array_type(Token *) Array_Token_Ptr;
typedef dyn_array_type(const Token *) Array_Const_Token_Ptr;

typedef struct Token_View Token_View;
typedef dyn_array_type(Token_View *) Array_Token_View_Ptr;
typedef dyn_array_type(const Token_View *) Array_Const_Token_View_Ptr;

typedef struct Token_Pattern Token_Pattern;
typedef dyn_array_type(Token_Pattern *) Array_Token_Pattern_Ptr;
typedef dyn_array_type(const Token_Pattern *) Array_Const_Token_Pattern_Ptr;

typedef struct Mass_Result Mass_Result;
typedef dyn_array_type(Mass_Result *) Array_Mass_Result_Ptr;
typedef dyn_array_type(const Mass_Result *) Array_Const_Mass_Result_Ptr;

typedef void(*fn_type_opaque)();

typedef struct Scope Scope;

typedef struct Function_Builder Function_Builder;


// Type Definitions

typedef enum Section_Permissions {
  Section_Permissions_Read = 1,
  Section_Permissions_Write = 2,
  Section_Permissions_Execute = 4,
} Section_Permissions;

typedef struct Section {
  Bucket_Buffer * buffer;
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

typedef struct Label_Index {
  u64 value;
} Label_Index;
typedef dyn_array_type(Label_Index) Array_Label_Index;

typedef struct Label {
  Section * section;
  u32 offset_in_section;
} Label;
typedef dyn_array_type(Label) Array_Label;

typedef struct Label_Location_Diff_Patch_Info {
  Label_Index target_label_index;
  Label from;
  s32 * patch_target;
} Label_Location_Diff_Patch_Info;
typedef dyn_array_type(Label_Location_Diff_Patch_Info) Array_Label_Location_Diff_Patch_Info;

typedef struct Import_Symbol {
  Slice name;
  u32 name_rva;
  Label_Index label32;
  fn_type_opaque address;
} Import_Symbol;
typedef dyn_array_type(Import_Symbol) Array_Import_Symbol;

typedef struct Import_Library {
  Slice name;
  u32 name_rva;
  u32 rva;
  Array_Import_Symbol symbols;
  u32 image_thunk_rva;
  void * handle;
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

typedef struct Maybe_Register {
  Register index;
  bool has_value;
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
  Maybe_Register maybe_index_register;
  s64 offset;
} Memory_Location_Indirect;
typedef struct Memory_Location {
  Memory_Location_Tag tag;
  union {
    Memory_Location_Instruction_Pointer_Relative Instruction_Pointer_Relative;
    Memory_Location_Indirect Indirect;
  };
} Memory_Location;
typedef dyn_array_type(Memory_Location) Array_Memory_Location;
typedef enum {
  Operand_Tag_None = 0,
  Operand_Tag_Any = 1,
  Operand_Tag_Eflags = 2,
  Operand_Tag_Register = 3,
  Operand_Tag_Xmm = 4,
  Operand_Tag_Immediate = 5,
  Operand_Tag_Memory = 6,
} Operand_Tag;

typedef struct {
  Compare_Type compare_type;
} Operand_Eflags;
typedef struct {
  Register index;
} Operand_Register;
typedef struct {
  Register index;
} Operand_Xmm;
typedef struct {
  void * memory;
} Operand_Immediate;
typedef struct {
  Memory_Location location;
} Operand_Memory;
typedef struct Operand {
  Operand_Tag tag;
  u32 byte_size;
  union {
    Operand_Eflags Eflags;
    Operand_Register Register;
    Operand_Xmm Xmm;
    Operand_Immediate Immediate;
    Operand_Memory Memory;
  };
} Operand;
typedef dyn_array_type(Operand) Array_Operand;
typedef struct Compiler_Source_Location {
  const char * filename;
  const char * function_name;
  u32 line_number;
} Compiler_Source_Location;
typedef dyn_array_type(Compiler_Source_Location) Array_Compiler_Source_Location;

typedef struct Value {
  Descriptor * descriptor;
  Operand operand;
  Value * next_overload;
  Compiler_Source_Location compiler_source_location;
} Value;
typedef dyn_array_type(Value) Array_Value;

typedef enum Descriptor_Function_Flags {
  Descriptor_Function_Flags_None = 0,
  Descriptor_Function_Flags_Pending_Body_Compilation = 1,
  Descriptor_Function_Flags_Macro = 2,
  Descriptor_Function_Flags_External = 4,
  Descriptor_Function_Flags_No_Own_Scope = 8,
} Descriptor_Function_Flags;

typedef struct Descriptor_Struct_Field {
  Slice name;
  Descriptor * descriptor;
  s32 offset;
} Descriptor_Struct_Field;
typedef dyn_array_type(Descriptor_Struct_Field) Array_Descriptor_Struct_Field;

typedef enum {
  Descriptor_Tag_Void = 0,
  Descriptor_Tag_Any = 1,
  Descriptor_Tag_Opaque = 2,
  Descriptor_Tag_Function = 3,
  Descriptor_Tag_Fixed_Size_Array = 4,
  Descriptor_Tag_Struct = 5,
  Descriptor_Tag_Pointer = 6,
} Descriptor_Tag;

typedef struct {
  u64 bit_size;
} Descriptor_Opaque;
typedef struct {
  Descriptor_Function_Flags flags;
  Array_Value_Ptr arguments;
  Array_Slice argument_names;
  const Token * body;
  Scope * scope;
  Function_Builder * builder;
  Value * returns;
} Descriptor_Function;
typedef struct {
  Descriptor * item;
  u32 length;
} Descriptor_Fixed_Size_Array;
typedef struct {
  Slice name;
  Array_Descriptor_Struct_Field fields;
} Descriptor_Struct;
typedef struct {
  Descriptor * to;
} Descriptor_Pointer;
typedef struct Descriptor {
  Descriptor_Tag tag;
  union {
    Descriptor_Opaque Opaque;
    Descriptor_Function Function;
    Descriptor_Fixed_Size_Array Fixed_Size_Array;
    Descriptor_Struct Struct;
    Descriptor_Pointer Pointer;
  };
} Descriptor;
typedef dyn_array_type(Descriptor) Array_Descriptor;
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

typedef struct Parse_Error {
  Slice message;
  Source_Range source_range;
} Parse_Error;
typedef dyn_array_type(Parse_Error) Array_Parse_Error;

typedef enum Token_Group_Tag {
  Token_Group_Tag_Paren = 1,
  Token_Group_Tag_Square = 2,
  Token_Group_Tag_Curly = 3,
} Token_Group_Tag;

typedef enum {
  Token_Tag_None = 0,
  Token_Tag_Id = 1,
  Token_Tag_Operator = 2,
  Token_Tag_Value = 3,
  Token_Tag_String = 4,
  Token_Tag_Group = 5,
} Token_Tag;

typedef struct {
  Value * value;
} Token_Value;
typedef struct {
  Slice slice;
} Token_String;
typedef struct {
  Token_Group_Tag tag;
  Array_Const_Token_Ptr children;
} Token_Group;
typedef struct Token {
  Token_Tag tag;
  Source_Range source_range;
  Slice source;
  union {
    Token_Value Value;
    Token_String String;
    Token_Group Group;
  };
} Token;
typedef dyn_array_type(Token) Array_Token;
typedef struct Token_View {
  const Token ** tokens;
  u64 length;
} Token_View;
typedef dyn_array_type(Token_View) Array_Token_View;

typedef struct Token_Pattern {
  Token_Tag tag;
  Token_Group_Tag group_tag;
  Slice source;
  const Token_Pattern * or;
} Token_Pattern;
typedef dyn_array_type(Token_Pattern) Array_Token_Pattern;

typedef enum {
  Mass_Result_Tag_Success = 0,
  Mass_Result_Tag_Error = 1,
} Mass_Result_Tag;

typedef struct {
  Parse_Error details;
} Mass_Result_Error;
typedef struct Mass_Result {
  Mass_Result_Tag tag;
  union {
    Mass_Result_Error Error;
  };
} Mass_Result;
typedef dyn_array_type(Mass_Result) Array_Mass_Result;
