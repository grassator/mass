// Forward declarations

typedef enum Section_Permissions Section_Permissions;

typedef struct Section Section;
typedef dyn_array_type(Section *) Array_Section_Ptr;
typedef dyn_array_type(const Section *) Array_Const_Section_Ptr;

typedef enum Register Register;

typedef enum SIB_Scale SIB_Scale;

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

typedef struct Source_File Source_File;
typedef dyn_array_type(Source_File *) Array_Source_File_Ptr;
typedef dyn_array_type(const Source_File *) Array_Const_Source_File_Ptr;

typedef struct Source_Position Source_Position;
typedef dyn_array_type(Source_Position *) Array_Source_Position_Ptr;
typedef dyn_array_type(const Source_Position *) Array_Const_Source_Position_Ptr;

typedef struct Source_Range Source_Range;
typedef dyn_array_type(Source_Range *) Array_Source_Range_Ptr;
typedef dyn_array_type(const Source_Range *) Array_Const_Source_Range_Ptr;

typedef struct Parse_Error Parse_Error;
typedef dyn_array_type(Parse_Error *) Array_Parse_Error_Ptr;
typedef dyn_array_type(const Parse_Error *) Array_Const_Parse_Error_Ptr;

typedef enum Token_Group_Type Token_Group_Type;

typedef struct Token Token;
typedef dyn_array_type(Token *) Array_Token_Ptr;
typedef dyn_array_type(const Token *) Array_Const_Token_Ptr;

typedef struct Token_View Token_View;
typedef dyn_array_type(Token_View *) Array_Token_View_Ptr;
typedef dyn_array_type(const Token_View *) Array_Const_Token_View_Ptr;

typedef struct Token_Pattern Token_Pattern;
typedef dyn_array_type(Token_Pattern *) Array_Token_Pattern_Ptr;
typedef dyn_array_type(const Token_Pattern *) Array_Const_Token_Pattern_Ptr;

typedef struct Tokenizer_Result Tokenizer_Result;
typedef dyn_array_type(Tokenizer_Result *) Array_Tokenizer_Result_Ptr;
typedef dyn_array_type(const Tokenizer_Result *) Array_Const_Tokenizer_Result_Ptr;

typedef struct Parse_Result Parse_Result;
typedef dyn_array_type(Parse_Result *) Array_Parse_Result_Ptr;
typedef dyn_array_type(const Parse_Result *) Array_Const_Parse_Result_Ptr;

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

typedef enum SIB_Scale {
  SIB_Scale_1 = 0,
  SIB_Scale_2 = 1,
  SIB_Scale_4 = 2,
  SIB_Scale_8 = 3,
} SIB_Scale;

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
} Import_Symbol;
typedef dyn_array_type(Import_Symbol) Array_Import_Symbol;

typedef struct Import_Library {
  Slice name;
  u32 name_rva;
  u32 rva;
  Array_Import_Symbol symbols;
  u32 image_thunk_rva;
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

typedef enum {
  Operand_Tag_None = 0,
  Operand_Tag_Any = 1,
  Operand_Tag_Eflags = 2,
  Operand_Tag_Register = 3,
  Operand_Tag_Xmm = 4,
  Operand_Tag_Immediate_8 = 5,
  Operand_Tag_Immediate_16 = 6,
  Operand_Tag_Immediate_32 = 7,
  Operand_Tag_Immediate_64 = 8,
  Operand_Tag_Memory_Indirect = 9,
  Operand_Tag_Sib = 10,
  Operand_Tag_Import = 11,
  Operand_Tag_Label = 12,
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
  s8 value;
} Operand_Immediate_8;
typedef struct {
  s16 value;
} Operand_Immediate_16;
typedef struct {
  s32 value;
} Operand_Immediate_32;
typedef struct {
  s64 value;
} Operand_Immediate_64;
typedef struct {
  Register reg;
  s32 displacement;
} Operand_Memory_Indirect;
typedef struct {
  SIB_Scale scale;
  Register index;
  Register base;
  s32 displacement;
} Operand_Sib;
typedef struct {
  Slice library_name;
  Slice symbol_name;
} Operand_Import;
typedef struct {
  Label_Index index;
} Operand_Label;
typedef struct Operand {
  Operand_Tag tag;
  u32 byte_size;
  union {
    Operand_Eflags Eflags;
    Operand_Register Register;
    Operand_Xmm Xmm;
    Operand_Immediate_8 Immediate_8;
    Operand_Immediate_16 Immediate_16;
    Operand_Immediate_32 Immediate_32;
    Operand_Immediate_64 Immediate_64;
    Operand_Memory_Indirect Memory_Indirect;
    Operand_Sib Sib;
    Operand_Import Import;
    Operand_Label Label;
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
  Compiler_Source_Location compiler_source_location;
} Value;
typedef dyn_array_type(Value) Array_Value;

typedef enum Descriptor_Function_Flags {
  Descriptor_Function_Flags_None = 0,
  Descriptor_Function_Flags_Inline = 1,
  Descriptor_Function_Flags_Pending_Body_Compilation = 2,
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
  Descriptor_Tag_Tagged_Union = 6,
  Descriptor_Tag_Pointer = 7,
  Descriptor_Tag_Type = 8,
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
  Value * next_overload;
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
  Descriptor_Struct * struct_list;
  s32 struct_count;
} Descriptor_Tagged_Union;
typedef struct {
  Descriptor * to;
} Descriptor_Pointer;
typedef struct {
  Descriptor * descriptor;
} Descriptor_Type;
typedef struct Descriptor {
  Descriptor_Tag tag;
  union {
    Descriptor_Opaque Opaque;
    Descriptor_Function Function;
    Descriptor_Fixed_Size_Array Fixed_Size_Array;
    Descriptor_Struct Struct;
    Descriptor_Tagged_Union Tagged_Union;
    Descriptor_Pointer Pointer;
    Descriptor_Type Type;
  };
} Descriptor;
typedef dyn_array_type(Descriptor) Array_Descriptor;
typedef struct Source_File {
  Slice path;
  Slice text;
  Array_Range_u64 lines;
} Source_File;
typedef dyn_array_type(Source_File) Array_Source_File;

typedef struct Source_Position {
  u64 line;
  u64 column;
} Source_Position;
typedef dyn_array_type(Source_Position) Array_Source_Position;

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

typedef enum Token_Group_Type {
  Token_Group_Type_Paren = 1,
  Token_Group_Type_Square = 2,
  Token_Group_Type_Curly = 3,
} Token_Group_Type;

typedef enum {
  Token_Tag_None = 0,
  Token_Tag_Id = 1,
  Token_Tag_Newline = 2,
  Token_Tag_Integer = 3,
  Token_Tag_Hex_Integer = 4,
  Token_Tag_Operator = 5,
  Token_Tag_Value = 6,
  Token_Tag_String = 7,
  Token_Tag_Group = 8,
} Token_Tag;

typedef struct {
  Value * value;
} Token_Value;
typedef struct {
  Slice slice;
} Token_String;
typedef struct {
  Token_Group_Type type;
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
  Token_Group_Type group_type;
  Slice source;
} Token_Pattern;
typedef dyn_array_type(Token_Pattern) Array_Token_Pattern;

typedef enum {
  Tokenizer_Result_Tag_Success = 0,
  Tokenizer_Result_Tag_Error = 1,
} Tokenizer_Result_Tag;

typedef struct {
  Array_Const_Token_Ptr tokens;
} Tokenizer_Result_Success;
typedef struct {
  Array_Parse_Error errors;
} Tokenizer_Result_Error;
typedef struct Tokenizer_Result {
  Tokenizer_Result_Tag tag;
  union {
    Tokenizer_Result_Success Success;
    Tokenizer_Result_Error Error;
  };
} Tokenizer_Result;
typedef dyn_array_type(Tokenizer_Result) Array_Tokenizer_Result;
typedef enum {
  Parse_Result_Tag_Success = 0,
  Parse_Result_Tag_Error = 1,
} Parse_Result_Tag;

typedef struct {
  Array_Parse_Error errors;
} Parse_Result_Error;
typedef struct Parse_Result {
  Parse_Result_Tag tag;
  union {
    Parse_Result_Error Error;
  };
} Parse_Result;
typedef dyn_array_type(Parse_Result) Array_Parse_Result;
