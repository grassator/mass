#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>

#ifndef countof
#define countof(...)\
  (sizeof(__VA_ARGS__) / sizeof((__VA_ARGS__)[0]))
#endif

typedef struct {
  const char *name;
  int32_t value;
} Enum_Item;

typedef struct {
  const char *name;
  Enum_Item *items;
  uint64_t item_count;
} Enum;

typedef struct {
  const char *type;
  const char *name;
} Struct_Item;

typedef struct {
  const char *name;
  Struct_Item *items;
  uint64_t item_count;
} Struct;

typedef struct {
  const char *name;
  Struct *common;
  Struct *items;
  uint64_t item_count;
} Tagged_Union;

typedef struct Type Type;

typedef struct {
  const char *name;
  Type *type;
} Argument;

typedef struct {
  const char *name;
  Argument *arguments;
  uint64_t argument_count;
  Type *returns;
} Function;

typedef enum {
  Type_Tag_Struct,
  Type_Tag_Tagged_Union,
  Type_Tag_Enum,
  Type_Tag_Function,
} Type_Tag;

typedef struct Type {
  Type_Tag tag;
  union {
    Struct struct_;
    Enum enum_;
    Tagged_Union union_;
    Function function;
  };
} Type;

void
print_c_type_forward_declaration(
  FILE *file,
  Type *type
) {
  switch(type->tag) {
    case Type_Tag_Struct: {
      fprintf(file, "typedef struct %s %s;\n", type->struct_.name, type->struct_.name);
      fprintf(file, "typedef dyn_array_type(%s *) Array_%s_Ptr;\n",
        type->struct_.name, type->struct_.name);
      fprintf(file, "typedef dyn_array_type(const %s *) Array_Const_%s_Ptr;\n",
        type->struct_.name, type->struct_.name);
      break;
    }
    case Type_Tag_Enum: {
      fprintf(file, "typedef enum %s %s;\n", type->enum_.name, type->enum_.name);
      break;
    }
    case Type_Tag_Tagged_Union: {
      fprintf(file, "typedef struct %s %s;\n", type->union_.name, type->union_.name);
      fprintf(file, "typedef dyn_array_type(%s *) Array_%s_Ptr;\n",
        type->union_.name, type->union_.name);
      fprintf(file, "typedef dyn_array_type(const %s *) Array_Const_%s_Ptr;\n",
        type->union_.name, type->union_.name);
      break;
    }
    case Type_Tag_Function: {
      // Nothing to do
      break;
    }
  }
  fprintf(file, "\n");
}

void
print_c_type(
  FILE *file,
  Type *type
) {
  switch(type->tag) {
    case Type_Tag_Struct: {
      fprintf(file, "typedef struct %s {\n", type->struct_.name);
      for (uint64_t i = 0; i < type->struct_.item_count; ++i) {
        Struct_Item *item = &type->struct_.items[i];
        fprintf(file, "  %s %s;\n", item->type, item->name);
      }
      fprintf(file, "} %s;\n", type->struct_.name);
      fprintf(file, "typedef dyn_array_type(%s) Array_%s;\n\n",
        type->struct_.name, type->struct_.name);
      break;
    }
    case Type_Tag_Enum: {
      fprintf(file, "typedef enum %s {\n", type->enum_.name);
      for (uint64_t i = 0; i < type->enum_.item_count; ++i) {
        Enum_Item *item = &type->enum_.items[i];
        fprintf(file, "  %s_%s = %d,\n", type->enum_.name, item->name, item->value);
      }
      fprintf(file, "} %s;\n\n", type->enum_.name);
      break;
    }
    case Type_Tag_Tagged_Union: {
      // Write out the enum
      {
        fprintf(file, "typedef enum {\n");
        for (uint64_t i = 0; i < type->union_.item_count; ++i) {
          Struct *item = &type->union_.items[i];
          fprintf(file, "  %s_Tag_%s = %llu,\n", type->union_.name, item->name, i);
        }
        fprintf(file, "} %s_Tag;\n\n", type->union_.name);
      }

      // Write out individual structs
      {
        for (uint64_t i = 0; i < type->union_.item_count; ++i) {
          Struct *struct_ = &type->union_.items[i];
          if (struct_->item_count) {
            fprintf(file, "typedef struct {\n");
            for (uint64_t i = 0; i < struct_->item_count; ++i) {
              Struct_Item *item = &struct_->items[i];
              fprintf(file, "  %s %s;\n", item->type, item->name);
            }
            fprintf(file, "} %s_%s;\n", type->union_.name, struct_->name);
          }
        }
      }

      // Write out the tagged union struct
      {
        fprintf(file, "typedef struct %s {\n", type->union_.name);
        fprintf(file, "  %s_Tag tag;\n", type->union_.name);
        if (type->union_.common) {
          for (uint64_t i = 0; i < type->union_.common->item_count; ++i) {
            Struct_Item *item = &type->union_.common->items[i];
            fprintf(file, "  %s %s;\n", item->type, item->name);
          }
        }
        fprintf(file, "  union {\n");
        for (uint64_t i = 0; i < type->union_.item_count; ++i) {
          Struct *struct_ = &type->union_.items[i];
          if (struct_->item_count) {
            fprintf(file, "    %s_%s %s;\n",
              type->union_.name, struct_->name, struct_->name);
          }
        }
        fprintf(file, "  };\n");
        fprintf(file, "} %s;\n", type->union_.name);
        fprintf(file, "typedef dyn_array_type(%s) Array_%s;\n",
          type->union_.name, type->union_.name);
      }
      break;
    }
    case Type_Tag_Function: {
      assert(!"Not implemented");
      break;
    }
  }
}

void
print_mass_type(
  FILE *file,
  Type *type
) {
  switch(type->tag) {
    case Type_Tag_Struct: {
      fprintf(file, "Mass_%s :: struct {\n", type->struct_.name);
      for (uint64_t i = 0; i < type->struct_.item_count; ++i) {
        Struct_Item *item = &type->struct_.items[i];
        fprintf(file, "  %s : %s\n", item->name, item->type);
      }
      fprintf(file, "}\n\n");
      break;
    }
    case Type_Tag_Enum: {
      for (uint64_t i = 0; i < type->enum_.item_count; ++i) {
        Enum_Item *item = &type->enum_.items[i];
        fprintf(file, "Mass_%s_%s :: %d\n", type->enum_.name, item->name, item->value);
      }
      fprintf(file, "\n");
      break;
    }
    case Type_Tag_Tagged_Union: {
      // TODO
      break;
    }
    case Type_Tag_Function: {
      assert(!"Not implemented");
      break;
    }
  }
}

#define struct_empty(_NAME_STRING_)\
  {\
    .name = _NAME_STRING_,\
    .items = 0,\
    .item_count = 0,\
  }

#define struct_fields(_NAME_STRING_, ...)\
  {\
    .name = _NAME_STRING_,\
    .items = (__VA_ARGS__),\
    .item_count = countof(__VA_ARGS__),\
  }

#define type_enum(_NAME_STRING_, ...)\
  &(Type){\
    .tag = Type_Tag_Enum,\
    .enum_ = {\
      .name = _NAME_STRING_,\
      .items = (__VA_ARGS__),\
      .item_count = countof(__VA_ARGS__),\
    }\
  }

#define type_union(_NAME_STRING_, ...)\
  &(Type){\
    .tag = Type_Tag_Tagged_Union,\
    .union_ = {\
      .name = _NAME_STRING_,\
      .items = (__VA_ARGS__),\
      .item_count = countof(__VA_ARGS__),\
    }\
  }

static inline Type *
add_common_fields_internal(
  Type *type,
  Struct *common
) {
  assert(type->tag == Type_Tag_Tagged_Union);
  type->union_.common = common;
  return type;
}

#define add_common_fields(_TYPE_, ...)\
  add_common_fields_internal(\
    _TYPE_,\
    &(Struct)struct_fields("", __VA_ARGS__)\
  )

#define type_struct(_NAME_STRING_, ...)\
  &(Type){\
    .tag = Type_Tag_Struct,\
    .struct_ = struct_fields(_NAME_STRING_, __VA_ARGS__)\
  }

int
main(void) {

  Type *types[4096];
  uint32_t type_count = 0;

  #define push_type(_TYPE_)\
    do {\
      if (type_count >= countof(types)) {\
        fprintf(stderr, "Too many types defined");\
      } else {\
        types[type_count++] = (_TYPE_);\
      }\
    } while(0);

  push_type(type_enum("Section_Permissions", (Enum_Item[]){
    { "Read",    1 << 0 },
    { "Write",   1 << 1 },
    { "Execute", 1 << 2 },
  }));

  push_type(type_struct("Section", (Struct_Item[]){
    { "Bucket_Buffer *", "buffer" },
    { "Slice", "name" },
    { "u32", "base_rva" },
    { "Section_Permissions", "permissions" },
  }));

  push_type(type_enum("Register", (Enum_Item[]){
    { "A", 0b0000 },
    { "C", 0b0001 },
    { "D", 0b0010 },
    { "B", 0b0011 },

    { "SP", 0b0100 },
    { "AH", 0b0100 },

    { "BP", 0b0101 },
    { "CH", 0b0100 },

    { "SI", 0b0110 },
    { "DH", 0b0100 },

    { "DI", 0b0111 },
    { "BH", 0b0100 },

    { "R8", 0b1000 },
    { "R9", 0b1001 },
    { "R10", 0b1010 },
    { "R11", 0b1011 },
    { "R12", 0b1100 },
    { "R13", 0b1101 },
    { "R14", 0b1110 },
    { "R15", 0b1111 },

    { "Xmm0", 0b10000 },
    { "Xmm1", 0b10001 },
    { "Xmm2", 0b10010 },
    { "Xmm3", 0b10011 },
    { "Xmm4", 0b10100 },
    { "Xmm5", 0b10101 },
    { "Xmm6", 0b10110 },
    { "Xmm7", 0b10111 },

    { "Xmm8", 0b11000 },
    { "Xmm9", 0b11001 },
    { "Xmm10", 0b11010 },
    { "Xmm11", 0b11011 },
    { "Xmm12", 0b11100 },
    { "Xmm13", 0b11101 },
    { "Xmm14", 0b11110 },
    { "Xmm15", 0b11111 },
  }));

  push_type(type_enum("SIB_Scale", (Enum_Item[]){
    { "1", 0b00 },
    { "2", 0b01 },
    { "4", 0b10 },
    { "8", 0b11 },
  }));

  push_type(type_struct("Label_Index", (Struct_Item[]){
    { "u64", "value" },
  }));

  push_type(type_struct("Label", (Struct_Item[]){
    { "Section *", "section" },
    { "u32", "offset_in_section" },
  }));

  push_type(type_struct("Label_Location_Diff_Patch_Info", (Struct_Item[]){
    { "Label_Index", "target_label_index" },
    { "Label", "from" },
    { "s32 *", "patch_target" },
  }));

  push_type(type_struct("Import_Symbol", (Struct_Item[]){
    { "Slice", "name" },
    { "u32", "name_rva" },
    { "Label_Index", "label32" },
  }));

  push_type(type_struct("Import_Library", (Struct_Item[]){
    { "Slice", "name" },
    { "u32", "name_rva" },
    { "u32", "rva" },
    { "Array_Import_Symbol", "symbols" },
    { "u32", "image_thunk_rva" },
  }));

  {
    int32_t value = 1;
    Enum_Item items[] = {
      { "Equal", value++ },
      { "Not_Equal", value++ },

      { "Unsigned_Below", value++ },
      { "Unsigned_Below_Equal", value++ },

      { "Unsigned_Above", value++ },
      { "Unsigned_Above_Equal", value++ },

      { "Signed_Less", value++ },
      { "Signed_Less_Equal", value++ },

      { "Signed_Greater", value++ },
      { "Signed_Greater_Equal", value++ },
    };
    push_type(type_enum("Compare_Type", items));
  }

  push_type(add_common_fields(type_union("Operand", (Struct[]){
    struct_empty("None"),
    struct_empty("Any"),
    struct_fields("Eflags", (Struct_Item[]){
      { "Compare_Type", "compare_type" },
    }),
    struct_fields("Register", (Struct_Item[]){
      { "Register", "index" },
    }),
    struct_fields("Xmm", (Struct_Item[]){
      { "Register", "index" },
    }),
    struct_fields("Immediate_8", (Struct_Item[]){
      { "s8", "value" },
    }),
    struct_fields("Immediate_16", (Struct_Item[]){
      { "s16", "value" },
    }),
    struct_fields("Immediate_32", (Struct_Item[]){
      { "s32", "value" },
    }),
    struct_fields("Immediate_64", (Struct_Item[]){
      { "s64", "value" },
    }),
    struct_fields("Memory_Indirect", (Struct_Item[]){
      { "Register", "reg" },
      { "s32", "displacement" },
    }),
    struct_fields("Sib", (Struct_Item[]){
      { "SIB_Scale", "scale" },
      { "Register", "index" },
      { "Register", "base" },
      { "s32", "displacement" },
    }),
    struct_fields("Import", (Struct_Item[]){
      { "Slice", "library_name" },
      { "Slice", "symbol_name" },
    }),
    struct_fields("Label", (Struct_Item[]){
      { "Label_Index", "index" },
    }),
  }), (Struct_Item[]){
    { "u32", "byte_size" },
  }));

  push_type(type_struct("Compiler_Source_Location", (Struct_Item[]){
    { "const char *", "filename" },
    { "const char *", "function_name" },
    { "u32", "line_number" },
  }));

  //push_type(type_struct("Value", (Struct_Item[]){
    //{ "Descriptor *", "descriptor" },
    //{ "Operand", "operand" },
    //{ "Compiler_Source_Location", "compiler_source_location" },
  //}));

  push_type(type_enum("Descriptor_Function_Flags", (Enum_Item[]){
    { "None",   0 },
    { "Inline", 1 << 0 },
    { "Pending_Body_Compilation", 1 << 1 },
  }));

  push_type(type_struct("Descriptor_Struct_Field", (Struct_Item[]){
    { "Slice", "name" },
    { "Descriptor *", "descriptor" },
    { "s32", "offset" },
  }));

  push_type(type_union("Descriptor", (Struct[]){
    struct_empty("Void"),
    struct_empty("Any"),
    struct_fields("Opaque", (Struct_Item[]){
      { "u64", "bit_size" },
    }),
    struct_fields("Function", (Struct_Item[]){
      { "Descriptor_Function_Flags", "flags" },
      { "Array_Value_Ptr", "arguments" },
      { "Array_Slice", "argument_names" },
      { "const Token *", "body" },
      { "Scope *", "scope" },
      { "Function_Builder *", "builder" },

      { "Value *", "returns" },
      { "Value *", "next_overload" },
    }),
    struct_fields("Fixed_Size_Array", (Struct_Item[]){
      { "Descriptor *", "item" },
      { "u32", "length" },
    }),
    struct_fields("Struct", (Struct_Item[]){
      { "Slice", "name" },
      { "Array_Descriptor_Struct_Field", "fields" },
    }),
    struct_fields("Tagged_Union", (Struct_Item[]){
      { "Descriptor_Struct *", "struct_list" },
      { "s32", "struct_count" },
    }),
    struct_fields("Pointer", (Struct_Item[]){
      { "Descriptor *", "to" },
    }),
    struct_fields("Type", (Struct_Item[]){
      { "Descriptor *", "descriptor" },
    }),
  }));

  push_type(type_struct("Source_File", (Struct_Item[]){
    { "Slice", "path" },
    { "Slice", "text" },
    { "Array_Range_u64", "lines" },
  }));

  push_type(type_struct("Source_Position", (Struct_Item[]){
    { "u64", "line" },
    { "u64", "column" },
  }));

  push_type(type_struct("Source_Range", (Struct_Item[]){
    { "const Source_File *", "file" },
    { "Range_u64", "offsets" },
  }));

  push_type(type_struct("Parse_Error", (Struct_Item[]){
    { "Slice", "message" },
    { "Source_Range", "source_range" },
  }));

  {
    int32_t value = 1;
    Enum_Item items[] = {
      { "Paren", value++ },
      { "Square", value++ },
      { "Curly", value++ },
    };
    push_type(type_enum("Token_Group_Type", items));
  }

  push_type(add_common_fields(type_union("Token", (Struct[]){
    struct_empty("None"),
    struct_empty("Id"),
    struct_empty("Newline"),
    struct_empty("Integer"),
    struct_empty("Hex_Integer"),
    struct_empty("Operator"),
    struct_fields("Value", (Struct_Item[]){
      { "Value *", "value" },
    }),
    struct_fields("String", (Struct_Item[]){
      { "Slice", "slice" },
    }),
    struct_fields("Group", (Struct_Item[]){
      { "Token_Group_Type", "type" },
      { "Array_Const_Token_Ptr", "children" },
    }),
  }), (Struct_Item[]){
    { "Source_Range", "source_range" },
    { "Slice", "source" },
  }));

  push_type(type_struct("Token_View", (Struct_Item[]){
    { "const Token **", "tokens" },
    { "u64", "length" },
  }));

  push_type(type_struct("Token_Pattern", (Struct_Item[]){
    { "Token_Tag", "tag" },
    { "Token_Group_Type", "group_type" },
    { "Slice", "source" },
  }));

  push_type(type_union("Tokenizer_Result", (Struct[]){
    struct_fields("Success", (Struct_Item[]){
      { "Array_Const_Token_Ptr", "tokens" },
    }),
    struct_fields("Error", (Struct_Item[]){
      { "Array_Parse_Error", "errors" },
    })
  }));

  push_type(type_union("Parse_Result", (Struct[]){
    struct_empty("Success"),
    struct_fields("Error", (Struct_Item[]){
      { "Array_Parse_Error", "errors" },
    })
  }));

  {
    const char *filename = "..\\types.h";
    #pragma warning(disable : 4996)
    FILE *file = fopen(filename, "w");
    if (!file) exit(1);

    fprintf(file, "// Forward declarations\n\n");
    for (uint32_t i = 0; i < type_count; ++i) {
      print_c_type_forward_declaration(file, types[i]);
    }
    // Custom forward declarations
    // TODO would be great to not have these
    {
      fprintf(file, "typedef struct Value Value;\n\n");
      fprintf(file, "typedef dyn_array_type(Value *) Array_Value_Ptr;\n\n");
      fprintf(file, "typedef struct Scope Scope;\n\n");
      fprintf(file, "typedef struct Function_Builder Function_Builder;\n\n");
    }

    fprintf(file, "\n// Type Definitions\n\n");

    for (uint32_t i = 0; i < type_count; ++i) {
      print_c_type(file, types[i]);
    }

    fclose(file);
    printf("C Types Generated at: %s\n", filename);
  }
  {
    const char *filename = "..\\lib\\compiler_types.mass";
    #pragma warning(disable : 4996)
    FILE *file = fopen(filename, "w");
    if (!file) exit(1);

    for (uint32_t i = 0; i < type_count; ++i) {
      print_mass_type(file, types[i]);
    }

    fclose(file);
    printf("Mass Types Generated at: %s\n", filename);
  }
  return 0;
}