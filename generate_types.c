#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h>
#include <assert.h>

#ifndef countof
#define countof(...)\
  ((sizeof(__VA_ARGS__)) / (sizeof((__VA_ARGS__)[0])))
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
  Struct common;
  Struct *items;
  uint64_t item_count;
} Tagged_Union;

typedef struct Type Type;

typedef struct {
  const char *type;
  const char *name;
} Argument;

typedef struct {
  const char *name;
  const char *returns;
  Argument *arguments;
  uint64_t argument_count;
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
      fprintf(file, "typedef %s (*%s)\n  (", type->function.returns, type->function.name);
      for (uint64_t i = 0; i < type->function.argument_count; ++i) {
        Argument *arg = &type->function.arguments[i];
        if (i != 0) fprintf(file, ", ");
        fprintf(file, "%s %s", arg->type, arg->name);
      }
      fprintf(file, ");\n");
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
          fprintf(file, "  %s_Tag_%s = %" PRIu64  ",\n", type->union_.name, item->name, i);
        }
        fprintf(file, "} %s_Tag;\n\n", type->union_.name);
      }

      // Write out individual structs
      {
        for (uint64_t i = 0; i < type->union_.item_count; ++i) {
          Struct *struct_ = &type->union_.items[i];
          if (struct_->item_count) {
            fprintf(file, "typedef struct {\n");
            for (uint64_t item_index = 0; item_index < struct_->item_count; ++item_index) {
              Struct_Item *item = &struct_->items[item_index];
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
        for (uint64_t i = 0; i < type->union_.common.item_count; ++i) {
          Struct_Item *item = &type->union_.common.items[i];
          fprintf(file, "  %s %s;\n", item->type, item->name);
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
      // We only need a forward declaration so nothing to do here
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
  (Type){\
    .tag = Type_Tag_Enum,\
    .enum_ = {\
      .name = _NAME_STRING_,\
      .items = (__VA_ARGS__),\
      .item_count = countof(__VA_ARGS__),\
    }\
  }

#define type_union(_NAME_STRING_, ...)\
  (Type){\
    .tag = Type_Tag_Tagged_Union,\
    .union_ = {\
      .name = _NAME_STRING_,\
      .items = (__VA_ARGS__),\
      .item_count = countof(__VA_ARGS__),\
    }\
  }

static inline Type
add_common_fields_internal(
  Type type,
  Struct common
) {
  assert(type.tag == Type_Tag_Tagged_Union);
  type.union_.common = common;
  return type;
}

#define add_common_fields(_TYPE_, ...)\
  add_common_fields_internal(\
    _TYPE_,\
    (Struct)struct_fields("", __VA_ARGS__)\
  )

#define type_struct(_NAME_STRING_, ...)\
  (Type){\
    .tag = Type_Tag_Struct,\
    .struct_ = struct_fields(_NAME_STRING_, __VA_ARGS__)\
  }

#define type_function(_NAME_STRING_, _RETURNS_, ...)\
  (Type){\
    .tag = Type_Tag_Function,\
    .function = {\
      .name = (_NAME_STRING_),\
      .returns = (_RETURNS_),\
      .arguments = (__VA_ARGS__),\
      .argument_count = countof(__VA_ARGS__),\
    }\
  }


Type types[4096] = {0};
uint32_t type_count = 0;

static inline void
push_type(
  Type type
) {
  if (type_count >= countof(types)) {
    fprintf(stderr, "Too many types defined");
    exit(1);
  }
  types[type_count++] = type;
}

int
main(void) {

  push_type(type_struct("Source_Position", (Struct_Item[]){
    { "u64", "line" },
    { "u64", "column" },
  }));

  push_type(type_struct("Source_File", (Struct_Item[]){
    { "Slice", "path" },
    { "Slice", "text" },
    { "Array_Range_u64", "line_ranges" },
  }));

  push_type(type_struct("Source_Range", (Struct_Item[]){
    { "const Source_File *", "file" },
    { "Range_u64", "offsets" },
  }));

  push_type(type_struct("Module", (Struct_Item[]){
    { "Source_File", "source_file" },
    { "Source_Range", "source_range" },
    { "Scope *", "scope" },
  }));

  push_type(type_struct("Parse_Error", (Struct_Item[]){
    { "Slice", "message" },
    { "Source_Range", "source_range" },
  }));

  push_type(type_enum("Token_Group_Tag", (Enum_Item[]){
    { "Paren", 1 },
    { "Square", 2 },
    { "Curly", 3 },
  }));

  push_type(add_common_fields(type_union("Token", (Struct[]){
    struct_empty("None"),
    struct_empty("Id"),
    struct_empty("Operator"),
    struct_fields("Value", (Struct_Item[]){
      { "Value *", "value" },
    }),
    struct_fields("String", (Struct_Item[]){
      { "Slice", "slice" },
    }),
    struct_fields("Group", (Struct_Item[]){
      { "Token_Group_Tag", "tag" },
      { "Array_Const_Token_Ptr", "children" },
    }),
  }), (Struct_Item[]){
    { "Source_Range", "source_range" },
    { "Slice", "source" },
  }));

  push_type(type_struct("Token_View", (Struct_Item[]){
    { "const Token **", "tokens" },
    { "u64", "length" },
    { "Source_Range", "source_range" },
  }));

  push_type(type_struct("Token_Pattern", (Struct_Item[]){
    { "Token_Tag", "tag" },
    { "Token_Group_Tag", "group_tag" },
    { "Slice", "source" },
    { "const Token_Pattern *", "or" },
  }));

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

  push_type(type_struct("Label_Index", (Struct_Item[]){
    { "u64", "value" },
  }));

  push_type(type_struct("Label", (Struct_Item[]){
    { "bool", "resolved" },
    { "Slice", "name" },
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
    { "Label_Index", "label32" },
  }));

  push_type(type_struct("Import_Library", (Struct_Item[]){
    { "Slice", "name" },
    { "Array_Import_Symbol", "symbols" },
  }));

  push_type(type_enum("Compare_Type", (Enum_Item[]){
    { "Equal", 1 },
    { "Not_Equal", 2 },

    { "Unsigned_Below", 3 },
    { "Unsigned_Below_Equal", 4 },

    { "Unsigned_Above", 5 },
    { "Unsigned_Above_Equal", 6 },

    { "Signed_Less", 7 },
    { "Signed_Less_Equal", 8 },

    { "Signed_Greater", 9 },
    { "Signed_Greater_Equal", 10 },
  }));

  push_type(type_struct("Maybe_Register", (Struct_Item[]){
    { "Register", "index" },
    { "bool", "has_value" },
  }));

  push_type(type_union("Memory_Location", (Struct[]){
    struct_fields("Instruction_Pointer_Relative", (Struct_Item[]){
      { "Label_Index", "label_index" },
    }),
    struct_fields("Indirect", (Struct_Item[]){
      { "Register", "base_register" },
      { "Maybe_Register", "maybe_index_register" },
      { "s64", "offset" },
    }),
  }));

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
    struct_fields("Immediate", (Struct_Item[]){
      { "void *", "memory" },
    }),
    struct_fields("Memory", (Struct_Item[]){
      { "Memory_Location", "location" },
    }),
  }), (Struct_Item[]){
    // TODO change to bit_size
    { "u32", "byte_size" },
  }));

  push_type(type_struct("Compiler_Source_Location", (Struct_Item[]){
    { "const char *", "filename" },
    { "const char *", "function_name" },
    { "u32", "line_number" },
  }));

  push_type(type_enum("Operator_Fixity", (Enum_Item[]){
    { "Infix", 0 },
    { "Prefix", 1 << 0 },
    { "Postfix", 1 << 1 },
  }));

  push_type(add_common_fields(type_union("Scope_Entry", (Struct[]){
    struct_fields("Value", (Struct_Item[]){
      { "Value *", "value" },
    }),
    struct_fields("Lazy_Expression", (Struct_Item[]){
      { "Scope *", "scope" },
      { "Token_View", "tokens" },
    }),
    struct_fields("Operator", (Struct_Item[]){
      { "Operator_Fixity", "fixity" },
      { "u64", "precedence" },
      { "u64", "argument_count" },
      { "Token_Handle_Operator_Proc", "handler" },
      { "void *", "handler_payload" },
    }),
  }), (Struct_Item[]){
    { "Scope_Entry *", "next_overload" },
  }));

  push_type(type_struct("Value", (Struct_Item[]){
    { "Descriptor *", "descriptor" },
    { "Operand", "operand" },
    { "Value *", "next_overload" },
    { "Compiler_Source_Location", "compiler_source_location" },
  }));

  push_type(type_enum("Descriptor_Function_Flags", (Enum_Item[]){
    { "None", 0 },
    { "Macro", 1 << 0 },
    { "No_Own_Scope", 1 << 1 },
    { "No_Own_Return", 1 << 2 },
  }));

  push_type(type_struct("Descriptor_Struct_Field", (Struct_Item[]){
    { "Slice", "name" },
    { "Descriptor *", "descriptor" },
    { "s32", "offset" },
  }));

  push_type(type_union("Function_Argument", (Struct[]){
    struct_fields("Any_Of_Type", (Struct_Item[]){
      { "Slice", "name" },
      { "Descriptor *", "descriptor" },
    }),
    struct_fields("Exact", (Struct_Item[]){
      { "Descriptor *", "descriptor" },
      { "Operand", "operand" },
    }),
  }));

  push_type(type_struct("Function_Return", (Struct_Item[]){
    { "Slice", "name" },
    { "Descriptor *", "descriptor" },
  }));

  push_type(type_union("Descriptor", (Struct[]){
    struct_empty("Void"),
    struct_empty("Any"),
    struct_fields("Opaque", (Struct_Item[]){
      { "u64", "bit_size" },
    }),
    struct_fields("Function", (Struct_Item[]){
      { "Descriptor_Function_Flags", "flags" },
      { "Array_Function_Argument", "arguments" },
      { "const Token *", "body" },
      { "Scope *", "scope" },
      { "Function_Return", "returns" },
    }),
    struct_fields("Fixed_Size_Array", (Struct_Item[]){
      { "Descriptor *", "item" },
      { "u32", "length" },
    }),
    struct_fields("Struct", (Struct_Item[]){
      { "Slice", "name" },
      { "Array_Descriptor_Struct_Field", "fields" },
    }),
    struct_fields("Pointer", (Struct_Item[]){
      { "Descriptor *", "to" },
    }),
  }));

  push_type(type_union("Mass_Result", (Struct[]){
    struct_empty("Success"),
    struct_fields("Error", (Struct_Item[]){
      { "Parse_Error", "details" },
    })
  }));

  push_type(type_function("Token_Handle_Operator_Proc", "void", (Argument[]){
    { "Compilation_Context *", "context" },
    { "Token_View", "view" },
    { "Value *", "result_value" },
    { "void *", "payload" },
  }));

  {
    const char *filename = "../types.h";
    #pragma warning(disable : 4996)
    FILE *file = fopen(filename, "wb");
    if (!file) exit(1);

    // Custom forward declarations
    {
      fprintf(file, "typedef void(*fn_type_opaque)();\n\n");
      fprintf(file, "typedef struct Scope Scope;\n\n");
      fprintf(file, "typedef struct Function_Builder Function_Builder;\n\n");
      fprintf(file, "typedef struct Compilation_Context Compilation_Context;\n\n");
    }

    fprintf(file, "// Forward declarations\n\n");
    for (uint32_t i = 0; i < type_count; ++i) {
      print_c_type_forward_declaration(file, &types[i]);
    }

    fprintf(file, "\n// Type Definitions\n\n");

    for (uint32_t i = 0; i < type_count; ++i) {
      print_c_type(file, &types[i]);
    }

    fclose(file);
    printf("C Types Generated at: %s\n", filename);
  }
  return 0;
}