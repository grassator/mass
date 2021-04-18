#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#define WIN32_LEAN_AND_MEAN
#include "prelude.h"

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
  Struct struct_;
  Enum enum_;
  Tagged_Union union_;
  Function function;
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

char *
strtolower(
  const char *str
) {
  size_t length = strlen(str) + 1;
  char *result = memcpy(malloc(length), str, length);
  for(size_t i = 0; result[i]; i++){
    result[i] = (char)tolower(result[i]);
  }
  return result;
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
      s32 max_value = INT32_MIN;
      s32 min_value = INT32_MAX;
      fprintf(file, "typedef enum %s {\n", type->enum_.name);
      for (uint64_t i = 0; i < type->enum_.item_count; ++i) {
        Enum_Item *item = &type->enum_.items[i];
        fprintf(file, "  %s_%s = %d,\n", type->enum_.name, item->name, item->value);
        max_value = s32_max(max_value, item->value);
        min_value = s32_min(min_value, item->value);
      }
      fprintf(file, "} %s;\n\n", type->enum_.name);

      fprintf(file, "#define %s__Max %i\n", type->enum_.name, max_value);
      fprintf(file, "#define %s__Min %i\n\n", type->enum_.name, min_value);

      char *lowercase_name = strtolower(type->enum_.name);
      fprintf(file, "const char *%s_name(%s value) {\n", lowercase_name, type->enum_.name);
      for (uint64_t i = 0; i < type->enum_.item_count; ++i) {
        Enum_Item *item = &type->enum_.items[i];
        fprintf(file, "  if (value == %d) return \"%s_%s\";\n", item->value, type->enum_.name, item->name);
      }
      fprintf(file, "  assert(!\"Unexpected value for enum %s\");\n", type->enum_.name);
      fprintf(file, "  return 0;\n");
      fprintf(file, "};\n\n");
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
        fprintf(file, "  char _tag_padding[4];\n");
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

void
print_mass_descriptor_and_type_forward_declaration(
  FILE *file,
  Type *type
) {
  switch(type->tag) {
    case Type_Tag_Struct: {
      char *lowercase_name = strtolower(type->struct_.name);
      fprintf(file, "static Descriptor descriptor_%s;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_array_%s;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_array_%s_ptr;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_%s_pointer;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_%s_pointer_pointer;\n", lowercase_name);
      break;
    }
    case Type_Tag_Enum: {
      char *lowercase_name = strtolower(type->enum_.name);
      fprintf(file, "static Descriptor descriptor_%s;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_array_%s;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_array_%s_ptr;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_%s_pointer;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_%s_pointer_pointer;\n", lowercase_name);
      break;
    }
    case Type_Tag_Tagged_Union: {
      char *lowercase_name = strtolower(type->union_.name);
      fprintf(file, "static Descriptor descriptor_%s;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_array_%s;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_array_%s_ptr;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_%s_pointer;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_%s_pointer_pointer;\n", lowercase_name);
      fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(%s_tag, %s_Tag)\n", lowercase_name, type->union_.name);
      break;
    }
    case Type_Tag_Function: {
      char *lowercase_name = strtolower(type->function.name);
      fprintf(file, "static Descriptor descriptor_%s;\n", lowercase_name);
      break;
    }
  }
}

void
print_mass_descriptor_and_type(
  FILE *file,
  Type *type
) {
  switch(type->tag) {
    case Type_Tag_Struct: {
      char *lowercase_name = strtolower(type->struct_.name);
      fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(array_%s_ptr, Array_%s_Ptr)\n", lowercase_name, type->struct_.name);
      fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(array_%s, Array_%s)\n", lowercase_name, type->struct_.name);
      fprintf(file, "MASS_DEFINE_STRUCT_DESCRIPTOR(%s,\n", lowercase_name);
      for (uint64_t i = 0; i < type->struct_.item_count; ++i) {
        Struct_Item *item = &type->struct_.items[i];
        fprintf(file, "  {\n");
        fprintf(file, "    .tag = Memory_Layout_Item_Tag_Base_Relative,\n");
        fprintf(file, "    .name = slice_literal_fields(\"%s\"),\n", item->name);
        Slice lowercase_type = slice_from_c_string(strtolower(item->type));
        // TODO support const
        Slice const_prefix = slice_literal("const ");
        if (slice_starts_with(lowercase_type, const_prefix)) {
          lowercase_type = slice_sub(lowercase_type, const_prefix.length, lowercase_type.length);
        }
        Slice original_lowercase_type = lowercase_type;
        Slice pointer_suffix = slice_literal(" *");
        while (slice_ends_with(lowercase_type, pointer_suffix)) {
          lowercase_type = slice_sub(lowercase_type, 0, lowercase_type.length - pointer_suffix.length);
        }
        fprintf(file, "    .descriptor = &descriptor_%"PRIslice, SLICE_EXPAND_PRINTF(lowercase_type));
        lowercase_type = original_lowercase_type;
        while (slice_ends_with(lowercase_type, pointer_suffix)) {
          fprintf(file, "_pointer");
          lowercase_type = slice_sub(lowercase_type, 0, lowercase_type.length - pointer_suffix.length);
        }
        fprintf(file, ",\n");
        fprintf(file, "    .Base_Relative.offset = offsetof(%s, %s),\n", type->struct_.name, item->name);
        fprintf(file, "  },\n");
      }
      fprintf(file, ");\n");
      fprintf(file, "MASS_DEFINE_TYPE_VALUE(%s);\n", lowercase_name);
      break;
    }
    case Type_Tag_Enum: {
      char *lowercase_name = strtolower(type->enum_.name);
      fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(%s, %s)\n", lowercase_name, type->enum_.name);
      break;
    }
    case Type_Tag_Tagged_Union: {
      char *lowercase_name = strtolower(type->union_.name);
      fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(%s, %s)\n", lowercase_name, type->union_.name);
      break;
    }
    case Type_Tag_Function: {

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

  push_type(type_enum("Module_Flags", (Enum_Item[]){
    { "Has_Exports", 1 << 0 },
  }));

  push_type(type_struct("Module", (Struct_Item[]){
    { "Module_Flags", "flags" },
    { "u32", "_flags_padding" },
    { "Source_File", "source_file" },
    { "Source_Range", "exports_source_range" },
    { "Scope *", "own_scope" },
    { "Scope *", "export_scope" },
  }));

  push_type(type_struct("Parse_Error", (Struct_Item[]){
    { "Slice", "message" },
    { "Source_Range", "source_range" },
  }));

  push_type(type_enum("Group_Tag", (Enum_Item[]){
    { "Paren", 1 },
    { "Square", 2 },
    { "Curly", 3 },
  }));

  push_type(type_struct("Value_View", (Struct_Item[]){
    { "Value * *", "values" },
    { "u64", "length" },
    { "Source_Range", "source_range" },
  }));

  push_type(type_enum("Symbol_Type", (Enum_Item[]){
    { "Id_Like", 1 },
    { "Operator_Like", 2 },
  }));

  push_type(type_struct("Symbol", (Struct_Item[]){
    { "Symbol_Type", "type" },
    { "u32", "_type_padding" },
    { "Slice", "name" },
  }));

  push_type(type_struct("Group", (Struct_Item[]){
    { "Group_Tag", "tag" },
    { "u32", "_tag_padding" },
    { "Value_View", "children" },
  }));

  push_type(add_common_fields(type_union("Token_Pattern", (Struct[]){
    struct_empty("Invalid"),
    struct_empty("Any"),
    struct_fields("Symbol", (Struct_Item[]){
      { "Slice", "name" },
    }),
    struct_fields("Group", (Struct_Item[]){
      { "Group_Tag", "tag" },
    }),
    struct_fields("String", (Struct_Item[]){
      { "Slice", "slice" },
    }),
  }), (Struct_Item[]){
    { "Token_Pattern *", "or" },
  }));

  push_type(type_enum("Section_Permissions", (Enum_Item[]){
    { "Read",    1 << 0 },
    { "Write",   1 << 1 },
    { "Execute", 1 << 2 },
  }));

  push_type(type_struct("Section", (Struct_Item[]){
    { "Virtual_Memory_Buffer", "buffer" },
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
    { "u32", "resolved" },
    { "u32", "offset_in_section" },
    { "Slice", "name" },
    { "Section *", "section" },
  }));

  push_type(type_struct("Label_Location_Diff_Patch_Info", (Struct_Item[]){
    { "Label_Index", "target_label_index" },
    { "Label", "from" },
    { "s32 *", "patch_target" },
  }));

   push_type(type_enum("Number_Base", (Enum_Item[]){
    { "2", 2 },
    { "10", 10 },
    { "16", 16 },
  }));

  push_type(type_struct("Number_Literal", (Struct_Item[]){
    { "Number_Base", "base" },
    { "u32", "negative" },
    { "u64", "bits" },
  }));

  push_type(type_struct("Macro_Capture", (Struct_Item[]){
    { "Scope *", "scope" },
    { "Slice", "name" },
    { "Value_View", "view" },
    { "Source_Range", "source_range" },
  }));

  push_type(type_struct("External_Symbol", (Struct_Item[]){
    { "Slice", "library_name" },
    { "Slice", "symbol_name" },
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
    { "u32", "has_value" },
  }));

  push_type(type_union("Memory_Location", (Struct[]){
    struct_fields("Instruction_Pointer_Relative", (Struct_Item[]){
      { "Label_Index", "label_index" },
    }),
    struct_fields("Indirect", (Struct_Item[]){
      { "Register", "base_register" },
      { "u32", "_base_register_padding" },
      { "Maybe_Register", "maybe_index_register" },
      { "s64", "offset" },
    }),
  }));

  push_type(type_union("Static_Memory", (Struct[]){
    //struct_fields("S8", (Struct_Item[]){{ "s8", "value" }}),
    //struct_fields("S16", (Struct_Item[]){{ "s16", "value" }}),
    //struct_fields("S32", (Struct_Item[]){{ "s32", "value" }}),
    //struct_fields("S64", (Struct_Item[]){{ "s64", "value" }}),
    struct_fields("U8", (Struct_Item[]){{ "u8", "value" }}),
    struct_fields("U16", (Struct_Item[]){{ "u16", "value" }}),
    struct_fields("U32", (Struct_Item[]){{ "u32", "value" }}),
    struct_fields("U64", (Struct_Item[]){{ "u64", "value" }}),
    struct_fields("Heap", (Struct_Item[]){
      { "void *", "pointer" },
    }),
  }));

  push_type(add_common_fields(type_union("Storage", (Struct[]){
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
    struct_fields("Static", (Struct_Item[]){
      { "Static_Memory", "memory" },
    }),
    struct_fields("Memory", (Struct_Item[]){
      { "Memory_Location", "location" },
    }),
  }), (Struct_Item[]){
    // TODO change to bit_size
    { "u64", "byte_size" },
  }));

  push_type(type_struct("Compiler_Source_Location", (Struct_Item[]){
    { "const char *", "filename" },
    { "const char *", "function_name" },
    { "u64", "line_number" },
  }));

  push_type(type_enum("Operator_Fixity", (Enum_Item[]){
    { "Infix", 1 << 0 },
    { "Prefix", 1 << 1 },
    { "Postfix", 1 << 2 },
  }));

  push_type(type_enum("Operator_Associativity", (Enum_Item[]){
    { "Left", 0 },
    { "Right", 1 },
  }));

  push_type(type_struct("Execution_Context", (Struct_Item[]){
    { "Allocator *", "allocator" },
    { "Compilation *", "compilation" },
    { "u64", "epoch" },
    { "Program *", "program" },
    { "Scope *", "scope" },
    { "Function_Builder *", "builder" },
    { "Module *", "module" },
    { "Mass_Result *", "result" },
  }));

  push_type(type_struct("Operator", (Struct_Item[]){
    { "Operator_Fixity", "fixity" },
    { "Operator_Associativity", "associativity" },
    { "u64", "precedence" },
    { "u64", "argument_count" },
    { "Mass_Handle_Operator_Proc", "handler" },
    { "void *", "handler_payload" },
  }));

  push_type(add_common_fields(type_union("Scope_Entry", (Struct[]){
    struct_fields("Value", (Struct_Item[]){
      { "Value *", "value" },
    }),
    struct_fields("Operator", (Struct_Item[]){
      { "Operator *", "maybe_prefix" },
      { "Operator *", "maybe_infix_or_postfix" },
    }),
  }), (Struct_Item[]){
    { "Source_Range", "source_range" },
  }));

  push_type(type_struct("Value", (Struct_Item[]){
    { "const Descriptor *", "descriptor" },
    { "Storage", "storage" },
    { "Value *", "next_overload" },
    { "u64", "epoch" },
    { "u64", "is_temporary" },
    { "Source_Range", "source_range" },
    { "Compiler_Source_Location", "compiler_source_location" },
  }));

  push_type(type_enum("Expected_Result_Storage", (Enum_Item[]){
    { "None", 0 },
    { "Static", 1 << 0 },
    { "Memory", 1 << 1 },
    { "Register", 1 << 2 },
    { "Xmm", 1 << 3 },
    { "Eflags", 1 << 4 },
  }));

  push_type(type_union("Expected_Result", (Struct[]){
    struct_fields("Exact", (Struct_Item[]){
      { "Value *", "value" },
    }),
    struct_fields("Flexible", (Struct_Item[]){
      { "const Descriptor *", "descriptor" },
      { "Expected_Result_Storage", "storage" },
      { "s32", "_storage_padding" },
      { "u64", "register_bit_set"},
    }),
  }));

  push_type(type_function("Lazy_Value_Proc", "Value *", (Argument[]){
    { "Execution_Context *", "context" },
    { "const Expected_Result *", "expected_result" },
    { "void *", "payload" },
  }));

  push_type(type_struct("Lazy_Value", (Struct_Item[]){
    { "Execution_Context", "context" },
    { "const Descriptor *", "descriptor" },
    { "Lazy_Value_Proc", "proc" },
    { "void *", "payload" },
  }));

  push_type(type_function("Mass_Handle_Operator_Proc", "Value *", (Argument[]){
    { "Execution_Context *", "context" },
    { "Value_View", "view" },
    { "void *", "payload" },
  }));

  push_type(type_enum("Descriptor_Function_Flags", (Enum_Item[]){
    { "None", 0 },
    { "Macro", 1 << 0 },
    { "No_Own_Scope", 1 << 1 },
    { "No_Own_Return", 1 << 2 },
    { "Compile_Time", 1 << 3 },
  }));

  push_type(add_common_fields(type_union("Memory_Layout_Item", (Struct[]){
    struct_fields("Absolute", (Struct_Item[]){
      { "Storage", "storage" },
    }),
    struct_fields("Base_Relative", (Struct_Item[]){
      { "s64", "offset" },
    }),
  }), (Struct_Item[]){
    { "Slice", "name" },
    { "const Descriptor *", "descriptor" },
    { "Source_Range", "source_range" },
    { "Value_View", "maybe_default_expression" },
  }));

  push_type(type_struct("Memory_Layout", (Struct_Item[]){
    { "Storage", "base" },
    { "Array_Memory_Layout_Item", "items" },
  }));

  push_type(type_struct("Function_Return", (Struct_Item[]){
    { "Slice", "name" },
    { "const Descriptor *", "descriptor" },
  }));

  push_type(type_struct("Function_Info", (Struct_Item[]){
    { "Descriptor_Function_Flags", "flags" },
    { "u32", "_flags_padding" },
    { "Memory_Layout", "memory_layout" },
    { "Value *", "body" },
    { "Scope *", "scope" },
    { "Function_Return", "returns" },
  }));

  push_type(add_common_fields(type_union("Descriptor", (Struct[]){
    struct_empty("Void"),
    struct_fields("Opaque", (Struct_Item[]){
      { "u64", "bit_size" },
    }),
    struct_fields("Function", (Struct_Item[]){
      { "Function_Info", "info" },
    }),
    struct_fields("Fixed_Size_Array", (Struct_Item[]){
      { "const Descriptor *", "item" },
      { "u64", "length" },
    }),
    struct_fields("Struct", (Struct_Item[]){
      { "Memory_Layout", "memory_layout" },
    }),
    struct_fields("Pointer", (Struct_Item[]){
      { "const Descriptor *", "to" },
    }),
  }), (Struct_Item[]){
    { "Slice", "name" },
  }));

  push_type(add_common_fields(type_union("Mass_Error", (Struct[]){
    struct_empty("Unknown"),
    struct_empty("Unimplemented"),
    struct_empty("Expression_Parse"),
    struct_empty("Non_Trailing_Default_Argument"),
    struct_fields("Unexpected_Token", (Struct_Item[]){
      { "Slice", "expected" },
    }),
    struct_fields("Operator_Infix_Suffix_Conflict", (Struct_Item[]){
      { "Slice", "symbol" },
    }),
    struct_fields("Operator_Prefix_Conflict", (Struct_Item[]){
      { "Slice", "symbol" },
    }),
    struct_fields("Undefined_Variable", (Struct_Item[]){
      { "Slice", "name" },
    }),
    struct_fields("Unknown_Field", (Struct_Item[]){
      { "const Descriptor *", "type" },
      { "Slice", "name" },
    }),
    struct_fields("Invalid_Identifier", (Struct_Item[]){
      { "Value *", "id" },
    }),
    struct_fields("Type_Mismatch", (Struct_Item[]){
      { "const Descriptor *", "expected" },
      { "const Descriptor *", "actual" },
    }),
    struct_fields("Undecidable_Overload", (Struct_Item[]){
      { "Value *", "a" },
      { "Value *", "b" },
    }),
  }), (Struct_Item[]){
    { "Slice", "detailed_message" },
    { "Source_Range", "source_range" },
  }));

  push_type(type_union("Mass_Result", (Struct[]){
    struct_empty("Success"),
    struct_fields("Error", (Struct_Item[]){
      { "Mass_Error", "error" },
    })
  }));

  {
    const char *filename = "../generated_types.h";
    #pragma warning(disable : 4996)
    FILE *file = fopen(filename, "wb");
    if (!file) exit(1);

    fprintf(file, "#ifndef GENERATED_TYPES_H\n");
    fprintf(file, "#define GENERATED_TYPES_H\n");

    // Make sure our generated structs have explicit padding
    fprintf(file, "_Pragma(\"warning (push)\") _Pragma(\"warning (default: 4820)\")\n");

    // Custom forward declarations
    {
      fprintf(file, "typedef void(*fn_type_opaque)();\n\n");
      fprintf(file, "typedef struct Scope Scope;\n\n");
      fprintf(file, "typedef struct Function_Builder Function_Builder;\n\n");
      fprintf(file, "typedef struct Program Program;\n\n");
      fprintf(file, "typedef struct Compilation Compilation;\n\n");
    }

    fprintf(file, "// Forward declarations\n\n");
    for (uint32_t i = 0; i < type_count; ++i) {
      print_c_type_forward_declaration(file, &types[i]);
    }

    fprintf(file, "\n// Type Definitions\n\n");

    for (uint32_t i = 0; i < type_count; ++i) {
      print_c_type(file, &types[i]);
    }
    fprintf(file, "_Pragma(\"warning (pop)\")\n");

    fprintf(file, "\n// Mass Type Reflection\n\n");

    fprintf(file, "static Descriptor descriptor_function_builder_pointer;\n");
    fprintf(file, "static Descriptor descriptor_program_pointer;\n");
    fprintf(file, "static Descriptor descriptor_scope_pointer;\n");
    fprintf(file, "static Descriptor descriptor_compilation_pointer;\n");
    fprintf(file, "static Descriptor descriptor_void;\n");
    fprintf(file, "static Descriptor descriptor_void_pointer;\n");
    fprintf(file, "static Descriptor descriptor_char;\n");
    fprintf(file, "static Descriptor descriptor_char_pointer;\n");

    // The type of type needs to be defined manually
    fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(type, Descriptor);\n");

    // Also need to define built-in types
    fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(allocator, Allocator);\n");
    fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(virtual_memory_buffer, Virtual_Memory_Buffer);\n");
    fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(range_u64, Range_u64);\n");
    fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(array_range_u64, Array_Range_u64);\n");
    fprintf(file, "#define MASS_PROCESS_BUILT_IN_TYPE(_NAME_, _BIT_SIZE_)\\\n");
    fprintf(file, "  MASS_DEFINE_OPAQUE_TYPE(_NAME_, _BIT_SIZE_)\n");
    fprintf(file, "MASS_ENUMERATE_BUILT_IN_TYPES\n");
    fprintf(file, "#undef MASS_PROCESS_BUILT_IN_TYPE\n\n");

    push_type(type_struct("Slice", (Struct_Item[]){
      { "u8 *", "bytes" },
      { "u64", "length" },
    }));
    fprintf(file, "typedef dyn_array_type(Slice *) Array_Slice_Ptr;\n");

    for (uint32_t i = 0; i < type_count; ++i) {
      print_mass_descriptor_and_type_forward_declaration(file, &types[i]);
    }
    for (uint32_t i = 0; i < type_count; ++i) {
      print_mass_descriptor_and_type(file, &types[i]);
    }
    fprintf(file, "\n#endif // GENERATED_TYPES_H\n");

    fclose(file);
    printf("C Types Generated at: %s\n", filename);
  }
  return 0;
}