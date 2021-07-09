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
} Enum_Type_Item;

typedef struct {
  Enum_Type_Item *items;
  uint64_t item_count;
} Enum_Type;

typedef struct {
  const char *type;
  const char *name;
  u32 array_length;
} Struct_Item;

typedef struct {
  const char *name;
  Struct_Item *items;
  uint64_t item_count;
} Struct_Type;

typedef struct {
  Struct_Type common;
  Struct_Type *items;
  uint64_t item_count;
} Tagged_Union_Type;

typedef struct {
  const char *key_type;
  const char *value_type;
  const char *hash_function;
  const char *equal_function;
} Hash_Map_Type;

typedef struct {
  const char *type;
  const char *name;
  const char *maybe_default_expression;
} Argument_Type;

typedef enum {
  Function_Kind_Default,
  Function_Kind_Typedef,
  Function_Kind_Compile_Time,
  Function_Kind_Intrinsic,
} Function_Kind;

typedef struct {
  const char *name;
  const char *returns;
  Function_Kind kind;
  Argument_Type *arguments;
  uint64_t argument_count;
} Function_Type;

typedef struct {
  u64 bits;
  bool negative;
} Meta_Number_Literal;

typedef enum {
  Meta_Type_Tag_C_Opaque,
  Meta_Type_Tag_Struct,
  Meta_Type_Tag_Tagged_Union,
  Meta_Type_Tag_Enum,
  Meta_Type_Tag_Function,
  Meta_Type_Tag_Hash_Map,
  Meta_Type_Tag_Number_Literal,
} Meta_Type_Tag;

typedef enum {
  Export_Target_None = 0,
  Export_Target_Global          = 1 << 0,
  Export_Target_Compiler_Module = 1 << 1,
} Export_Target;

typedef enum {
  Meta_Type_Flags_None           = 0,
  Meta_Type_Flags_No_C_Type      = 1 << 0,
  Meta_Type_Flags_No_Value_Array = 1 << 1,
} Meta_Type_Flags;

typedef struct {
  Meta_Type_Tag tag;
  Meta_Type_Flags flags;
  Export_Target export_target;
  const char *export_name;
  const char *name;
  union {
    Struct_Type struct_;
    Enum_Type enum_;
    Tagged_Union_Type union_;
    Function_Type function;
    Hash_Map_Type hash_map;
    Meta_Number_Literal number_literal;
  };
} Meta_Type;

void
print_c_type_forward_declaration(
  FILE *file,
  Meta_Type *type
) {
  const char *name;
  switch(type->tag) {
    case Meta_Type_Tag_Struct: {
      name = type->name;
      if (!(type->flags & Meta_Type_Flags_No_C_Type)) {
        fprintf(file, "typedef struct %s %s;\n", type->struct_.name, type->struct_.name);
      }
      break;
    }
    case Meta_Type_Tag_Enum: {
      name = 0;
      if (!(type->flags & Meta_Type_Flags_No_C_Type)) {
        fprintf(file, "typedef enum %s %s;\n", type->name, type->name);
      }
      break;
    }
    case Meta_Type_Tag_Tagged_Union: {
      name = type->name;
      if (!(type->flags & Meta_Type_Flags_No_C_Type)) {
        fprintf(file, "typedef struct %s %s;\n", type->name, type->name);
      }
      break;
    }
    case Meta_Type_Tag_Function: {
      name = 0;
      if (!(type->flags & Meta_Type_Flags_No_C_Type)) {
        if (type->function.kind == Function_Kind_Typedef) {
          fprintf(file, "typedef %s (*%s)\n  (", type->function.returns, type->name);
        } else {
          fprintf(file, "static %s %s\n  (", type->function.returns, type->name);
        }
        for (uint64_t i = 0; i < type->function.argument_count; ++i) {
          Argument_Type *arg = &type->function.arguments[i];
          if (i != 0) fprintf(file, ", ");
          fprintf(file, "%s %s", arg->type, arg->name);
        }
        fprintf(file, ");\n");
      }
      break;
    }
    case Meta_Type_Tag_Hash_Map: {
      name = 0;
      if (!(type->flags & Meta_Type_Flags_No_C_Type)) {
        fprintf(file, "typedef struct %s %s;\n", type->name, type->name);
      }
      break;
    }
    case Meta_Type_Tag_Number_Literal: {
      name = 0;
      if (!(type->flags & Meta_Type_Flags_No_C_Type)) {
        fprintf(file, "#define %s (%s%"PRIu64")\n", type->name,
          type->number_literal.negative ? "-" : "", type->number_literal.bits);
      }
      break;
    }
    case Meta_Type_Tag_C_Opaque: {
      name = type->name;
      break;
    }
    default: {
      name = 0;
    }
  }
  if (name) {
    fprintf(file, "typedef dyn_array_type(%s *) Array_%s_Ptr;\n", name, name);
    fprintf(file, "typedef dyn_array_type(const %s *) Array_Const_%s_Ptr;\n", name, name);
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
print_c_struct(
  FILE *file,
  Struct_Type *struct_,
  const char *name
) {
  fprintf(file, "typedef struct %s {\n", name);
  for (uint64_t i = 0; i < struct_->item_count; ++i) {
    Struct_Item *item = &struct_->items[i];
    fprintf(file, "  %s %s", item->type, item->name);
    if (item->array_length > 1) fprintf(file, "[%u]", item->array_length);
    fprintf(file, ";\n");
  }
  fprintf(file, "} %s;\n", name);
}

static void
print_union_variant_getter(
  FILE *file,
  const char *union_name,
  const char *variant_name
) {
  char *lower_union_name = strtolower(union_name);
  char *lower_variant_name = strtolower(variant_name);
  fprintf(file, "static inline %s_%s *\n", union_name, variant_name); // type signature
  fprintf(file, "%s_as_%s", lower_union_name, lower_variant_name); // fn name
  fprintf(file, "(%s *%s) {\n", union_name, lower_union_name); // args
  fprintf(file, "  assert(%s->tag == %s_Tag_%s);\n", lower_union_name, union_name, variant_name); // body
  fprintf(file, "  return &%s->%s;\n", lower_union_name, variant_name); // body
  fprintf(file, "}\n");
}

void
print_c_type(
  FILE *file,
  Meta_Type *type
) {
  switch(type->tag) {
    case Meta_Type_Tag_Struct: {
      if (!(type->flags & Meta_Type_Flags_No_C_Type)) {
        print_c_struct(file, &type->struct_, type->struct_.name);
      }
      if (!(type->flags & Meta_Type_Flags_No_Value_Array)) {
        fprintf(file, "typedef dyn_array_type(%s) Array_%s;\n\n", type->struct_.name, type->struct_.name);
      }
      break;
    }
    case Meta_Type_Tag_Enum: {
      if (!(type->flags & Meta_Type_Flags_No_C_Type)) {
        fprintf(file, "typedef enum %s {\n", type->name);
        for (uint64_t i = 0; i < type->enum_.item_count; ++i) {
          Enum_Type_Item *item = &type->enum_.items[i];
          fprintf(file, "  %s_%s = %d,\n", type->name, item->name, item->value);
        }
        fprintf(file, "} %s;\n\n", type->name);

        char *lowercase_name = strtolower(type->name);
        fprintf(file, "const char *%s_name(%s value) {\n", lowercase_name, type->name);
        for (uint64_t i = 0; i < type->enum_.item_count; ++i) {
          Enum_Type_Item *item = &type->enum_.items[i];
          fprintf(file, "  if (value == %d) return \"%s_%s\";\n", item->value, type->name, item->name);
        }
        fprintf(file, "  assert(!\"Unexpected value for enum %s\");\n", type->name);
        fprintf(file, "  return 0;\n");
        fprintf(file, "};\n\n");
      }
      break;
    }
    case Meta_Type_Tag_Tagged_Union: {
      if (!(type->flags & Meta_Type_Flags_No_C_Type)) {
        // Write out the enum
        {
          fprintf(file, "typedef enum {\n");
          for (uint64_t i = 0; i < type->union_.item_count; ++i) {
            Struct_Type *item = &type->union_.items[i];
            fprintf(file, "  %s_Tag_%s = %" PRIu64  ",\n", type->name, item->name, i);
          }
          fprintf(file, "} %s_Tag;\n\n", type->name);
        }

        // Write out individual structs
        {
          for (uint64_t i = 0; i < type->union_.item_count; ++i) {
            Struct_Type *struct_ = &type->union_.items[i];
            if (struct_->item_count) {
              char name_buffer[1024];
              assert(snprintf(name_buffer, countof(name_buffer), "%s_%s", type->name, struct_->name) > 0);
              print_c_struct(file, struct_, name_buffer);
            }
          }
        }

        // Write out the tagged union struct
        {
          fprintf(file, "typedef struct %s {\n", type->name);
          fprintf(file, "  %s_Tag tag;\n", type->name);
          fprintf(file, "  char _tag_padding[4];\n");
          for (uint64_t i = 0; i < type->union_.common.item_count; ++i) {
            Struct_Item *item = &type->union_.common.items[i];
            fprintf(file, "  %s %s;\n", item->type, item->name);
          }
          fprintf(file, "  union {\n");
          for (uint64_t i = 0; i < type->union_.item_count; ++i) {
            Struct_Type *struct_ = &type->union_.items[i];
            if (struct_->item_count) {
              fprintf(file, "    %s_%s %s;\n",
                type->name, struct_->name, struct_->name);
            }
          }
          fprintf(file, "  };\n");
          fprintf(file, "} %s;\n", type->name);
        }

        // Write out helper getters
        {
          for (uint64_t i = 0; i < type->union_.item_count; ++i) {
            Struct_Type *struct_ = &type->union_.items[i];
            if (struct_->item_count) {
              print_union_variant_getter(file, type->name, struct_->name);
            }
          }
        }
      }
      if (!(type->flags & Meta_Type_Flags_No_Value_Array)) {
        fprintf(file, "typedef dyn_array_type(%s) Array_%s;\n",
          type->name, type->name);
      }
      break;
    }
    case Meta_Type_Tag_C_Opaque: {
      if (!(type->flags & Meta_Type_Flags_No_Value_Array)) {
        fprintf(file, "typedef dyn_array_type(%s) Array_%s;\n\n", type->name, type->name);
      }
      break;
    }
    case Meta_Type_Tag_Number_Literal:
    case Meta_Type_Tag_Function: {
      // We only need a forward declaration so nothing to do here
      break;
    }
    case Meta_Type_Tag_Hash_Map: {
      Hash_Map_Type *map = &type->hash_map;
      if (strcmp(map->key_type, "Slice") == 0) {
        assert(!map->hash_function);
        assert(!map->equal_function);
        fprintf(file, "hash_map_slice_template(%s, %s)\n", type->name, map->value_type);
      } else {
        fprintf(file, "hash_map_template(%s, %s, %s, %s, %s)\n",
          type->name, map->key_type, map->value_type, map->hash_function, map->equal_function);
      }
      break;
    }
  }
}

static void
print_scope_define(
  FILE *file,
  const char *name,
  const char *export_name
) {
  char *lowercase_name = strtolower(name);
  fprintf(file, "  scope_define_value(\n");
  fprintf(file, "    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,\n");
  fprintf(file, "    slice_literal(\"%s\"), type_%s_value\n", export_name, lowercase_name);
  fprintf(file, "  );\n");
}

static void
print_scope_enum(
  FILE *file,
  const char *name,
  const char *export_name
) {
  char *lowercase_name = strtolower(name);
  fprintf(file, "  scope_define_enum(\n");
  fprintf(file, "    compilation->allocator, scope, COMPILER_SOURCE_RANGE,\n");
  fprintf(file, "    slice_literal(\"%s\"), type_%s_value,\n", export_name, lowercase_name);
  fprintf(file, "    %s_items, countof(%s_items)\n", lowercase_name, lowercase_name);
  fprintf(file, "  );\n");
}

static void
print_mass_struct_descriptor_type(
  FILE *file,
  const char *type
) {
  Slice const_prefix = slice_literal("const ");
  Slice lowercase_type = slice_from_c_string(strtolower(type));
  if (slice_starts_with(lowercase_type, const_prefix)) {
    lowercase_type = slice_sub(lowercase_type, const_prefix.length, lowercase_type.length);
  }
  Slice original_lowercase_type = lowercase_type;
  Slice pointer_suffix = slice_literal(" *");
  while (slice_ends_with(lowercase_type, pointer_suffix)) {
    lowercase_type = slice_sub(lowercase_type, 0, lowercase_type.length - pointer_suffix.length);
  }
  fprintf(file, "descriptor_%"PRIslice, SLICE_EXPAND_PRINTF(lowercase_type));
  lowercase_type = original_lowercase_type;
  while (slice_ends_with(lowercase_type, pointer_suffix)) {
    fprintf(file, "_pointer");
    lowercase_type = slice_sub(lowercase_type, 0, lowercase_type.length - pointer_suffix.length);
  }
}

static void
print_scope_define_function(
  FILE *file,
  Meta_Type *type
) {
  assert(type->tag == Meta_Type_Tag_Function);
  Function_Type *function = &type->function;

  #define DEFAULT_EXPR_FORMAT "%s_%s__default_expression"
  for (uint64_t i = 0; i < function->argument_count; ++i) {
    Argument_Type *arg = &function->arguments[i];
    if (!arg->maybe_default_expression) continue;
    // TODO escape default expression
    fprintf(file, "  MASS_FN_ARG_DEFAULT_EXPRESSION("DEFAULT_EXPR_FORMAT", \"%s\")\n",
      function->name, arg->name, arg->maybe_default_expression);
  }

  if (function->kind == Function_Kind_Typedef) {
    fprintf(file, "  MASS_DEFINE_FUNCTION_TYPE(\n");
  } else {
    fprintf(file, "  MASS_DEFINE_FUNCTION(\n");
  }
  fprintf(file, "    Descriptor_Function_Flags_None");
  switch(function->kind) {
    case Function_Kind_Default:
    case Function_Kind_Typedef: {
      // Nothing to do
    } break;
    case Function_Kind_Compile_Time: {
      fprintf(file, " | Descriptor_Function_Flags_Compile_Time");
    } break;
    case Function_Kind_Intrinsic: {
      fprintf(file, " | Descriptor_Function_Flags_Compile_Time");
      fprintf(file, " | Descriptor_Function_Flags_Intrinsic");

      if (function->argument_count != 2) {
        printf("Intrinsic %s must have 2 arguments", function->name);
        exit(1);
      }
      {
        const char *expected = "Execution_Context *";
        if (strcmp(function->arguments[0].type, expected) != 0) {
          printf("Intrinsic %s first argument must have type %s", function->name, expected);
          exit(1);
        }
      }
      {
        const char *expected = "Value_View";
        if (strcmp(function->arguments[1].type, expected) != 0) {
          printf("Intrinsic %s second argument must have type %s", function->name, expected);
          exit(1);
        }
      }
      {
        const char *expected = "Value *";
        if (strcmp(function->returns, expected) != 0) {
          printf("Intrinsic %s return type must be %s", function->name, expected);
          exit(1);
        }
      }
    } break;
  }
  fprintf(file, ",\n");

  fprintf(file, "    %s,", function->name);
  fprintf(file, " \"%s\",", type->export_name);
  {
    fprintf(file, " &");
    print_mass_struct_descriptor_type(file, function->returns);
    fprintf(file, ",\n");
  }
  for (uint64_t i = 0; i < function->argument_count; ++i) {
    Argument_Type *arg = &function->arguments[i];
    if (i != 0) fprintf(file, ",\n");
    if (arg->maybe_default_expression) {
      fprintf(file, "    MASS_FN_ARG_WITH_DEFAULT(\"%s\", "DEFAULT_EXPR_FORMAT", &",
        arg->name, function->name, arg->name);
    } else {
      fprintf(file, "    MASS_FN_ARG(\"%s\", &", arg->name);
    }
    print_mass_struct_descriptor_type(file, arg->type);
    fprintf(file, ")");
  }
  fprintf(file, "\n  );\n");
}

static void
print_scope_export(
  FILE *file,
  Meta_Type *type
) {
  switch(type->tag) {
    case Meta_Type_Tag_C_Opaque:
    case Meta_Type_Tag_Struct: {
      print_scope_define(file, type->name, type->export_name);
      break;
    }
    case Meta_Type_Tag_Enum: {
      print_scope_enum(file, type->name, type->export_name);
      break;
    }
    case Meta_Type_Tag_Tagged_Union: {
      print_scope_define(file, type->name, type->export_name);

      char name_buffer[1024];
      s32 result = snprintf(name_buffer, countof(name_buffer), "%s_Tag", type->name);
      assert(result > 0);
      char export_name_buffer[1024];
      result = snprintf(export_name_buffer, countof(export_name_buffer), "%s_Tag", type->export_name);
      assert(result > 0);
      print_scope_enum(file, name_buffer, export_name_buffer);

      for (uint64_t i = 0; i < type->union_.item_count; ++i) {
        Struct_Type *struct_ = &type->union_.items[i];
        if (struct_->item_count) {
          s32 result = snprintf(name_buffer, countof(name_buffer), "%s_%s", type->name, struct_->name);
          assert(result > 0);
          result = snprintf(export_name_buffer, countof(export_name_buffer), "%s_%s", type->export_name, struct_->name);
          assert(result > 0);
          print_scope_define(file, name_buffer, export_name_buffer);
        }
      }
      break;
    }
    case Meta_Type_Tag_Function: {
      print_scope_define_function(file, type);
      break;
    }
    case Meta_Type_Tag_Hash_Map: {
      print_scope_define(file, type->name, type->export_name);
      break;
    }
    case Meta_Type_Tag_Number_Literal: {
      panic("NOT SUPPORTED");
      break;
    }
  }
}

static void
print_natvis_array(
  FILE *file,
  const char *name
) {
  fprintf(file, "<Type Name=\"Array_%s\">\n", name);
  fprintf(file, "  <Expand>\n");
  fprintf(file, "    <Item Name=\"[length]\">data->length</Item>\n");
  fprintf(file, "    <ArrayItems>\n");
  fprintf(file, "      <Size>data->length</Size>\n");
  fprintf(file, "      <ValuePointer>data->items</ValuePointer>\n");
  fprintf(file, "    </ArrayItems>\n");
  fprintf(file, "  </Expand>\n");
  fprintf(file, "</Type>\n");
}

static void
print_natvis_array_types(
  FILE *file,
  const char *name
) {
  print_natvis_array(file, name);

  char name_buffer[1024];

  s32 result = snprintf(name_buffer, countof(name_buffer), "%s_Ptr", name);
  assert(result > 0);
  print_natvis_array(file, name_buffer);

  result = snprintf(name_buffer, countof(name_buffer), "Const_%s_Ptr", name);
  assert(result > 0);
  print_natvis_array(file, name_buffer);
}

static void
print_natvis(
  FILE *file,
  Meta_Type *type
) {
  switch(type->tag) {
    case Meta_Type_Tag_Struct: {
      print_natvis_array_types(file, type->struct_.name);
      break;
    }
    case Meta_Type_Tag_Tagged_Union: {
      print_natvis_array_types(file, type->name);

      fprintf(file, "<Type Name=\"%s\">\n", type->name);
      for (uint64_t i = 0; i < type->union_.item_count; ++i) {
        Struct_Type *struct_ = &type->union_.items[i];
        fprintf(file, "  <DisplayString Condition=\"tag == %s_Tag_%s\">\n", type->name, struct_->name);
        if (struct_->item_count) {
          fprintf(file, "    %s { %s }\n", struct_->name, struct_->name);
        } else {
          fprintf(file, "    %s\n", struct_->name);
        }
        fprintf(file, "  </DisplayString>\n");
      }

      fprintf(file, "  <Expand>\n");
      fprintf(file, "    <Item Name=\"tag\">tag</Item>\n");
      for (uint64_t i = 0; i < type->union_.common.item_count; ++i) {
        Struct_Item *item = &type->union_.common.items[i];
        fprintf(file, "    <Item Name=\"%s\">%s</Item>\n", item->name, item->name);
      }
      for (uint64_t i = 0; i < type->union_.item_count; ++i) {
        Struct_Type *struct_ = &type->union_.items[i];
        if (struct_->item_count) {
          fprintf(file, "    <Item Name=\"%s\" Condition=\"tag == %s_Tag_%s\">%s</Item>\n",
            struct_->name, type->name, struct_->name, struct_->name);
        }
      }
      fprintf(file, "  </Expand>\n");
      fprintf(file, "</Type>\n");
      break;
    }
    case Meta_Type_Tag_Hash_Map: {
      // TODO
      break;
    }
    case Meta_Type_Tag_C_Opaque:
    case Meta_Type_Tag_Enum:
    case Meta_Type_Tag_Function:
    case Meta_Type_Tag_Number_Literal: {
      // Nothing to do
      break;
    }
  }
}

hash_map_slice_template(Fixed_Array_Descriptor_Set, bool)

void
print_mass_array_descriptors_for_struct(
  FILE *file,
  Struct_Type *struct_
) {
  static Fixed_Array_Descriptor_Set *already_defined_set = 0;
  if (!already_defined_set) already_defined_set = hash_map_make(Fixed_Array_Descriptor_Set);
  static Fixed_Buffer *temp = 0;
  if (!temp) temp = fixed_buffer_make(.capacity = 1024);
  static Bucket_Buffer *names_buffer = 0;
  if (!names_buffer) names_buffer = bucket_buffer_make();

  for (uint64_t i = 0; i < struct_->item_count; ++i) {
    Struct_Item *item = &struct_->items[i];
    if (item->array_length <= 1) continue;
    temp->occupied = 0;
    fixed_buffer_append_slice(temp, slice_from_c_string(item->type));
    fixed_buffer_append_u32(temp, item->array_length);
    if (hash_map_has(already_defined_set, fixed_buffer_as_slice(temp))) continue;
    Slice type_slice = bucket_buffer_append_slice(names_buffer, fixed_buffer_as_slice(temp));
    hash_map_set(already_defined_set, type_slice, true);
    fprintf(file, "static Descriptor ");
    print_mass_struct_descriptor_type(file, item->type);
    fprintf(file, "_%u = MASS_DESCRIPTOR_STATIC_ARRAY(%s, %u, &",
      item->array_length, item->type, item->array_length);
    print_mass_struct_descriptor_type(file, item->type);
    fprintf(file, ");\n");
  }
}

void
print_mass_descriptor_fixed_array_types(
  FILE *file,
  Meta_Type *type
) {
  switch(type->tag) {
    case Meta_Type_Tag_Struct: {
      print_mass_array_descriptors_for_struct(file, &type->struct_);
      break;
    }
    case Meta_Type_Tag_Tagged_Union: {
      for (uint64_t i = 0; i < type->union_.item_count; ++i) {
        Struct_Type *struct_ = &type->union_.items[i];
        print_mass_array_descriptors_for_struct(file, struct_);
      }
      break;
    }
    case Meta_Type_Tag_C_Opaque:
    case Meta_Type_Tag_Enum:
    case Meta_Type_Tag_Function:
    case Meta_Type_Tag_Number_Literal:
    case Meta_Type_Tag_Hash_Map: {
      break;
    }
  }
}

void
print_mass_descriptor_and_type_forward_declaration(
  FILE *file,
  Meta_Type *type
) {
  switch(type->tag) {
    case Meta_Type_Tag_Struct: {
      char *lowercase_name = strtolower(type->struct_.name);
      fprintf(file, "static Descriptor descriptor_%s;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_array_%s;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_array_%s_ptr;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_%s_pointer;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_%s_pointer_pointer;\n", lowercase_name);
      break;
    }
    case Meta_Type_Tag_Enum: {
      char *lowercase_name = strtolower(type->name);
      fprintf(file, "static Descriptor descriptor_%s;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_array_%s;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_array_%s_ptr;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_%s_pointer;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_%s_pointer_pointer;\n", lowercase_name);
      break;
    }
    case Meta_Type_Tag_Tagged_Union: {
      char *lowercase_name = strtolower(type->name);
      fprintf(file, "static Descriptor descriptor_%s;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_array_%s;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_array_%s_ptr;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_%s_pointer;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_%s_pointer_pointer;\n", lowercase_name);
      break;
    }
    case Meta_Type_Tag_C_Opaque: {
      char *lowercase_name = strtolower(type->name);
      fprintf(file, "static Descriptor descriptor_%s;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_array_%s;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_%s_pointer;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_%s_pointer_pointer;\n", lowercase_name);
      break;
    }
    case Meta_Type_Tag_Function: {
      char *lowercase_name = strtolower(type->name);
      fprintf(file, "static Descriptor descriptor_%s;\n", lowercase_name);
      break;
    }
    case Meta_Type_Tag_Hash_Map: {
      char *lowercase_name = strtolower(type->name);
      fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(%s, %s);\n", lowercase_name, type->name);
      break;
    }
    case Meta_Type_Tag_Number_Literal: {
      // TODO define global number literal values
      break;
    }
  }
}

static void
print_mass_struct_item(
  FILE *file,
  const char *struct_name,
  Struct_Item *item
) {
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
  if (item->array_length > 1) fprintf(file, "_%u", item->array_length);
  fprintf(file, ",\n");
  fprintf(file, "    .Base_Relative.offset = offsetof(%s, %s),\n", struct_name, item->name);
  fprintf(file, "  },\n");
}

static void
print_mass_struct(
  FILE *file,
  const char *struct_name,
  Struct_Type *struct_
) {
  char *lowercase_name = strtolower(struct_name);
  fprintf(file, "MASS_DEFINE_STRUCT_DESCRIPTOR(%s, %s,\n", lowercase_name, struct_name);
  for (uint64_t i = 0; i < struct_->item_count; ++i) {
    Struct_Item *item = &struct_->items[i];
    print_mass_struct_item(file, struct_name, item);
  }
  fprintf(file, ");\n");
  fprintf(file, "MASS_DEFINE_TYPE_VALUE(%s);\n", lowercase_name);
}

static void
print_mass_descriptor_and_type(
  FILE *file,
  Meta_Type *type
) {
  switch(type->tag) {
    case Meta_Type_Tag_Struct: {
      char *lowercase_name = strtolower(type->name);
      fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(array_%s_ptr, Array_%s_Ptr)\n", lowercase_name, type->name);
      fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(array_%s, Array_%s)\n", lowercase_name, type->name);
      print_mass_struct(file, type->name, &type->struct_);
      break;
    }
    case Meta_Type_Tag_C_Opaque: {
      char *lowercase_name = strtolower(type->name);
      fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(%s, %s)\n", lowercase_name, type->name);
      fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(array_%s, Array_%s)\n", lowercase_name, type->name);
      break;
    }
    case Meta_Type_Tag_Enum: {
      char *lowercase_name = strtolower(type->name);
      fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(%s, %s)\n", lowercase_name, type->name);

      fprintf(file, "static C_Enum_Item %s_items[] = {\n", lowercase_name);
      for (uint64_t i = 0; i < type->enum_.item_count; ++i) {
        Enum_Type_Item *item = &type->enum_.items[i];
        fprintf(
          file, "{ .name = slice_literal_fields(\"%s\"), .value = %d },\n",
          item->name, item->value
        );
      }
      fprintf(file, "};\n");
      break;
    }
    case Meta_Type_Tag_Tagged_Union: {
      fprintf(file, "/*union struct start */\n");
      char *lowercase_name = strtolower(type->name);
      fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(array_%s_ptr, Array_%s_Ptr)\n", lowercase_name, type->name);
      fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(array_%s, Array_%s)\n", lowercase_name, type->name);

      // Write out the enum
      {
        fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(%s_tag, %s_Tag)\n", lowercase_name, type->name);

        fprintf(file, "static C_Enum_Item %s_tag_items[] = {\n", lowercase_name);
        for (uint64_t i = 0; i < type->union_.item_count; ++i) {
          Struct_Type *item = &type->union_.items[i];
          fprintf(
            file, "{ .name = slice_literal_fields(\"%s\"), .value = %"PRIu64" },\n",
            item->name, i
          );
        }
        fprintf(file, "};\n");
      }

      // Write out individual structs
      {
        for (uint64_t i = 0; i < type->union_.item_count; ++i) {
          Struct_Type *struct_ = &type->union_.items[i];
          if (struct_->item_count) {
            char buffer[1024];
            s32 result = snprintf(buffer, countof(buffer), "%s_%s", type->name, struct_->name);
            assert(result > 0);
            print_mass_struct(file, buffer, struct_);
          }
        }
      }

      // Write out the tagged union struct
      {
        fprintf(file, "MASS_DEFINE_STRUCT_DESCRIPTOR(%s, %s,\n", lowercase_name, type->name);

        fprintf(file, "  {\n");
        fprintf(file, "    .tag = Memory_Layout_Item_Tag_Base_Relative,\n");
        fprintf(file, "    .name = slice_literal_fields(\"tag\"),\n");
        fprintf(file, "    .descriptor = &descriptor_%s_tag,\n", lowercase_name);
        fprintf(file, "    .Base_Relative.offset = offsetof(%s, tag),\n", type->name);
        fprintf(file, "  },\n");

        for (uint64_t i = 0; i < type->union_.common.item_count; ++i) {
          Struct_Item *item = &type->union_.common.items[i];
          print_mass_struct_item(file, type->name, item);
        }

        for (uint64_t i = 0; i < type->union_.item_count; ++i) {
          Struct_Type *struct_ = &type->union_.items[i];
          if (struct_->item_count) {
            const char *struct_lowercase_name = strtolower(struct_->name);

            fprintf(file, "  {\n");
            fprintf(file, "    .tag = Memory_Layout_Item_Tag_Base_Relative,\n");
            fprintf(file, "    .name = slice_literal_fields(\"%s\"),\n", struct_->name);
            fprintf(file, "    .descriptor = &descriptor_%s_%s,\n", lowercase_name, struct_lowercase_name);
            fprintf(file, "    .Base_Relative.offset = offsetof(%s, %s),\n", type->name, struct_->name);
            fprintf(file, "  },\n");
          }
        }
        fprintf(file, ");\n");
        fprintf(file, "MASS_DEFINE_TYPE_VALUE(%s);\n", lowercase_name);
      }
      fprintf(file, "/*union struct end*/\n");
      break;
    }
    case Meta_Type_Tag_Function: {
      // Exported functions are handled separately
      break;
    }
    case Meta_Type_Tag_Hash_Map: {
      // TODO
      break;
    }
    case Meta_Type_Tag_Number_Literal: {
      // TODO define global number literal values
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
    .name = (_NAME_STRING_),\
    .items = (__VA_ARGS__),\
    .item_count = countof(__VA_ARGS__),\
  }

#define type_c_opaque(_NAME_STRING_, ...)\
  (Meta_Type){\
    .tag = Meta_Type_Tag_C_Opaque,\
    .name = (_NAME_STRING_),\
  }

#define type_enum(_NAME_STRING_, ...)\
  (Meta_Type){\
    .tag = Meta_Type_Tag_Enum,\
    .name = (_NAME_STRING_),\
    .enum_ = {\
      .items = (__VA_ARGS__),\
      .item_count = countof(__VA_ARGS__),\
    }\
  }

#define type_union(_NAME_STRING_, ...)\
  (Meta_Type){\
    .tag = Meta_Type_Tag_Tagged_Union,\
    .name = (_NAME_STRING_),\
    .union_ = {\
      .items = (__VA_ARGS__),\
      .item_count = countof(__VA_ARGS__),\
    }\
  }

static inline Meta_Type
add_common_fields_internal(
  Meta_Type type,
  Struct_Type common
) {
  assert(type.tag == Meta_Type_Tag_Tagged_Union);
  type.union_.common = common;
  return type;
}

#define add_common_fields(_TYPE_, ...)\
  add_common_fields_internal(\
    _TYPE_,\
    (Struct_Type)struct_fields("", __VA_ARGS__)\
  )

#define type_struct(_NAME_STRING_, ...)\
  (Meta_Type){\
    .name = (_NAME_STRING_),\
    .tag = Meta_Type_Tag_Struct,\
    .struct_ = struct_fields((_NAME_STRING_), __VA_ARGS__)\
  }

#define type_hash_map(_NAME_STRING_, ...)\
  (Meta_Type){\
    .tag = Meta_Type_Tag_Hash_Map,\
    .name = (_NAME_STRING_),\
    .hash_map = __VA_ARGS__\
  }

#define meta_number_literal(_NAME_STRING_, ...)\
  (Meta_Type){\
    .tag = Meta_Type_Tag_Number_Literal,\
    .name = (_NAME_STRING_),\
    .number_literal = __VA_ARGS__\
  }

static inline Meta_Type
type_function_impl(
  const char *name,
  const char *returns,
  Argument_Type *arguments,
  u64 argument_count,
  Function_Kind kind
) {
  return (Meta_Type){
    .tag = Meta_Type_Tag_Function,
    .name = name,
    .function = {
      .name = name,
      .returns = returns,
      .arguments = arguments,
      .argument_count = argument_count,
      .kind = kind,
    }
  };
}

#define type_function(_KIND_, _NAME_STRING_, _RETURNS_, ...)\
  type_function_impl((_NAME_STRING_), (_RETURNS_), (__VA_ARGS__), countof(__VA_ARGS__), Function_Kind_##_KIND_)

#define type_intrinsic(_NAME_STRING_)\
  type_function(Intrinsic, (_NAME_STRING_), "Value *", (Argument_Type[]){\
    { "Execution_Context *", "context" },\
    { "Value_View", "args" },\
  })

Meta_Type types[4096] = {0};
uint32_t type_count = 0;

static inline Meta_Type *
push_type(
  Meta_Type type
) {
  if (type_count >= countof(types)) {
    fprintf(stderr, "Too many types defined");
    exit(1);
  }
  types[type_count] = type;
  return &types[type_count++];
}

static inline Meta_Type *
export_global_custom_name(
  const char *name,
  Meta_Type *type
) {
  type->export_name = name;
  type->export_target |= Export_Target_Global;
  return type;
}

static inline Meta_Type *
export_compiler_custom_name(
  const char *name,
  Meta_Type *type
) {
  type->export_name = name;
  type->export_target |= Export_Target_Compiler_Module;
  return type;
}

static inline Meta_Type *
export_global(
  Meta_Type *type
) {
  return export_global_custom_name(type->name, type);
}

static inline Meta_Type *
export_compiler(
  Meta_Type *type
) {
  return export_compiler_custom_name(type->name, type);
}

static inline Meta_Type *
set_flags(
  Meta_Type *type,
  Meta_Type_Flags flags
) {
  type->flags |= flags;
  return type;
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
    { "Range_u32", "offsets" },
  }));

  push_type(add_common_fields(type_union("Module_Export", (Struct_Type[]){
    struct_empty("None"),
    struct_empty("All"),
    struct_fields("Selective", (Struct_Item[]){
      { "Array_Value_Ptr", "symbols" },
    }),
  }), (Struct_Item[]){
    { "Scope *", "scope" },
    { "Source_Range", "source_range" },
  }));


  push_type(type_struct("Module", (Struct_Item[]){
    { "Source_File", "source_file" },
    { "Scope *", "own_scope" },
    { "Module_Export", "export" },
  }));

  push_type(type_struct("Parse_Error", (Struct_Item[]){
    { "Slice", "message" },
    { "Source_Range", "source_range" },
  }));

  push_type(type_enum("Group_Tag", (Enum_Type_Item[]){
    { "Paren", 1 },
    { "Square", 2 },
    { "Curly", 3 },
  }));

  export_compiler(push_type(type_struct("Value_View", (Struct_Item[]){
    { "Value * *", "values" },
    { "u64", "length" },
    { "Source_Range", "source_range" },
  })));

  push_type(type_enum("Symbol_Type", (Enum_Type_Item[]){
    { "Id_Like", 1 },
    { "Operator_Like", 2 },
  }));

  push_type(type_struct("Symbol", (Struct_Item[]){
    { "Symbol_Type", "type" },
    { "s32", "hash" },
    { "Slice", "name" },
  }));

  push_type(type_struct("Group", (Struct_Item[]){
    { "Group_Tag", "tag" },
    { "u32", "_tag_padding" },
    { "Value_View", "children" },
  }));

  push_type(type_union("Token_Pattern", (Struct_Type[]){
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
  }));

  push_type(type_enum("Section_Permissions", (Enum_Type_Item[]){
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

  push_type(type_struct("Program_Memory", (Struct_Item[]){
    { "Virtual_Memory_Buffer", "buffer" },
    { "Section", "rw_data" },
    { "Section", "code" },
    { "Section", "ro_data" },
  }));

  push_type(type_enum("Register", (Enum_Type_Item[]){
    { "A", 0b0000 },
    { "C", 0b0001 },
    { "D", 0b0010 },
    { "B", 0b0011 },
    { "SP", 0b0100 },
    { "BP", 0b0101 },
    { "SI", 0b0110 },
    { "DI", 0b0111 },
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
    { "const Program *", "program" },
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

  push_type(type_enum("Number_Base", (Enum_Type_Item[]){
    { "2", 2 },
    { "10", 10 },
    { "16", 16 },
  }));

  export_global(push_type(type_struct("Number_Literal", (Struct_Item[]){
    { "Number_Base", "base" },
    { "u32", "negative" },
    { "u64", "bits" },
  })));

  push_type(type_struct("Macro_Capture", (Struct_Item[]){
    { "Scope *", "scope" },
    { "Slice", "name" },
    { "Value_View", "view" },
    { "Source_Range", "source_range" },
  }));

  export_compiler(push_type(type_struct("External_Symbol", (Struct_Item[]){
    { "Slice", "library_name" },
    { "Slice", "symbol_name" },
  })));

  push_type(type_struct("Import_Symbol", (Struct_Item[]){
    { "Slice", "name" },
    { "Label_Index", "label32" },
  }));

  push_type(type_struct("Import_Library", (Struct_Item[]){
    { "Slice", "name" },
    { "Array_Import_Symbol", "symbols" },
  }));

  push_type(type_enum("Compare_Type", (Enum_Type_Item[]){
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

  push_type(type_enum("Stack_Area", (Enum_Type_Item[]){
    { "Local", 0 },
    { "Received_Argument", 1 },
    { "Call_Target_Argument", 2 },
  }));

  push_type(type_union("Memory_Location", (Struct_Type[]){
    struct_fields("Instruction_Pointer_Relative", (Struct_Item[]){
      { "Label_Index", "label_index" },
    }),
    struct_fields("Indirect", (Struct_Item[]){
      { "Register", "base_register" },
      { "u32", "_base_register_padding" },
      { "s64", "offset" },
    }),
    struct_fields("Stack", (Struct_Item[]){
      { "Stack_Area", "area" },
      { "s32", "offset" },
    }),
  }));

  push_type(type_union("Static_Memory", (Struct_Type[]){
    struct_fields("U8", (Struct_Item[]){{ "u8", "value" }}),
    struct_fields("U16", (Struct_Item[]){{ "u16", "value" }}),
    struct_fields("U32", (Struct_Item[]){{ "u32", "value" }}),
    struct_fields("U64", (Struct_Item[]){{ "u64", "value" }}),
    struct_fields("Heap", (Struct_Item[]){
      { "const void *", "pointer" },
    }),
  }));

  push_type(add_common_fields(type_union("Storage", (Struct_Type[]){
    struct_empty("None"),
    struct_empty("Any"),
    struct_fields("Eflags", (Struct_Item[]){
      { "Compare_Type", "compare_type" },
    }),
    struct_fields("Register", (Struct_Item[]){
      { "Register", "index" },
      { "u16", "packed" },
      { "u16", "offset_in_bits" },
    }),
    struct_fields("Xmm", (Struct_Item[]){
      { "Register", "index" },
      { "u32", "offset" },
    }),
    struct_fields("Static", (Struct_Item[]){
      { "Static_Memory", "memory" },
    }),
    struct_fields("Memory", (Struct_Item[]){
      { "Memory_Location", "location" },
    }),
    struct_fields("Unpacked", (Struct_Item[]){
      { "Register", "registers", 2 },
    }),
  }), (Struct_Item[]){
    // TODO change to bit_size
    { "u64", "byte_size" },
  }));

  push_type(type_struct("Relocation", (Struct_Item[]){
    { "Storage", "patch_at" },
    { "Storage", "address_of" },
  }));

  push_type(type_struct("Compiler_Source_Location", (Struct_Item[]){
    { "const char *", "filename" },
    { "const char *", "function_name" },
    { "u64", "line_number" },
  }));

  push_type(type_struct("Instruction_Assembly", (Struct_Item[]){
    { "const X64_Mnemonic *", "mnemonic" },
    { "Storage", "operands", 3 },
  }));

  push_type(add_common_fields(type_union("Instruction", (Struct_Type[]){
    struct_fields("Label", (Struct_Item[]){
      { "Label_Index", "index" },
    }),
    struct_fields("Bytes", (Struct_Item[]){
      { "u8", "memory", 15 },
      { "u8", "length" },
    }),
    struct_fields("Label_Patch", (Struct_Item[]){
      { "s64", "offset" },
      { "Label_Index", "label_index" },
    }),
    struct_fields("Stack_Patch", (Struct_Item[]){
      { "s32", "mod_r_m_offset_in_previous_instruction" },
      { "Stack_Area", "stack_area" },
    }),
  }), (Struct_Item[]){
    { "Compiler_Source_Location", "compiler_source_location" },
    { "Source_Range", "source_range" },
    { "Scope *", "scope" },
  }));

  push_type(type_struct("Code_Block", (Struct_Item[]){
    { "Label_Index", "start_label" },
    { "Label_Index", "end_label" },
    { "Array_Instruction", "instructions" },
  }));

  push_type(type_struct("Function_Builder", (Struct_Item[]){
    { "s32", "stack_reserve" },
    { "u32", "max_call_parameters_stack_size" },
    { "Value *", "return_value" },
    { "Code_Block", "code_block" },
    { "u64", "register_used_bitset" },
    { "u64", "register_volatile_bitset" },
    { "u64", "register_occupied_bitset" },
    { "Slice", "source" },
    { "const Function_Info *", "function" },
  }));

  push_type(type_enum("Function_Parameter_Mode", (Enum_Type_Item[]){
    { "Call", 0 },
    { "Body", 1 },
  }));

  push_type(type_enum("Operator_Fixity", (Enum_Type_Item[]){
    { "Infix", 1 << 0 },
    { "Prefix", 1 << 1 },
    { "Postfix", 1 << 2 },
  }));

  push_type(type_enum("Operator_Associativity", (Enum_Type_Item[]){
    { "Left", 0 },
    { "Right", 1 },
  }));

  push_type(type_struct("Function_Layout", (Struct_Item[]){
    { "s32", "stack_reserve" },
    { "u8", "stack_allocation_offset_in_prolog" },
    { "u8", "size_of_prolog" },
    { "u16", "_padding" },
    { "u32", "begin_rva" },
    { "u32", "end_rva" },
    { "u8", "volatile_register_push_offsets", 16 },
  }));

  push_type(type_enum("Execution_Context_Flags", (Enum_Type_Item[]){
    { "None", 0 },
    { "Global", 1 << 0 },
  }));

  export_compiler(push_type(type_struct("Execution_Context", (Struct_Item[]){
    { "Allocator *", "allocator" },
    { "Allocator *", "temp_allocator" },
    { "const Value *", "current_compile_time_function_call_target" },
    { "Execution_Context_Flags", "flags" },
    { "s32", "_flags_padding" },
    { "Compilation *", "compilation" },
    { "u64", "epoch" },
    { "Program *", "program" },
    { "Scope *", "scope" },
    { "Module *", "module" },
    { "Mass_Result *", "result" },
  })));

  push_type(type_struct("User_Defined_Operator", (Struct_Item[]){
    { "Operator_Fixity", "fixity" },
    { "u32", "argument_count" },
    { "Slice", "argument_names", 2 },
    { "Value *", "body" },
    { "Scope *", "scope" },
  }));

  push_type(type_struct("Operator", (Struct_Item[]){
    { "Operator_Fixity", "fixity" },
    { "Operator_Associativity", "associativity" },
    { "u64", "precedence" },
    { "u64", "argument_count" },
    { "Mass_Handle_Operator_Proc", "handler" },
    { "void *", "handler_payload" },
  }));

  push_type(add_common_fields(type_union("Macro_Pattern", (Struct_Type[]){
    struct_empty("Any_Token_Sequence"),
    struct_fields("Single_Token", (Struct_Item[]){
      { "Token_Pattern", "token_pattern" },
    }),
  }), (Struct_Item[]){
    { "Slice", "capture_name" },
  }));

  push_type(type_struct("Macro", (Struct_Item[]){
    { "Array_Macro_Pattern", "pattern" },
    { "Value_View", "replacement" },
    { "Scope *", "scope" },
  }));

  push_type(type_struct("Token_Statement_Matcher", (Struct_Item[]){
    { "Token_Statement_Matcher_Proc", "proc" },
    { "void *", "payload" },
  }));

  push_type(add_common_fields(type_union("Scope_Entry", (Struct_Type[]){
    struct_fields("Value", (Struct_Item[]){
      { "u64", "forced" },
      { "Value *", "value" },
    }),
    struct_fields("Operator", (Struct_Item[]){
      { "Operator *", "maybe_prefix" },
      { "Operator *", "maybe_infix_or_postfix" },
    }),
  }), (Struct_Item[]){
    { "Slice", "name" },
    { "u64", "epoch" },
    { "Source_Range", "source_range" },
  }));

  push_type(type_struct("Scope", (Struct_Item[]){
    { "const Allocator *", "allocator" },
    { "u64", "id" },
    { "const Scope *", "parent" },
    { "Scope_Map *", "map" },
    { "Array_Macro_Ptr", "macros" },
    { "Array_Token_Statement_Matcher", "statement_matchers" },
  }));

  push_type(type_struct("Overload_Set", (Struct_Item[]){
    { "Array_Value_Ptr", "items" },
  }));

  push_type(type_struct("Overload_Set_Iterator", (Struct_Item[]){
    { "const Overload_Set *", "set_stack", 16 },
    { "s64", "last_stack_index" },
    { "u64", "index_in_set" },
  }));

  push_type(type_union("Overload_Match", (Struct_Type[]){
    struct_empty("No_Match"),
    struct_fields("Undecidable", (Struct_Item[]){
      { "Value *", "a" },
      { "Value *", "b" },
    }),
    struct_fields("Found", (Struct_Item[]){
      { "Value *", "value" },
    }),
  }));

  export_compiler(push_type(type_struct("Value", (Struct_Item[]){
    { "const Descriptor *", "descriptor" },
    { "Storage", "storage" },
    { "u64", "is_temporary" },
    { "Source_Range", "source_range" },
  })));

  push_type(type_enum("Expected_Result_Storage", (Enum_Type_Item[]){
    { "None", 0 },
    { "Static", 1 << 0 },
    { "Memory", 1 << 1 },
    { "Register", 1 << 2 },
    { "Xmm", 1 << 3 },
    { "Eflags", 1 << 4 },
    { "Unpacked", 1 << 5 },
  }));

  push_type(type_union("Expected_Result", (Struct_Type[]){
    struct_fields("Exact", (Struct_Item[]){
      { "Value *", "value" },
    }),
    struct_fields("Flexible", (Struct_Item[]){
      { "const Descriptor *", "descriptor" },
      { "Expected_Result_Storage", "storage" },
      { "s32", "_storage_padding" },
      { "u64", "register_bitset"},
    }),
  }));

  push_type(type_function(Typedef, "Lazy_Value_Proc", "Value *", (Argument_Type[]){
    { "Execution_Context *", "context" },
    { "Function_Builder *", "builder" },
    { "const Expected_Result *", "expected_result" },
    { "void *", "payload" },
  }));

  push_type(type_struct("Lazy_Value", (Struct_Item[]){
    { "Execution_Context", "context" },
    { "const Descriptor *", "descriptor" },
    { "Lazy_Value_Proc", "proc" },
    { "void *", "payload" },
    { "u64", "epoch" },
  }));

  push_type(type_struct("Lazy_Static_Value", (Struct_Item[]){
    { "Execution_Context", "context" },
    { "Value_View", "expression" },
    { "u64", "resolving" },
  }));

  push_type(type_function(Typedef, "Mass_Handle_Operator_Proc", "Value *", (Argument_Type[]){
    { "Execution_Context *", "context" },
    { "Value_View", "view" },
    { "void *", "payload" },
  }));

  push_type(type_enum("Memory_Layout_Item_Flags", (Enum_Type_Item[]){
    { "None", 0 },
    { "Uninitialized", 1 << 0 },
  }));

  push_type(add_common_fields(type_union("Memory_Layout_Item", (Struct_Type[]){
    struct_fields("Absolute", (Struct_Item[]){
      { "Storage", "storage" },
    }),
    struct_fields("Base_Relative", (Struct_Item[]){
      { "s64", "offset" },
    }),
  }), (Struct_Item[]){
    { "Memory_Layout_Item_Flags", "flags" },
    { "u32", "_flags_padding" },
    { "Slice", "name" },
    { "const Descriptor *", "descriptor" },
    { "Source_Range", "source_range" },
  }));

  push_type(type_struct("Memory_Layout", (Struct_Item[]){
    { "Array_Memory_Layout_Item", "items" },
  }));

  push_type(type_struct("Function_Return", (Struct_Item[]){
    { "Slice", "name" },
    { "const Descriptor *", "descriptor" },
    { "Source_Range", "source_range" },
  }));

  push_type(type_struct("Function_Parameter", (Struct_Item[]){
    { "Slice", "name" },
    { "const Descriptor *", "descriptor" },
    { "Source_Range", "source_range" },
    { "Value_View", "maybe_default_expression" },
  }));

  push_type(type_enum("Descriptor_Function_Flags", (Enum_Type_Item[]){
    { "None", 0 },
    { "Macro", 1 << 0 },
    { "Compile_Time", 1 << 2 },
    { "Intrinsic", 1 << 3},
  }));

  push_type(type_struct("Function_Info", (Struct_Item[]){
    { "Descriptor_Function_Flags", "flags" },
    { "u32", "_flags_padding" },
    { "Array_Function_Parameter", "parameters" },
    { "Scope *", "scope" },
    { "Function_Return", "returns" },
  }));

  push_type(type_struct("Function_Literal", (Struct_Item[]){
    { "Function_Info *", "info" },
    { "Value *", "body" },
    { "Value *", "runtime_instance"},
    { "Value *", "compile_time_instance"},
  }));

  push_type(type_enum("Function_Call_Setup_Flags", (Enum_Type_Item[]){
    { "None", 0 },
    { "Indirect_Return", 1 << 0 },
  }));

  push_type(type_struct("Function_Call_Setup", (Struct_Item[]){
    { "Function_Call_Setup_Flags", "flags"},
    { "u32", "parameters_stack_size"},
    { "const Calling_Convention *", "calling_convention" },
    { "Memory_Layout", "arguments_layout" },
    { "Value *", "caller_return_value" },
    { "Value *", "callee_return_value" },
  }));

  export_compiler(push_type(add_common_fields(type_union("Descriptor", (Struct_Type[]){
    struct_empty("Opaque"),
    struct_fields("Function_Instance", (Struct_Item[]){
      { "Function_Info *", "info" },
      { "Function_Call_Setup", "call_setup"},
    }),
    struct_fields("Fixed_Size_Array", (Struct_Item[]){
      { "const Descriptor *", "item" },
      { "u64", "length" },
    }),
    struct_fields("Struct", (Struct_Item[]){
      { "Memory_Layout", "memory_layout" },
    }),
    struct_fields("Pointer_To", (Struct_Item[]){
      { "const Descriptor *", "descriptor" },
    }),
    struct_fields("Reference_To", (Struct_Item[]){
      { "const Descriptor *", "descriptor" },
    }),
  }), (Struct_Item[]){
    { "Slice", "name" },
    { "u64", "bit_size" },
    { "u64", "bit_alignment" },
  })));

  export_compiler_custom_name("Error", push_type(add_common_fields(type_union("Mass_Error", (Struct_Type[]){
    struct_empty("Unimplemented"),
    struct_empty("Parse"),
    struct_fields("User_Defined", (Struct_Item[]){
      { "Slice", "name" },
    }),
    struct_fields("Circular_Dependency", (Struct_Item[]){
      { "Slice", "name" },
      { "Source_Range", "previous_source_range" },
    }),
    struct_empty("Non_Trailing_Default_Argument"),
    struct_empty("Expected_Static"),
    struct_fields("Integer_Range", (Struct_Item[]){
      { "const Descriptor *", "descriptor" },
    }),
    struct_fields("File_Open", (Struct_Item[]){
      { "Slice", "path" },
    }),
    struct_fields("File_Too_Large", (Struct_Item[]){
      { "Slice", "path" },
    }),
    struct_fields("Dynamic_Library_Load", (Struct_Item[]){
      { "Slice", "library_name" },
    }),
    struct_fields("Dynamic_Library_Symbol_Not_Found", (Struct_Item[]){
      { "Slice", "library_name" },
      { "Slice", "symbol_name" },
    }),
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
      { "u64", "is_operator" },
    }),
    struct_fields("Redifinition", (Struct_Item[]){
      { "Slice", "name" },
      { "Source_Range", "previous_source_range" },
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
    struct_empty("Epoch_Mismatch"),
    struct_fields("No_Matching_Overload", (Struct_Item[]){
      { "Value *", "target" },
      { "Array_Value_Ptr", "arguments" },
    }),
    struct_fields("Undecidable_Overload", (Struct_Item[]){
      { "Value *", "a" },
      { "Value *", "b" },
    }),
  }), (Struct_Item[]){
    { "Slice", "detailed_message" },
    { "Source_Range", "source_range" },
    { "Source_Range", "other_source_range" },
  })));

  export_compiler_custom_name("Result", push_type(type_union("Mass_Result", (Struct_Type[]){
    struct_empty("Success"),
    struct_fields("Error", (Struct_Item[]){
      { "Mass_Error", "error" },
    })
  })));

  push_type(type_struct("Program", (Struct_Item[]){
    { "Array_Import_Library", "import_libraries" },
    { "Array_Label", "labels" },
    { "Array_Label_Location_Diff_Patch_Info", "patch_info_array" },
    { "Array_Value_Ptr", "startup_functions" },
    { "Array_Relocation", "relocations" },
    { "Value *", "entry_point" },
    { "Array_Function_Builder", "functions" },
    { "Program_Memory", "memory" },
    { "const Calling_Convention *", "default_calling_convention"},
  }));

  push_type(type_function(Typedef, "Calling_Convention_Call_Setup_Proc", "Function_Call_Setup", (Argument_Type[]){
    { "const Allocator *", "allocator" },
    { "const Function_Info *", "function_info" },
  }));

  push_type(type_struct("Calling_Convention", (Struct_Item[]){
    { "u64", "register_volatile_bitset" },
    { "Calling_Convention_Call_Setup_Proc", "call_setup_proc"},
  }));

  push_type(type_function(Typedef, "Token_Statement_Matcher_Proc", "u64", (Argument_Type[]){
    { "Execution_Context *", "context" },
    { "Value_View", "view" },
    { "Lazy_Value *", "out_lazy_value" },
    { "void *", "payload" },
  }));

  push_type(type_hash_map("Symbol_Map", {
    .key_type = "Slice",
    .value_type = "Symbol *",
  }));

  push_type(type_hash_map("Scope_Map", {
    .key_type = "Slice",
    .value_type = "Scope_Entry *",
  }));

  push_type(type_hash_map("Macro_Replacement_Map", {
    .key_type = "Slice",
    .value_type = "Value_View",
  }));

  push_type(type_hash_map("Jit_Import_Library_Handle_Map", {
    .key_type = "Slice",
    .value_type = "void *",
  }));

  push_type(type_hash_map("Imported_Module_Map", {
    .key_type = "Slice",
    .value_type = "Module *",
  }));

  push_type(type_struct("Jit_Counters", (Struct_Item[]){
    { "u64", "functions" },
    { "u64", "imports" },
    { "u64", "startup" },
    { "u64", "relocations" },
  }));

  push_type(type_struct("Jit", (Struct_Item[]){
    { "u64", "is_stack_unwinding_in_progress" },
    { "Program *", "program" },
    { "Jit_Import_Library_Handle_Map *", "import_library_handles" },
    { "Jit_Counters", "previous_counts" },
    { "void *", "platform_specific_payload" },
  }));

  push_type(type_hash_map("Static_Pointer_Map", {
    .key_type = "const void *",
    .value_type = "Value",
    .hash_function = "hash_pointer",
    .equal_function = "const_void_pointer_equal",
  }));

  push_type(type_struct("Compilation", (Struct_Item[]){
    { "Virtual_Memory_Buffer", "temp_buffer" },
    { "Allocator *", "temp_allocator" },
    { "Virtual_Memory_Buffer", "allocation_buffer" },
    { "Allocator *", "allocator" },
    { "Jit", "jit" },
    { "Module", "compiler_module" },
    { "Static_Pointer_Map *", "static_pointer_map" },
    { "Imported_Module_Map *", "module_map" },
    { "Scope *", "root_scope" },
    { "Program *", "runtime_program" },
    { "Mass_Result *", "result" },
  }));

  push_type(type_enum("Instruction_Extension_Type", (Enum_Type_Item[]){
    { "None", 0 },
    { "Register", 1 },
    { "Op_Code", 2 },
    { "Plus_Register", 3},
  }));

  push_type(type_enum("Operand_Encoding_Type", (Enum_Type_Item[]){
    { "None", 0},
    { "Eflags", 1},
    { "Register", 2},
    { "Register_A", 3},
    { "Register_Memory", 4},
    { "Xmm", 5},
    { "Xmm_Memory", 6},
    { "Memory", 7},
    { "Immediate", 8},
  }));

  push_type(type_enum("Operand_Size", (Enum_Type_Item[]){
    { "Any", 0 },
    { "8", 1 },
    { "16", 2 },
    { "32", 4 },
    { "64", 8 },
  }));

  push_type(type_struct("Operand_Encoding", (Struct_Item[]){
    { "Operand_Encoding_Type", "type" },
    { "Operand_Size", "size" },
  }));

  push_type(type_struct("Instruction_Encoding", (Struct_Item[]){
    { "u8", "op_code", 4 },
    { "Instruction_Extension_Type", "extension_type" },
    { "u8", "op_code_extension" },
    { "u8", "_op_code_extension_padding", 3 },
    { "Operand_Encoding", "operands", 3 },
  }));

  push_type(type_struct("X64_Mnemonic", (Struct_Item[]){
    { "const char *", "name" },
    { "const Instruction_Encoding *", "encoding_list" },
    { "u64", "encoding_count" },
  }));

  export_compiler(push_type(type_function(Default, "tokenize", "Mass_Result", (Argument_Type[]){
    { "Compilation *", "compilation" },
    { "Source_File *", "file" },
    { "Value_View *", "out_tokens" },
  })));

  export_global_custom_name("address_of", push_type(type_intrinsic("mass_address_of")));

  export_global_custom_name("import", push_type(
    type_function(Compile_Time, "mass_import", "Scope", (Argument_Type[]){
      { "Slice", "name" },
      { "Execution_Context *", "context", "@context" },
    })
  ));

  export_compiler_custom_name("number_literal_logical_shift_left", push_type(
    type_function(Compile_Time, "mass_number_literal_logical_shift_left", "Number_Literal", (Argument_Type[]){
      { "Number_Literal", "input" },
      { "Number_Literal", "shift" },
    })
  ));

  export_compiler_custom_name("number_literal_logical_shift_right", push_type(
    type_function(Compile_Time, "mass_number_literal_logical_shift_right", "Number_Literal", (Argument_Type[]){
      { "Number_Literal", "input" },
      { "Number_Literal", "shift" },
    })
  ));

  export_compiler_custom_name("number_literal_bitwise_or", push_type(
    type_function(Compile_Time, "mass_number_literal_bitwise_or", "Number_Literal", (Argument_Type[]){
      { "Number_Literal", "a" },
      { "Number_Literal", "b" },
    })
  ));

  export_compiler_custom_name("number_literal_bitwise_and", push_type(
    type_function(Compile_Time, "mass_number_literal_bitwise_and", "Number_Literal", (Argument_Type[]){
      { "Number_Literal", "a" },
      { "Number_Literal", "b" },
    })
  ));

  #define DEFINE_ARITHMETIC(_NAME_)\
    export_compiler_custom_name(#_NAME_, push_type(type_intrinsic("mass_" #_NAME_)))

  DEFINE_ARITHMETIC(add);
  DEFINE_ARITHMETIC(subtract);
  DEFINE_ARITHMETIC(multiply);
  DEFINE_ARITHMETIC(divide);
  DEFINE_ARITHMETIC(remainder);

  // Standard C types
  set_flags(push_type(type_c_opaque("char")), Meta_Type_Flags_No_C_Type);
  set_flags(push_type(type_c_opaque("int")), Meta_Type_Flags_No_C_Type);

  // Prelude Types
  #define PROCESS_PRELUDE_TYPES(F)\
    F(u8) F(u16) F(u32) F(u64)\
    F(s8) F(s16) F(s32) F(s64)\
    F(f32) F(f64)

  #define DEFINE_PRIMITIVE(T)\
    export_global(set_flags(push_type(type_c_opaque(#T)),\
      Meta_Type_Flags_No_C_Type | Meta_Type_Flags_No_Value_Array));

  #define DEFINE_RANGES(T)\
    set_flags(push_type(type_struct("Range_" #T, (Struct_Item[]){\
      { #T, "from" },\
      { #T, "to" },\
    })), Meta_Type_Flags_No_C_Type | Meta_Type_Flags_No_Value_Array);

  PROCESS_PRELUDE_TYPES(DEFINE_PRIMITIVE)
  PROCESS_PRELUDE_TYPES(DEFINE_RANGES)

  export_global(set_flags(push_type(type_struct("Slice", (Struct_Item[]){
    { "u8 *", "bytes" },
    { "u64", "length" },
  })), Meta_Type_Flags_No_C_Type | Meta_Type_Flags_No_Value_Array));

  const char *this_filename = __FILE__;
  File_Info this_file_info;
  assert(file_info_c_string(this_filename, &this_file_info));

  {
    const char *filename = "../generated_types.h";

    File_Info generated_file_info;
    bool info_success = file_info_c_string(filename, &generated_file_info);
    if (
      !info_success ||
      generated_file_info.last_modified_time < this_file_info.last_modified_time
    ) {
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

      fprintf(file, "static Descriptor descriptor_void;\n");
      fprintf(file, "static Descriptor descriptor_void_pointer;\n");

      // The descriptor of descriptors needs to be forward declared
      fprintf(file, "static Descriptor descriptor_descriptor;\n");

      // Also need to define built-in types
      fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(allocator, Allocator);\n");
      fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(virtual_memory_buffer, Virtual_Memory_Buffer);\n");

      for (uint32_t i = 0; i < type_count; ++i) {
        print_mass_descriptor_and_type_forward_declaration(file, &types[i]);
      }
      for (uint32_t i = 0; i < type_count; ++i) {
        print_mass_descriptor_fixed_array_types(file, &types[i]);
      }
      for (uint32_t i = 0; i < type_count; ++i) {
        print_mass_descriptor_and_type(file, &types[i]);
      }
      fprintf(file, "\n#endif // GENERATED_TYPES_H\n");

      fclose(file);
      printf("C types generated at: %s\n", filename);
    } else {
      printf("C types up to date at: %s (skipped)\n", filename);
    }
  }

  {
    const char *filename = "../generated_exports.c";

    File_Info generated_file_info;
    bool info_success = file_info_c_string(filename, &generated_file_info);
    if (
      !info_success ||
      generated_file_info.last_modified_time < this_file_info.last_modified_time
    ) {
      #pragma warning(disable : 4996)
      FILE *file = fopen(filename, "wb");
      if (!file) exit(1);
      fprintf(file, "#include \"source.h\"\n\n");

      fprintf(file,
        "static void\n"
        "compiler_scope_define_exports(\n"
        "  Compilation *compilation,\n"
        "  Scope *scope\n"
        ") {\n"
        "  const Allocator *allocator = compilation->allocator;\n"
        "  (void)allocator;\n"
        "  const Calling_Convention *calling_convention =\n"
        "    compilation->jit.program->default_calling_convention;\n"
        "  (void)calling_convention;\n"
      );
      for (uint32_t i = 0; i < type_count; ++i) {
        Meta_Type *type = &types[i];
        if (type->export_target & Export_Target_Compiler_Module) {
          print_scope_export(file, type);
        }
      }
      fprintf(file, "}\n\n" );

      fprintf(file,
        "static void\n"
        "global_scope_define_exports(\n"
        "  Compilation *compilation,\n"
        "  Scope *scope\n"
        ") {\n"
        "  const Allocator *allocator = compilation->allocator;\n"
        "  (void)allocator;\n"
        "  const Calling_Convention *calling_convention =\n"
        "    compilation->jit.program->default_calling_convention;\n"
        "  (void)calling_convention;\n"
      );
      for (uint32_t i = 0; i < type_count; ++i) {
        Meta_Type *type = &types[i];
        if (type->export_target & Export_Target_Global) {
          print_scope_export(file, type);
        }
      }
      fprintf(file, "}\n\n" );

      fclose(file);
      printf("Mass exports generated at: %s\n", filename);
    } else {
      printf("Mass exports up to date at: %s (skipped)\n", filename);
    }
  }

  {
    const char *filename = "../generated.natvis";

    File_Info generated_file_info;
    bool info_success = file_info_c_string(filename, &generated_file_info);
    if (
      !info_success ||
      generated_file_info.last_modified_time < this_file_info.last_modified_time
    ) {
      #pragma warning(disable : 4996)
      FILE *file = fopen(filename, "wb");
      if (!file) exit(1);
      fprintf(file, "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
      fprintf(file, "<AutoVisualizer xmlns=\"http://schemas.microsoft.com/vstudio/debugger/natvis/2010\">\n");

      for (uint32_t i = 0; i < type_count; ++i) {
        Meta_Type *type = &types[i];
        print_natvis(file, type);
      }
      fprintf(file, "</AutoVisualizer>\n");

      fclose(file);
      printf("MSVC native visualizers generated at: %s\n", filename);
    } else {
      printf("MSVC native visualizers up to date at: %s (skipped)\n", filename);
    }
  }
  return 0;
}