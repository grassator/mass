#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <limits.h>

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

typedef enum {
  Meta_Integer_Type_Unsigned = false,
  Meta_Integer_Type_Signed = true,
} Meta_Integer_Type;

typedef struct {
  bool is_signed;
} Meta_Integer;

typedef enum {
  Meta_Type_Tag_Raw,
  Meta_Type_Tag_C_Opaque,
  Meta_Type_Tag_Struct,
  Meta_Type_Tag_Tagged_Union,
  Meta_Type_Tag_Enum,
  Meta_Type_Tag_Float,
  Meta_Type_Tag_Integer,
  Meta_Type_Tag_Function,
  Meta_Type_Tag_Hash_Map,
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
    Meta_Integer integer;
    Function_Type function;
    Hash_Map_Type hash_map;
  };
} Meta_Type;

static char *
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

static char *
strjoin(
  const char *a,
  const char *b
) {
  size_t a_length = strlen(a);
  size_t b_length = strlen(b);
  size_t length = a_length + b_length + 1;
  char *result = malloc(length);
  memcpy(result, a, a_length);
  memcpy(result + a_length, b, b_length);
  result[length - 1] = 0;
  return result;
}

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
        name = type->name;
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
      name = type->name;
      if (!(type->flags & Meta_Type_Flags_No_C_Type)) {
        fprintf(file, "typedef struct %s %s;\n", type->name, type->name);

        // Write out individual structs
        for (uint64_t i = 0; i < type->union_.item_count; ++i) {
          Struct_Type *struct_ = &type->union_.items[i];
          if (!struct_->item_count) continue;
          fprintf(file, "typedef struct %s_%s %s_%s;\n", type->name, struct_->name, type->name, struct_->name);
        }
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
    case Meta_Type_Tag_Float:
    case Meta_Type_Tag_Integer:
    case Meta_Type_Tag_C_Opaque:
    case Meta_Type_Tag_Raw: {
      name = type->name;
      break;
    }
    default: {
      name = 0;
    } break;
  }
  if (name) {
    fprintf(file, "typedef dyn_array_type(%s *) Array_%s_Ptr;\n", name, name);
    fprintf(file, "typedef dyn_array_type(const %s *) Array_Const_%s_Ptr;\n", name, name);
  }
  fprintf(file, "\n");
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
  fprintf(file, "static inline const %s_%s *\n", union_name, variant_name); // type signature
  fprintf(file, "%s_as_%s", lower_union_name, lower_variant_name); // fn name
  fprintf(file, "(const %s *%s) {\n", union_name, lower_union_name); // args
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
      // Handled in forward declarations
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
    case Meta_Type_Tag_Float:
    case Meta_Type_Tag_Integer:
    case Meta_Type_Tag_Raw:
    case Meta_Type_Tag_C_Opaque: {
      if (!(type->flags & Meta_Type_Flags_No_Value_Array)) {
        fprintf(file, "typedef dyn_array_type(%s) Array_%s;\n\n", type->name, type->name);
      }
      break;
    }
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
  fprintf(file, "  Source_Range %s__source_range;\n", export_name);
  fprintf(file, "  INIT_LITERAL_SOURCE_RANGE(&%s__source_range, \"%s\");\n", export_name, export_name);
  fprintf(file, "  scope_define_value(\n");
  fprintf(file, "    scope, VALUE_STATIC_EPOCH, %s__source_range,\n", export_name);
  fprintf(file, "    mass_ensure_symbol(compilation, slice_literal(\"%s\")),\n", export_name);
  fprintf(file, "    type_%s_value\n", lowercase_name);
  fprintf(file, "  );\n");
}

static void
print_scope_enum(
  FILE *file,
  const char *name,
  const char *export_name
) {
  char *lowercase_name = strtolower(name);
  fprintf(file, "  Source_Range %s__source_range;\n", export_name);
  fprintf(file, "  INIT_LITERAL_SOURCE_RANGE(&%s__source_range, \"%s\");\n", export_name, export_name);
  fprintf(file, "  scope_define_enum(\n");
  fprintf(file, "    compilation, scope, %s__source_range,\n", export_name);
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
  // Mass userland should not be aware that signed and unsigned C integer types
  // exists and most of the time it doesn't care anyway. So we are just swapping
  // them for the one that does not have a sign attached.
  if (
    slice_equal(lowercase_type, slice_literal("s8")) ||
    slice_equal(lowercase_type, slice_literal("u8"))
  ) {
    lowercase_type = slice_literal("i8");
  } else if (
    slice_equal(lowercase_type, slice_literal("s16")) ||
    slice_equal(lowercase_type, slice_literal("u16"))
  ) {
    lowercase_type = slice_literal("i16");
  } else if (
    slice_equal(lowercase_type, slice_literal("s32")) ||
    slice_equal(lowercase_type, slice_literal("u32"))
  ) {
    lowercase_type = slice_literal("i32");
  } else if (
    slice_equal(lowercase_type, slice_literal("s64")) ||
    slice_equal(lowercase_type, slice_literal("u64"))
  ) {
    lowercase_type = slice_literal("i64");
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

  if (function->kind == Function_Kind_Typedef) {
    print_scope_define(file, type->name, type->export_name);
    return;
  }

  fprintf(file, "  MASS_DEFINE_FUNCTION(\n");
  fprintf(file, "    Function_Info_Flags_None");
  switch(function->kind) {
    case Function_Kind_Default:
    case Function_Kind_Typedef: {
      // Nothing to do
    } break;
    case Function_Kind_Compile_Time: {
      fprintf(file, " | Function_Info_Flags_Compile_Time");
    } break;
    case Function_Kind_Intrinsic: {
      fprintf(file, " | Function_Info_Flags_Intrinsic");

      if (function->argument_count != 3) {
        printf("Intrinsic %s must have 3 arguments", function->name);
        exit(1);
      }
      {
        const char *expected = "Mass_Context *";
        if (strcmp(function->arguments[0].type, expected) != 0) {
          printf("Intrinsic %s first argument must have type %s", function->name, expected);
          exit(1);
        }
      }
      {
        const char *expected = "Parser *";
        if (strcmp(function->arguments[1].type, expected) != 0) {
          printf("Intrinsic %s second argument must have type %s", function->name, expected);
          exit(1);
        }
      }
      {
        const char *expected = "Value_View";
        if (strcmp(function->arguments[2].type, expected) != 0) {
          printf("Intrinsic %s third argument must have type %s", function->name, expected);
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
    fprintf(file, "    (Resolved_Function_Parameter) {\n");
    fprintf(file, "      .symbol = mass_ensure_symbol(compilation, slice_literal(\"%s\")),\n", arg->name);
    fprintf(file, "      .descriptor = &");
    print_mass_struct_descriptor_type(file, arg->type);
    fprintf(file, "\n    }");
  }
  fprintf(file, "\n  );\n");
}

static void
print_scope_export(
  FILE *file,
  Meta_Type *type
) {
  switch(type->tag) {
    case Meta_Type_Tag_Integer:
    case Meta_Type_Tag_Float:
    case Meta_Type_Tag_C_Opaque:
    case Meta_Type_Tag_Raw:
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
      if (!(type->flags & Meta_Type_Flags_No_Value_Array)) {
        print_natvis_array_types(file, type->struct_.name);
      }
      break;
    }
    case Meta_Type_Tag_Tagged_Union: {
      if (!(type->flags & Meta_Type_Flags_No_Value_Array)) {
        print_natvis_array_types(file, type->name);
      }

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
    case Meta_Type_Tag_Integer:
    case Meta_Type_Tag_Float:
    case Meta_Type_Tag_C_Opaque:
    case Meta_Type_Tag_Raw:
    case Meta_Type_Tag_Enum:
    case Meta_Type_Tag_Function: {
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
    case Meta_Type_Tag_Integer:
    case Meta_Type_Tag_Float:
    case Meta_Type_Tag_C_Opaque:
    case Meta_Type_Tag_Raw:
    case Meta_Type_Tag_Enum:
    case Meta_Type_Tag_Function:
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
      fprintf(file, "static Descriptor descriptor_array_const_%s_ptr;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_%s_pointer;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_%s_pointer_pointer;\n", lowercase_name);
      break;
    }
    case Meta_Type_Tag_Tagged_Union: {
      char *lowercase_name = strtolower(type->name);
      fprintf(file, "static Descriptor descriptor_%s;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_array_%s;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_array_%s_ptr;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_array_const_%s_ptr;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_%s_pointer;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_%s_pointer_pointer;\n", lowercase_name);
      break;
    }
    case Meta_Type_Tag_Integer:
    case Meta_Type_Tag_Float:
    case Meta_Type_Tag_C_Opaque:
    case Meta_Type_Tag_Raw: {
      char *lowercase_name = strtolower(type->name);
      fprintf(file, "static Descriptor descriptor_%s;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_array_%s;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_array_%s_ptr;\n", lowercase_name);
      fprintf(file, "static Descriptor descriptor_array_const_%s_ptr;\n", lowercase_name);
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
  }
}

static void
print_mass_struct_item(
  FILE *file,
  const char *struct_name,
  Struct_Item *item
) {
  Slice name_slice = slice_from_c_string(item->name);
  if (slice_starts_with(name_slice, slice_literal("_"))) {
    if (slice_ends_with(name_slice, slice_literal("padding"))) return;
  }
  fprintf(file, "  {\n");
  fprintf(file, "    .descriptor = &");
  print_mass_struct_descriptor_type(file, item->type);
  if (item->array_length > 1) fprintf(file, "_%u", item->array_length);
  fprintf(file, ",\n");
  fprintf(file, "    .name = slice_literal_fields(\"%s\"),\n", item->name);
  fprintf(file, "    .offset = offsetof(%s, %s),\n", struct_name, item->name);
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
print_mass_dyn_array(
  FILE *file,
  Meta_Type *type
) {
  char *lowercase_name = strtolower(type->name);
  fprintf(file, "MASS_DEFINE_C_DYN_ARRAY_TYPE(array_%s_ptr, %s_pointer, Array_%s_Ptr);\n",
    lowercase_name, lowercase_name, type->name);
  if (!(type->flags & Meta_Type_Flags_No_Value_Array)) {
    fprintf(file, "MASS_DEFINE_C_DYN_ARRAY_TYPE(array_%s, %s, Array_%s);\n",
      lowercase_name, lowercase_name, type->name);
  }
}

static void
print_mass_descriptor_and_type(
  FILE *file,
  Meta_Type *type
) {
  char *lowercase_name = strtolower(type->name);
  switch(type->tag) {
    case Meta_Type_Tag_Struct: {
      print_mass_struct(file, type->name, &type->struct_);
      print_mass_dyn_array(file, type);
      fprintf(file, "DEFINE_VALUE_IS_AS_HELPERS(%s, %s);\n", type->name, lowercase_name);
      fprintf(file, "DEFINE_VALUE_IS_AS_HELPERS(%s *, %s_pointer);\n", type->name, lowercase_name);
      break;
    }
    case Meta_Type_Tag_Float:
    case Meta_Type_Tag_Integer:
    case Meta_Type_Tag_Raw:
    case Meta_Type_Tag_C_Opaque: {
      if (type->tag == Meta_Type_Tag_Float) {
        fprintf(file, "MASS_DEFINE_FLOAT_C_TYPE(%s, %s)\n", lowercase_name, type->name);
      } else if (type->tag == Meta_Type_Tag_Integer) {
        fprintf(file, "MASS_DEFINE_INTEGER_C_TYPE(%s, %s, %i)\n", lowercase_name, type->name, type->integer.is_signed);
      } else if (type->tag == Meta_Type_Tag_Raw) {
        fprintf(file, "MASS_DEFINE_RAW_C_TYPE(%s, %s)\n", lowercase_name, type->name);
      } else {
        fprintf(file, "MASS_DEFINE_OPAQUE_C_TYPE(%s, %s)\n", lowercase_name, type->name);
      }
      print_mass_dyn_array(file, type);
      fprintf(file, "DEFINE_VALUE_IS_AS_HELPERS(%s, %s);\n", type->name, lowercase_name);
      fprintf(file, "DEFINE_VALUE_IS_AS_HELPERS(%s *, %s_pointer);\n", type->name, lowercase_name);
    } break;
    case Meta_Type_Tag_Enum: {
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
      fprintf(file, "DEFINE_VALUE_IS_AS_HELPERS(%s, %s);\n", type->name, lowercase_name);
      fprintf(file, "DEFINE_VALUE_IS_AS_HELPERS(%s *, %s_pointer);\n", type->name, lowercase_name);
      break;
    }
    case Meta_Type_Tag_Tagged_Union: {
      fprintf(file, "/*union struct start */\n");
      print_mass_dyn_array(file, type);

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
        fprintf(file, "    .name = slice_literal_fields(\"tag\"),\n");
        fprintf(file, "    .descriptor = &descriptor_%s_tag,\n", lowercase_name);
        fprintf(file, "    .offset = offsetof(%s, tag),\n", type->name);
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
            fprintf(file, "    .name = slice_literal_fields(\"%s\"),\n", struct_->name);
            fprintf(file, "    .descriptor = &descriptor_%s_%s,\n", lowercase_name, struct_lowercase_name);
            fprintf(file, "    .offset = offsetof(%s, %s),\n", type->name, struct_->name);
            fprintf(file, "  },\n");
          }
        }
        fprintf(file, ");\n");
        fprintf(file, "MASS_DEFINE_TYPE_VALUE(%s);\n", lowercase_name);
      }
      fprintf(file, "DEFINE_VALUE_IS_AS_HELPERS(%s, %s);\n", type->name, lowercase_name);
      fprintf(file, "DEFINE_VALUE_IS_AS_HELPERS(%s *, %s_pointer);\n", type->name, lowercase_name);
      fprintf(file, "/*union struct end*/\n");
      break;
    }
    case Meta_Type_Tag_Function: {
      // Exported functions are handled separately
      Function_Type *function = &type->function;
      if (function->kind == Function_Kind_Typedef) {
        fprintf(file, "MASS_DEFINE_FUNCTION_DESCRIPTOR(\n  %s,\n", lowercase_name);
        {
          fprintf(file, "  &");
          print_mass_struct_descriptor_type(file, function->returns);
        }
        for (uint64_t i = 0; i < function->argument_count; ++i) {
          Argument_Type *arg = &function->arguments[i];
          fprintf(file, ",\n  {\n");
          fprintf(file, "    .tag = Resolved_Function_Parameter_Tag_Unknown,\n");
          {
            fprintf(file, "    .descriptor = &");
            print_mass_struct_descriptor_type(file, arg->type);
            fprintf(file, ",\n");
          }
          fprintf(file, "  }");
        }
        fprintf(file, "\n)\n");
      }
      break;
    }
    case Meta_Type_Tag_Hash_Map: {
      // TODO
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

#define type_c_opaque(_NAME_STRING_)\
  (Meta_Type){\
    .tag = Meta_Type_Tag_C_Opaque,\
    .name = (_NAME_STRING_),\
  }

#define type_float(_NAME_STRING_)\
  (Meta_Type){\
    .tag = Meta_Type_Tag_Float,\
    .name = (_NAME_STRING_),\
  }

#define type_integer(_NAME_STRING_, _SIGNEDNESS_)\
  (Meta_Type){\
    .tag = Meta_Type_Tag_Integer,\
    .name = (_NAME_STRING_),\
    .integer.is_signed = (bool)(_SIGNEDNESS_),\
  }

#define type_raw(_NAME_STRING_)\
  (Meta_Type){\
    .tag = Meta_Type_Tag_Raw,\
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
    { "Mass_Context *", "context" },\
    { "Parser *", "parser" },\
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
  export_compiler(push_type(type_struct("Bits", (Struct_Item[]){
    { "u64", "as_u64" },
  })));

  push_type(type_struct("Source_Position", (Struct_Item[]){
    { "u64", "line" },
    { "u64", "column" },
  }));

  push_type(type_struct("Source_File", (Struct_Item[]){
    { "Slice", "path" },
    { "Slice", "text" },
  }));

  export_compiler(push_type(type_struct("Source_Range", (Struct_Item[]){
    { "const Source_File *", "file" },
    { "Range_u32", "offsets" },
  })));

  push_type(type_struct("Mass_While", (Struct_Item[]){
    { "Value *", "condition" },
    { "Value *", "body" },
  }));

  push_type(type_struct("Assignment", (Struct_Item[]){
    { "Value *", "target" },
    { "Value *", "source" },
  }));

  push_type(add_common_fields(type_union("Module_Exports", (Struct_Type[]){
    struct_empty("Not_Specified"),
    struct_empty("All"),
    struct_fields("Selective", (Struct_Item[]){
      { "const Tuple *", "tuple" },
    }),
  }), (Struct_Item[]){
    { "Scope *", "scope" },
    { "Source_Range", "source_range" },
  }));

  export_compiler(push_type(type_struct("Module", (Struct_Item[]){
    { "Source_Range", "source_range" },
    { "Scope *", "own_scope" },
    { "Module_Exports", "exports" },
  })));

  push_type(type_struct("Parse_Error", (Struct_Item[]){
    { "Slice", "message" },
    { "Source_Range", "source_range" },
  }));

  export_compiler(push_type(type_struct("Value_View", (Struct_Item[]){
    { "Value * *", "values" },
    { "u32", "length" },
    { "u32", "_length_padding" },
    { "Source_Range", "source_range" },
  })));

  export_compiler(push_type(type_struct("Symbol", (Struct_Item[]){
    { "Slice", "name" },
  })));

  export_compiler(push_type(type_struct("Group_Paren", (Struct_Item[]){
    { "Value_View", "children" },
  })));

  export_compiler(push_type(type_struct("Ast_Statement", (Struct_Item[]){
    { "Value_View", "children" },
    { "Ast_Statement *", "next" },
  })));

  export_compiler(push_type(type_struct("Ast_Block", (Struct_Item[]){
    { "Ast_Statement *", "first_statement" },
    { "Ast_Statement *", "last_statement" },
  })));

  export_compiler(push_type(type_struct("Group_Square", (Struct_Item[]){
    { "Value_View", "children" },
  })));

  push_type(type_struct("Ast_Using", (Struct_Item[]){
    { "const Module *", "module" },
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

  // @Volatile :RegisterEnumValues
  export_compiler(push_type(type_enum("Register", (Enum_Type_Item[]){
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
  })));

  push_type(type_struct("Label", (Struct_Item[]){
    { "u32", "resolved" },
    { "u32", "offset_in_section" },
    { "Slice", "name" },
    { "Section *", "section" },
    { "const Program *", "program" },
  }));

  push_type(type_struct("Label_Location_Diff_Patch_Info", (Struct_Item[]){
    { "Label *", "target" },
    { "Label", "from" },
    { "void *", "patch32_at" },
    { "s32", "offset_from_label" },
    { "u32", "_offset_from_label_padding" },
  }));

  push_type(type_enum("Number_Base", (Enum_Type_Item[]){
    { "2", 2 },
    { "10", 10 },
    { "16", 16 },
  }));

  export_compiler(push_type(type_struct("Quoted", (Struct_Item[]){
    { "Value *", "value" },
  })));

  export_compiler(push_type(type_struct("Named_Accessor", (Struct_Item[]){
    { "const Symbol *", "symbol" },
  })));

  export_compiler(push_type(type_struct("External_Symbol", (Struct_Item[]){
    { "Slice", "library_name" },
    { "Slice", "symbol_name" },
  })));

  push_type(type_struct("Import_Symbol", (Struct_Item[]){
    { "Slice", "name" },
    { "Label *", "label32" },
  }));

  push_type(type_struct("Import_Library", (Struct_Item[]){
    { "Slice", "name" },
    { "Array_Import_Symbol", "symbols" },
  }));

  export_compiler(push_type(type_enum("Compare_Type", (Enum_Type_Item[]){
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
  })));

  push_type(type_enum("Stack_Area", (Enum_Type_Item[]){
    { "Local", 0 },
    { "Received_Argument", 1 },
    { "Call_Target_Argument", 2 },
  }));

  push_type(type_union("Memory_Location", (Struct_Type[]){
    struct_fields("Instruction_Pointer_Relative", (Struct_Item[]){
      { "Label *", "label" },
      { "s64", "offset" },
    }),
    struct_fields("Indirect", (Struct_Item[]){
      { "Register", "base_register" },
      { "s32", "offset" },
    }),
    struct_fields("Stack", (Struct_Item[]){
      { "Stack_Area", "area" },
      { "s32", "offset" },
    }),
  }));

  export_compiler(push_type(type_enum("Storage_Flags", (Enum_Type_Item[]){
    { "None", 0 },
    { "Temporary", 1 << 0 },
  })));

  export_compiler(push_type(add_common_fields(type_union("Storage", (Struct_Type[]){
    struct_fields("Immediate", (Struct_Item[]){
      { "u64", "bits" },
    }),
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
      { "const void *", "pointer" },
    }),
    struct_fields("Memory", (Struct_Item[]){
      { "Memory_Location", "location" },
    }),
    struct_fields("Disjoint", (Struct_Item[]){
      { "Array_Storage_Ptr", "pieces" },
    }),
  }), (Struct_Item[]){
    { "Storage_Flags", "flags" },
    { "u32", "_flags_padding" },
    { "Bits", "bit_size" },
  })));

  push_type(type_struct("Relocation", (Struct_Item[]){
    { "Storage", "patch_at" },
    { "Storage", "address_of" },
  }));

  push_type(type_struct("Instruction_Assembly", (Struct_Item[]){
    { "const X64_Mnemonic *", "mnemonic" },
    { "Storage", "operands", 3 },
  }));

  export_compiler(push_type(add_common_fields(type_union("Instruction", (Struct_Type[]){
    struct_fields("Label", (Struct_Item[]){
      { "Label *", "pointer" },
    }),
    struct_fields("Bytes", (Struct_Item[]){
      { "u8", "memory", 15 },
      { "u8", "length" },
    }),
    struct_fields("Label_Patch", (Struct_Item[]){
      { "s32", "offset_in_instruction" },
      { "s32", "offset_from_label" },
      { "Label *", "label" },
    }),
    struct_fields("Stack_Patch", (Struct_Item[]){
      { "s32", "mod_r_m_offset_in_previous_instruction" },
      { "Stack_Area", "stack_area" },
    }),
    struct_fields("Location", (Struct_Item[]){
      { "Source_Range", "source_range" },
    }),
  }), (Struct_Item[]){
    { "Scope *", "scope" },
  })));

  push_type(type_struct("Instruction_Bucket", (Struct_Item[]){
    { "Instruction", "items", 15},
    { "u64", "length" },
    { "Instruction_Bucket *", "next" },
  }));

  push_type(type_struct("Code_Block", (Struct_Item[]){
    { "const Allocator *", "allocator" },
    { "Label *", "start_label" },
    { "Label *", "end_label" },
    { "Instruction_Bucket *", "first_bucket" },
    { "Instruction_Bucket *", "last_bucket" },
  }));

  push_type(type_struct("Epoch", (Struct_Item[]){
    { "u64", "as_u64" },
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

  export_compiler_custom_name("Context", push_type(type_struct("Mass_Context", (Struct_Item[]){
    { "Allocator *", "allocator" },
    { "Allocator *", "temp_allocator" },
    { "Compilation *", "compilation" },
    { "Program *", "program" },
    { "Mass_Result *", "result" },
  })));

  push_type(type_enum("Parser_Flags", (Enum_Type_Item[]){
    { "None", 0 },
    { "Global", 1 << 0 },
    { "Type_Only", 1 << 1 },
  }));

  export_compiler(push_type(type_struct("Parser", (Struct_Item[]){
    { "Parser_Flags", "flags" },
    { "s32", "_flags_padding" },
    { "Epoch", "epoch" },
    { "Scope *", "scope" },
    { "Module *", "module" },
  })));

  push_type(type_enum("Operator_Fixity", (Enum_Type_Item[]){
    { "Infix", 1 << 0 },
    { "Prefix", 1 << 1 },
    { "Postfix", 1 << 2 },
  }));

  push_type(type_enum("Operator_Associativity", (Enum_Type_Item[]){
    { "Left", 0 },
    { "Right", 1 },
  }));

  push_type(type_enum("Operator_Flags", (Enum_Type_Item[]){
    { "None", 0 },
    { "Optional_Rhs", 1 << 0 },
  }));

  push_type(add_common_fields(type_union("Operator", (Struct_Type[]){
    struct_fields("Alias", (Struct_Item[]){
      { "const Symbol *", "symbol" },
    }),
    struct_fields("Intrinsic", (Struct_Item[]){
      { "Value *", "body" },
    }),
  }), (Struct_Item[]){
    { "Operator_Flags", "flags" },
    { "Operator_Fixity", "fixity" },
    { "Operator_Associativity", "associativity" },
    { "u32", "precedence" },
  }));

  push_type(type_struct("Scope_Entry", (Struct_Item[]){
    { "Value *", "value" },
    { "Slice", "name" },
    { "Epoch", "epoch" },
    { "const Value *", "latest_forced_value" },
    { "Source_Range", "source_range" },
  }));

  push_type(type_hash_map("Operator_Map", {
    .key_type = "const Symbol *",
    .value_type = "Operator *",
    .hash_function = "hash_pointer",
    .equal_function = "const_void_pointer_equal",
  }));

  push_type(type_hash_map("Operator_Symbol_Map", {
    .key_type = "const Symbol *",
    .value_type = "const Symbol *",
    .hash_function = "hash_pointer",
    .equal_function = "const_void_pointer_equal",
  }));

  export_compiler(push_type(type_struct("Scope", (Struct_Item[]){
    { "const Allocator *", "allocator" },
    { "const Scope *", "parent" },
    { "Scope_Map *", "map" },
  })));

  push_type(type_struct("Overload", (Struct_Item[]){
    { "Value *", "value" },
    { "Value *", "next" },
  }));

  push_type(type_struct("Undecidable_Match", (Struct_Item[]){
    { "const Function_Info *", "info" },
    { "Value *", "value" },
  }));

  push_type(type_union("Overload_Match", (Struct_Type[]){
    struct_empty("No_Match"),
    struct_fields("Undecidable", (Struct_Item[]){
      { "Array_Undecidable_Match", "matches" },
    }),
    struct_fields("Found", (Struct_Item[]){
      { "Value *", "value" },
      { "const Function_Info *", "info" }
    }),
  }));

  push_type(type_struct("Overload_Match_Summary", (Struct_Item[]){
    { "u16", "inverted_generic_count" },
    { "u16", "inverted_cast_count" },
    { "u16", "exact_count" },
    { "_Bool", "compile_time" },
    { "_Bool", "matched" },
  }));

  push_type(type_struct("Overload_Match_State", (Struct_Item[]){
    { "Value *", "value" },
    { "const Function_Info *", "info" },
    { "Overload_Match_Summary", "summary" },
  }));

  export_compiler(push_type(type_enum("Value_Flags", (Enum_Type_Item[]){
    { "None",  0 },
    { "Constant", 1 << 0 },
  })));

  export_compiler(push_type(add_common_fields(type_union("Value", (Struct_Type[]){
    struct_fields("Lazy", (Struct_Item[]){
      { "_Bool", "is_factory" },
      { "u8", "_is_factory_padding", 7 },
      { "Epoch", "epoch" },
      { "const void *", "payload" },
      { "Lazy_Value_Proc", "proc" }
    }),
    struct_fields("Forced", (Struct_Item[]){
      { "Storage", "storage" },
    }),
  }), (Struct_Item[]){
    { "Value_Flags", "flags" },
    { "u32", "_flags_padding" },
    { "const Descriptor *", "descriptor" },
    { "Source_Range", "source_range" },
  })));

  push_type(type_struct("Register_Bitset", (Struct_Item[]){
    { "u64", "bits" },
  }));

  export_compiler(push_type(type_struct("Function_Builder", (Struct_Item[]){
    { "Epoch", "epoch" },
    { "s32", "stack_reserve" },
    { "u32", "max_call_parameters_stack_size" },
    { "Value", "return_value" },
    { "Code_Block", "code_block" },
    { "Register_Bitset", "register_used_bitset" },
    { "Register_Bitset", "register_volatile_bitset" },
    { "Register_Bitset", "register_occupied_bitset" },
    { "Slice", "source" },
    { "const Function_Info *", "function" },
  })));

  export_compiler(push_type(type_union("Expected_Result", (Struct_Type[]){
    struct_fields("Exact", (Struct_Item[]){
      { "const Descriptor *", "descriptor" },
      { "Storage", "storage" },
    }),
    struct_fields("Flexible", (Struct_Item[]){
      { "const Descriptor *", "descriptor" },
    }),
  })));

  push_type(type_struct("Lazy_Static_Value", (Struct_Item[]){
    { "Mass_Context", "context" },
    { "Parser", "parser" },
    { "Value_View", "expression" },
    { "u64", "resolving" },
  }));

  export_compiler_custom_name(
    "Intrinsic_Proc",
    push_type(type_function(Typedef, "Mass_Intrinsic_Proc", "Value *", (Argument_Type[]){
      { "Mass_Context *", "context" },
      { "Parser *", "parser" },
      { "Value_View", "view" },
    }))
  );

  push_type(add_common_fields(type_union("Function_Parameter", (Struct_Type[]){
    struct_empty("Runtime"),
    struct_fields("Generic", (Struct_Item[]){
      { "u64", "is_static" },
      { "Mass_Type_Constraint_Proc", "maybe_type_constraint" },
    }),
    struct_fields("Exact_Static", (Struct_Item[]){
      { "Storage", "storage" },
    }),
  }), (Struct_Item[]){
    { "const Descriptor *", "descriptor" },
    { "const Symbol *", "symbol" },
    { "Source_Range", "source_range" },
    { "Value_View", "maybe_type_expression" },
    { "Value *", "maybe_default_value" },
  }));

  push_type(add_common_fields(type_union("Resolved_Function_Parameter", (Struct_Type[]){
    struct_empty("Unknown"),
    struct_fields("Known", (Struct_Item[]){
      { "Storage", "storage" },
    }),
  }), (Struct_Item[]){
    { "u64", "was_generic" },
    { "const Descriptor *", "descriptor" },
    { "const Symbol *", "symbol" },
    { "Source_Range", "source_range" },
    { "Value *", "maybe_default_value" },
  }));

  push_type(type_enum("Function_Info_Flags", (Enum_Type_Item[]){
    { "None", 0 },
    { "Compile_Time", 1 << 1 },
    { "Intrinsic", 1 << 2},
  }));

  push_type(add_common_fields(type_union("Function_Return", (Struct_Type[]){
    struct_empty("Inferred"),
    struct_fields("Generic", (Struct_Item[]){
      { "Value_View", "type_expression" },
    }),
    struct_fields("Exact", (Struct_Item[]){
      { "const Descriptor *", "descriptor" },
    }),
  }), (Struct_Item[]){
    { "Source_Range", "source_range" },
  }));

  push_type(type_struct("Function_Info", (Struct_Item[]){
    { "Function_Info_Flags", "flags" },
    { "u32", "_flags_padding" },
    { "Array_Resolved_Function_Parameter", "parameters" },
    { "const Descriptor *", "return_descriptor" },
  }));

  push_type(type_enum("Function_Header_Flags", (Enum_Type_Item[]){
    { "None", 0 },
    { "Macro", 1 << 0 },
    { "Intrinsic", 1 << 1 },
    { "Compile_Time", 1 << 2 },
  }));

  push_type(type_struct("Function_Specialization", (Struct_Item[]){
    { "Array_Function_Parameter", "parameters" },
    { "Function_Info *", "info" },
  }));

  export_compiler(push_type(type_struct("Function_Header", (Struct_Item[]){
    { "Function_Header_Flags", "flags"},
    { "u32", "generic_parameter_count"},
    { "Array_Function_Parameter", "parameters"},
    { "Function_Return", "returns"},
  })));

  export_compiler(push_type(type_struct("Function_Literal", (Struct_Item[]){
    { "Function_Header", "header" },
    { "Scope *", "own_scope" },
    { "Value *", "body" },
    { "u64 *", "overload_lock_count"},
    { "Array_Value_Ptr", "instances"},
    { "Array_Function_Specialization", "specializations"},
  })));

  push_type(type_enum("Function_Call_Jump", (Enum_Type_Item[]){
    { "Call", 0 },
    { "Syscall", 1},
  }));

  push_type(type_enum("Function_Call_Parameter_Flags", (Enum_Type_Item[]){
    { "None", 0 },
    { "Uninitialized", 1 << 0 },
    { "Implicit_Pointer", 1 << 1 },
  }));

  push_type(type_struct("Function_Call_Parameter", (Struct_Item[]){
    { "Function_Call_Parameter_Flags", "flags" },
    { "u32", "_flags_padding" },
    { "const Descriptor *", "descriptor" },
    { "Storage", "storage" },
    { "u64", "original_index" },
  }));

  push_type(type_struct("Function_Call_Setup", (Struct_Item[]){
    { "u32", "parameters_stack_size"},
    { "u32", "_parameters_stack_size_padding"},
    { "Function_Call_Jump", "jump"},
    { "u32", "_jump_padding" },
    { "const Calling_Convention *", "calling_convention" },
    { "Array_Function_Call_Parameter", "parameters" },
    { "Register_Bitset", "parameter_registers_bitset" },
    { "Storage", "caller_return" },
    { "Storage", "callee_return" },
  }));

  export_compiler(push_type(type_struct("Tuple", (Struct_Item[]){
    { "Epoch", "epoch" },
    { "const Scope *", "scope_where_it_was_created" },
    { "Array_Value_Ptr", "items"},
  })));

  export_compiler(push_type(type_struct("List_Node", (Struct_Item[]){
    { "const List_Node *", "maybe_previous" },
    { "Value *", "value" },
  })));

  push_type(type_struct("Typed_Symbol", (Struct_Item[]){
    { "const Symbol *", "symbol"},
    { "const Descriptor *", "descriptor"},
  }));

  push_type(type_struct("Struct_Field", (Struct_Item[]){
    // TODO This should probably be "const Symbol *", but generating that is a giant pain
    { "Slice", "name"},
    { "const Descriptor *", "descriptor"},
    { "u64", "offset" },
  }));

  export_compiler(push_type(add_common_fields(type_union("Descriptor", (Struct_Type[]){
    struct_empty("Void"),
    struct_empty("Raw"),
    struct_empty("Float"),
    struct_fields("Integer", (Struct_Item[]){
      { "u64", "is_signed" },
    }),
    struct_fields("Function_Instance", (Struct_Item[]){
      { "const Function_Info *", "info" },
      { "Function_Call_Setup", "call_setup"},
      { "const Program *", "program" },
    }),
    struct_fields("Fixed_Array", (Struct_Item[]){
      { "const Descriptor *", "item" },
      { "u64", "length" },
    }),
    struct_fields("Struct", (Struct_Item[]){
      { "Array_Struct_Field", "fields" },
    }),
    struct_fields("Pointer_To", (Struct_Item[]){
      { "const Descriptor *", "descriptor" },
    }),
  }), (Struct_Item[]){
    { "Module *", "own_module" },
    { "const Symbol *", "brand" },
    { "Bits", "bit_size" },
    { "Bits", "bit_alignment" },
  })));

  push_type(type_function(Typedef, "Mass_Type_Constraint_Proc", "const Descriptor *", (Argument_Type[]){
    { "const Descriptor *", "descriptor" },
  }));

  export_compiler_custom_name("Error", push_type(add_common_fields(type_union("Mass_Error", (Struct_Type[]){
    struct_empty("Unimplemented"),
    struct_empty("Parse"),
    struct_empty("Assignment_To_Constant"),
    struct_fields("User_Defined", (Struct_Item[]){
      { "Slice", "name" },
    }),
    struct_fields("Circular_Dependency", (Struct_Item[]){
      { "Slice", "name" },
    }),
    struct_empty("Non_Trailing_Default_Argument"),
    struct_empty("Expected_Static"),
    struct_empty("Tokenizer"),
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
    struct_fields("Operator_Fixity_Conflict", (Struct_Item[]){
      { "Operator_Fixity", "fixity" },
      { "u32", "_fixity_padding" },
      { "Slice", "symbol" },
    }),
    struct_fields("Undefined_Variable", (Struct_Item[]){
      { "Slice", "name" },
      { "u64", "is_operator" },
    }),
    struct_fields("Redefinition", (Struct_Item[]){
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
    struct_empty("Epoch_Mismatch"),
    struct_fields("No_Matching_Overload", (Struct_Item[]){
      { "Value *", "target" },
    }),
    struct_fields("Undecidable_Overload", (Struct_Item[]){
      { "Array_Undecidable_Match", "matches" },
    }),
    struct_empty("Non_Function_Overload"),
    struct_empty("No_Runtime_Use"),
    struct_empty("Recursive_Intrinsic_Use"),
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

  export_compiler(push_type(type_enum("Os", (Enum_Type_Item[]){
    { "Windows", 1 },
    { "Linux", 2 },
    { "Mac", 3 },
  })));

  push_type(type_struct("Program", (Struct_Item[]){
    { "Array_Import_Library", "import_libraries" },
    { "Array_Label_Location_Diff_Patch_Info", "patch_info_array" },
    { "Array_Relocation", "relocations" },
    { "Value *", "entry_point" },
    { "Array_Function_Builder", "functions" },
    { "Program_Memory", "memory" },
    { "const Calling_Convention *", "default_calling_convention"},
    { "Os", "os"},
    { "u32", "_os_padding"},
  }));

  push_type(type_function(Typedef, "Calling_Convention_Call_Setup_Proc", "Function_Call_Setup", (Argument_Type[]){
    { "const Allocator *", "allocator" },
    { "const Function_Info *", "function_info" },
  }));

  push_type(type_struct("Calling_Convention", (Struct_Item[]){
    { "Register_Bitset", "register_volatile_bitset" },
    { "Calling_Convention_Call_Setup_Proc", "call_setup_proc"},
  }));

  push_type(type_function(Typedef, "Mass_Trampoline_Proc", "void", (Argument_Type[]){
    { "void *", "returns" },
    { "void *", "args" },
  }));

  push_type(type_struct("Mass_Trampoline", (Struct_Item[]){
    { "const Descriptor *", "args_descriptor" },
    { "const Function_Info *", "original_info" },
    { "Mass_Trampoline_Proc", "proc"},
  }));

  push_type(type_hash_map("Struct_Field_Set", {
    .key_type = "const Struct_Field *",
    .value_type = "u64",
    .hash_function = "hash_pointer",
    .equal_function = "const_void_pointer_equal",
  }));

  push_type(type_hash_map("Slice_Set", {
    .key_type = "Slice",
    .value_type = "u64",
  }));

  push_type(type_hash_map("Symbol_Map", {
    .key_type = "Slice",
    .value_type = "Symbol *",
  }));

  push_type(type_hash_map("Trampoline_Map", {
    .key_type = "const Function_Info *",
    .value_type = "const Mass_Trampoline *",
    .hash_function = "hash_pointer",
    .equal_function = "const_void_pointer_equal",
  }));

  push_type(type_hash_map("Scope_Map", {
    .key_type = "const Symbol *",
    .value_type = "Scope_Entry *",
    .hash_function = "hash_pointer",
    .equal_function = "const_void_pointer_equal",
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
    { "u64", "relocations" },
    { "u64", "protected_ro_data_page_count" },
  }));

  push_type(type_struct("Jit", (Struct_Item[]){
    { "u64", "is_stack_unwinding_in_progress" },
    { "Program *", "program" },
    { "Jit_Import_Library_Handle_Map *", "import_library_handles" },
    { "Jit_Counters", "previous_counts" },
    { "void *", "platform_specific_payload" },
  }));

  push_type(type_hash_map("Static_Pointer_Length_Map", {
    .key_type = "const void *",
    .value_type = "u64",
    .hash_function = "hash_pointer",
    .equal_function = "const_void_pointer_equal",
  }));

  push_type(type_hash_map("Descriptor_Pointer_To_Cache_Map", {
    .key_type = "const Descriptor *",
    .value_type = "const Descriptor *",
    .hash_function = "hash_pointer",
    .equal_function = "const_void_pointer_equal",
  }));

  push_type(type_hash_map("Intrinsic_Proc_Cache_Map", {
    .key_type = "const Value *",
    .value_type = "Mass_Intrinsic_Proc",
    .hash_function = "hash_pointer",
    .equal_function = "const_void_pointer_equal",
  }));

  push_type(type_struct("Common_Symbols", (Struct_Item[]){
    { "const Symbol *", "apply" },
    { "const Symbol *", "fn" },
    { "const Symbol *", "get" },
    { "const Symbol *", "intrinsic" },
    { "const Symbol *", "label" },
    { "const Symbol *", "macro" },
    { "const Symbol *", "operator" },
    { "const Symbol *", "placeholder" },
    { "const Symbol *", "postfix_block" },
    { "const Symbol *", "statement" },
    { "const Symbol *", "syntax" },
    { "const Symbol *", "underscore" },
    { "const Symbol *", "_if" },
    { "const Symbol *", "then" },
    { "const Symbol *", "_while" },
    { "const Symbol *", "_else" },
    { "const Symbol *", "_" },
    { "const Symbol *", "operator_arrow" },
    { "const Symbol *", "operator_at" },
    { "const Symbol *", "operator_colon" },
    { "const Symbol *", "operator_colon_equal" },
    { "const Symbol *", "operator_double_colon" },
    { "const Symbol *", "operator_comma" },
    { "const Symbol *", "operator_dot" },
    { "const Symbol *", "operator_dot_star" },
    { "const Symbol *", "operator_equal" },
    { "const Symbol *", "operator_fat_arrow" },
    { "const Symbol *", "operator_space" },
    { "const Symbol *", "operator_tilde" },
    { "const Symbol *", "operator_quote" },
  }));

  export_compiler(push_type(type_struct("Compilation", (Struct_Item[]){
    { "Virtual_Memory_Buffer", "temp_buffer" },
    { "Allocator *", "temp_allocator" },
    { "Virtual_Memory_Buffer", "allocation_buffer" },
    { "Allocator *", "allocator" },
    { "Jit", "jit" },
    { "Module", "compiler_module" },
    { "Static_Pointer_Length_Map *", "static_pointer_length_map" },
    { "Imported_Module_Map *", "module_map" },
    { "Trampoline_Map *", "trampoline_map" },
    { "Scope *", "root_scope" },
    { "Program *", "runtime_program" },
    { "Mass_Result *", "result" },
    { "Symbol_Map *", "symbol_cache_map" },
    { "Operator_Symbol_Map *", "prefix_operator_symbol_map" },
    { "Operator_Symbol_Map *", "infix_or_suffix_operator_symbol_map" },
    { "Descriptor_Pointer_To_Cache_Map *", "descriptor_pointer_to_cache_map"},
    { "Intrinsic_Proc_Cache_Map *", "intrinsic_proc_cache_map" },
    { "Common_Symbols", "common_symbols" },
    { "Operator", "apply_operator" },
  })));

  push_type(type_function(Typedef, "Lazy_Value_Proc", "Value *", (Argument_Type[]){
    { "Mass_Context *", "context" },
    { "Function_Builder *", "builder" },
    { "const Expected_Result *", "expected_result" },
    { "const Source_Range *", "source_range" },
    { "const void *", "payload" },
  }));

  push_type(type_enum("Instruction_Extension_Type", (Enum_Type_Item[]){
    { "None", 0 },
    { "Register", 1 },
    { "Op_Code", 2 },
    { "Plus_Register", 3},
  }));

  push_type(type_enum("Operand_Encoding_Type", (Enum_Type_Item[]){
    { "None", 0},
    { "Register", 2},
    { "Register_A", 3},
    { "Register_Or_Memory", 4},
    { "Xmm", 5},
    { "Memory", 7},
    { "Immediate", 8},
  }));

  push_type(type_struct("Operand_Encoding", (Struct_Item[]){
    { "Operand_Encoding_Type", "type" },
    { "u32", "bit_size" },
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

  export_compiler(push_type(type_function(Default, "push_instruction", "void", (Argument_Type[]){
    {"Code_Block *", "code_block"},
    {"Instruction", "instruction"},
  })));

  export_compiler_custom_name("get", push_type(type_intrinsic("mass_get")));
  export_compiler_custom_name("import", push_type(type_intrinsic("mass_import")));

  export_compiler_custom_name("intrinsic", push_type(type_intrinsic("mass_intrinsic")));
  export_compiler_custom_name("apply", push_type(type_intrinsic("mass_apply")));
  export_compiler_custom_name("call", push_type(type_intrinsic("mass_call")));
  export_compiler_custom_name("using", push_type(type_intrinsic("mass_using")));
  export_compiler_custom_name("return", push_type(type_intrinsic("mass_return")));
  export_compiler_custom_name("operator_assignment", push_type(type_intrinsic("mass_operator_assignment")));
  export_compiler_custom_name("define_inferred", push_type(type_intrinsic("mass_define_inferred")));
  export_compiler_custom_name("quote", push_type(type_intrinsic("mass_quote")));
  export_compiler_custom_name("unquote", push_type(type_intrinsic("mass_unquote")));
  export_compiler_custom_name("comma", push_type(type_intrinsic("mass_comma")));
  export_compiler_custom_name("function_literal", push_type(type_intrinsic("mass_function_literal")));
  export_compiler_custom_name("dereference", push_type(type_intrinsic("mass_dereference")));
  export_compiler_custom_name("unchecked_get_at_index", push_type(type_intrinsic("mass_array_like_get")));
  export_compiler_custom_name("struct_get", push_type(type_intrinsic("mass_struct_get")));
  export_compiler_custom_name("named_accessor", push_type(type_intrinsic("mass_named_accessor")));
  export_compiler_custom_name("typed_symbol", push_type(type_intrinsic("mass_typed_symbol")));
  export_compiler_custom_name("pointer_to", push_type(type_intrinsic("mass_pointer_to")));
  export_compiler_custom_name("pointer_to_type", push_type(type_intrinsic("mass_pointer_to_type")));
  export_compiler_custom_name("eval", push_type(type_intrinsic("mass_eval")));
  export_compiler_custom_name("inline_module", push_type(type_intrinsic("mass_inline_module")));
  export_compiler_custom_name("c_struct", push_type(type_intrinsic("mass_c_struct")));
  export_compiler_custom_name("exports", push_type(type_intrinsic("mass_exports")));
  export_compiler_custom_name("cast", push_type(type_intrinsic("mass_cast")));
  export_compiler_custom_name("zero_extend", push_type(type_intrinsic("mass_zero_extend")));
  export_compiler_custom_name("parse_type", push_type(type_intrinsic("mass_parse_type")));
  export_compiler_custom_name("type_of", push_type(type_intrinsic("mass_type_of")));
  export_compiler_custom_name("size_of", push_type(type_intrinsic("mass_size_of")));
  export_compiler_custom_name("static_assert", push_type(type_intrinsic("mass_static_assert")));

  export_compiler_custom_name("allocator_allocate_bytes", push_type(
    type_function(Default, "allocator_allocate_bytes", "void *", (Argument_Type[]){
      { "const Allocator *", "allocator" },
      { "u64", "byte_size" },
      { "u64", "byte_alignment" },
    })
  ));
  export_compiler_custom_name("constraint_integer_type", push_type(
    type_function(Default, "mass_constraint_integer_type", "const Descriptor *", (Argument_Type[]){
      { "const Descriptor *", "descriptor" },
    })
  ));
  export_compiler_custom_name("constraint_float_type", push_type(
    type_function(Default, "mass_constraint_float_type", "const Descriptor *", (Argument_Type[]){
      { "const Descriptor *", "descriptor" },
    })
  ));
  export_compiler_custom_name("constraint_pointer_type", push_type(
    type_function(Default, "mass_constraint_pointer_type", "const Descriptor *", (Argument_Type[]){
      { "const Descriptor *", "descriptor" },
    })
  ));
  export_compiler_custom_name("constraint_struct_type", push_type(
    type_function(Default, "mass_constraint_struct_type", "const Descriptor *", (Argument_Type[]){
      { "const Descriptor *", "descriptor" },
    })
  ));
  export_compiler_custom_name("constraint_fixed_array_type", push_type(
    type_function(Default, "mass_constraint_fixed_array_type", "const Descriptor *", (Argument_Type[]){
      { "const Descriptor *", "descriptor" },
    })
  ));
  export_compiler_custom_name("constraint_function_instance_type", push_type(
    type_function(Default, "mass_constraint_function_instance_type", "const Descriptor *", (Argument_Type[]){
      { "const Descriptor *", "descriptor" },
    })
  ));

  export_compiler(push_type(
    type_function(Default, "descriptor_pointer_to", "const Descriptor *", (Argument_Type[]){
      { "Compilation *", "compilation" },
      { "const Descriptor *", "descriptor" },
    })
  ));

  export_compiler(push_type(
    type_function(Default, "same_type", "_Bool", (Argument_Type[]){
      { "const Descriptor *", "a" },
      { "const Descriptor *", "b" },
    })
  ));

  export_compiler(push_type(
    type_function(Default, "register_acquire_temp", "Register", (Argument_Type[]){
      { "Function_Builder *", "builder" },
    })
  ));

  export_compiler(push_type(
    type_function(Default, "register_release", "void", (Argument_Type[]){
      { "Function_Builder *", "builder" },
      { "Register", "reg" },
    })
  ));

  export_compiler(push_type(
    type_function(Default, "storage_register", "Storage", (Argument_Type[]){
      { "Register", "reg" },
      { "Bits", "bit_size" },
    })
  ));

  export_compiler(push_type(
    type_function(Default, "storage_register_temp", "Storage", (Argument_Type[]){
      { "Function_Builder *", "builder" },
      { "Bits", "bit_size" },
    })
  ));

  export_compiler(push_type(
    type_function(Default, "storage_release_if_temporary", "void", (Argument_Type[]){
      { "Function_Builder *", "builder" },
      { "const Storage *", "storage" },
    })
  ));

  export_compiler_custom_name("expected_result_exact", push_type(
    type_function(Default, "mass_expected_result_exact", "Expected_Result", (Argument_Type[]){
      { "const Descriptor *", "descriptor" },
      { "Storage", "storage" },
    })
  ));

  export_compiler_custom_name("syscall", push_type(
    type_function(Default, "mass_syscall", "Value *", (Argument_Type[]){
      { "Mass_Context *", "context" },
      { "Parser *", "parser" },
      { "Value_View", "args" },
      { "const Function_Header *", "header" },
      { "u64", "number" },
    })
  ));

  export_compiler_custom_name("value_force", push_type(
    type_function(Default, "value_force", "Value *", (Argument_Type[]){
      { "Mass_Context *", "context" },
      { "Function_Builder *", "builder" },
      { "const Expected_Result *", "expected_result" },
      { "Value *", "value" },
    })
  ));

  export_compiler_custom_name("module_get", push_type(
    type_function(Default, "mass_module_get_impl", "Value *", (Argument_Type[]){
      { "Mass_Context *", "context" },
      { "const Module *", "module" },
      { "const Symbol *", "symbol" },
      { "const Source_Range *", "source_range" },
    })
  ));

  export_compiler_custom_name("forward_call_to_alias", push_type(
    type_function(Default, "mass_forward_call_to_alias", "Value *", (Argument_Type[]){
      { "Mass_Context *", "context" },
      { "Parser *", "parser" },
      { "Value_View", "args" },
      { "const Symbol *", "symbol" },
    })
  ));

  const char *i64_ops[] = {
    "logical_shift_left", "logical_shift_right",
    "bitwise_and", "bitwise_or",
    "add", "subtract",
    "signed_multiply", "unsigned_multiply",
    "signed_divide", "unsigned_divide",
    "signed_remainder", "unsigned_remainder",
  };

  for (u64 i = 0; i < countof(i64_ops); ++i) {
    const char *base = i64_ops[i];
    const char *exported_name = strjoin("i64_", base);
    const char *internal_name = strjoin("mass_i64_", base);
    export_compiler_custom_name(exported_name, push_type(
      type_function(Default, internal_name, "i64", (Argument_Type[]){
        { "i64", "a" },
        { "i64", "b" },
      })
    ));
  }

  const char *i64_compares[] = {
    "signed_less", "unsigned_less",
    "signed_less_equal", "unsigned_less_equal",
    "signed_greater", "unsigned_greater",
    "signed_greater_equal", "unsigned_greater_equal",
  };
  for (u64 i = 0; i < countof(i64_compares); ++i) {
    const char *base = i64_compares[i];
    const char *exported_name = strjoin("i64_", base);
    const char *internal_name = strjoin("mass_i64_", base);
    export_compiler_custom_name(exported_name, push_type(
      type_function(Default, internal_name, "_Bool", (Argument_Type[]){
        { "i64", "a" },
        { "i64", "b" },
      })
    ));
  }

  export_compiler_custom_name("integer_add", push_type(type_intrinsic("mass_integer_add")));
  export_compiler_custom_name("integer_subtract", push_type(type_intrinsic("mass_integer_subtract")));
  export_compiler_custom_name("integer_multiply", push_type(type_intrinsic("mass_integer_multiply")));
  export_compiler_custom_name("integer_divide", push_type(type_intrinsic("mass_integer_divide")));
  export_compiler_custom_name("integer_remainder", push_type(type_intrinsic("mass_integer_remainder")));
  export_compiler_custom_name("integer_less", push_type(type_intrinsic("mass_integer_less")));
  export_compiler_custom_name("integer_greater", push_type(type_intrinsic("mass_integer_greater")));
  export_compiler_custom_name("integer_less_equal", push_type(type_intrinsic("mass_integer_less_equal")));
  export_compiler_custom_name("integer_greater_equal", push_type(type_intrinsic("mass_integer_greater_equal")));
  export_compiler_custom_name("integer_equal", push_type(type_intrinsic("mass_integer_equal")));
  export_compiler_custom_name("integer_not_equal", push_type(type_intrinsic("mass_integer_not_equal")));

  export_compiler_custom_name("generic_equal", push_type(type_intrinsic("mass_generic_equal")));
  export_compiler_custom_name("generic_not_equal", push_type(type_intrinsic("mass_generic_not_equal")));

  //////////////////////
  // Standard C types //
  //////////////////////

  // The signedness of `char` is implementation defined. To keep things same
  // we assume that the meta program and the main one are both compiled
  // on the same machine with the same compiler and flags, allowing to detect
  // `char` sign here. It is still going to be a problem in case someone wants
  // to do C++ name mangling but that is way down the line.
  Meta_Integer_Type char_type = CHAR_MIN != 0 ? Meta_Integer_Type_Signed : Meta_Integer_Type_Unsigned;
  set_flags(push_type(type_integer("char", char_type)), Meta_Type_Flags_No_C_Type);
  set_flags(push_type(type_integer("int", Meta_Integer_Type_Signed)), Meta_Type_Flags_No_C_Type);

  // Prelude Types
  export_compiler(set_flags(push_type(type_c_opaque("Allocator")), Meta_Type_Flags_No_C_Type));
  set_flags(push_type(type_c_opaque("Virtual_Memory_Buffer")), Meta_Type_Flags_No_C_Type);

  set_flags(
    export_global_custom_name("bool", push_type(type_c_opaque("_Bool"))),
    Meta_Type_Flags_No_C_Type
   );

  export_global(set_flags(push_type(type_raw("i8")), Meta_Type_Flags_No_C_Type));
  export_global(set_flags(push_type(type_raw("i16")), Meta_Type_Flags_No_C_Type));
  export_global(set_flags(push_type(type_raw("i32")), Meta_Type_Flags_No_C_Type));
  export_global(set_flags(push_type(type_raw("i64")), Meta_Type_Flags_No_C_Type));

  #define PROCESS_INTEGER_TYPES(F)\
    F(u8, Unsigned) F(u16, Unsigned) F(u32, Unsigned) F(u64, Unsigned)\
    F(s8, Signed) F(s16, Signed) F(s32, Signed) F(s64, Signed)

  #define PROCESS_FLOAT_TYPES(F, ...)\
    F(f32) F(f64)

  #define PROCESS_NUMERIC_TYPES(F)\
    PROCESS_INTEGER_TYPES(F)\
    PROCESS_FLOAT_TYPES(F)

  #define DEFINE_INTEGER_TYPE(T, SIGNEDNESS)\
    export_global(set_flags(push_type(type_integer(#T, Meta_Integer_Type_##SIGNEDNESS)),\
      Meta_Type_Flags_No_C_Type | Meta_Type_Flags_No_Value_Array));
  #define DEFINE_FLOAT_TYPE(T)\
    export_global(set_flags(push_type(type_float(#T)),\
      Meta_Type_Flags_No_C_Type | Meta_Type_Flags_No_Value_Array));
  PROCESS_INTEGER_TYPES(DEFINE_INTEGER_TYPE)
  PROCESS_FLOAT_TYPES(DEFINE_FLOAT_TYPE)

  #define DEFINE_RANGES(T, ...)\
    set_flags(push_type(type_struct("Range_" #T, (Struct_Item[]){\
      { #T, "from" },\
      { #T, "to" },\
    })), Meta_Type_Flags_No_C_Type | Meta_Type_Flags_No_Value_Array);

  PROCESS_NUMERIC_TYPES(DEFINE_RANGES)

  export_global_custom_name("String", set_flags(push_type(type_struct("Slice", (Struct_Item[]){
    { "u8 *", "bytes" },
    { "i64", "length" },
  })), Meta_Type_Flags_No_C_Type | Meta_Type_Flags_No_Value_Array));

  export_compiler(set_flags(push_type(type_struct("Dyn_Array_Internal", (Struct_Item[]){
    { "const Allocator *", "allocator" },
    { "i64", "length" },
    { "i64", "capacity" },
    //{ "s8 *", "items" }, // TODO support flexible array members or even better - sized by a field
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
      fprintf(file, "#include \"prelude.h\"\n");

      // Make sure our generated structs have explicit padding
      fprintf(file, "_Pragma(\"warning (push)\") _Pragma(\"warning (default: 4820)\")\n");

      // Custom forward declarations
      {
        fprintf(file, "typedef void(*fn_type_opaque)();\n\n");
        fprintf(file, "typedef struct { u8 bits; } i8;\n\n");
        fprintf(file, "typedef struct { u16 bits; } i16;\n\n");
        fprintf(file, "typedef struct { u32 bits; } i32;\n\n");
        fprintf(file, "typedef struct { u64 bits; } i64;\n\n");
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
      fprintf(file, "static Descriptor descriptor_descriptor_pointer;\n");

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