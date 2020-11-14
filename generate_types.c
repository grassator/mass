#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

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
  Struct *items;
  uint64_t item_count;
} Tagged_Union;

typedef enum {
  Type_Tag_Struct,
  Type_Tag_Tagged_Union,
  Type_Tag_Enum,
} Type_Tag;

typedef struct {
  Type_Tag tag;
  union {
    Struct struct_;
    Enum enum_;
    Tagged_Union union_;
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
      break;
    }
    case Type_Tag_Enum: {
      fprintf(file, "typedef enum %s %s;\n", type->enum_.name, type->enum_.name);
      break;
    }
    case Type_Tag_Tagged_Union: {
      fprintf(file, "typedef struct %s %s;\n", type->union_.name, type->union_.name);
      break;
    }
  }
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
      fprintf(file, "typedef dyn_array_type(%s) Array_%s;\n",
        type->struct_.name, type->struct_.name);
      fprintf(file, "typedef dyn_array_type(%s *) Array_%s_Ptr;\n\n",
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
        for (uint64_t i = 0; i < type->union_.item_count; ++i) {
          Struct *struct_ = &type->union_.items[i];
          if (struct_->item_count) {
            fprintf(file, "  %s_%s %s;\n",
              type->union_.name, struct_->name, struct_->name);
          }
        }
        fprintf(file, "} %s;\n", type->union_.name);
        fprintf(file, "typedef dyn_array_type(%s) Array_%s;\n",
          type->union_.name, type->union_.name);
        fprintf(file, "typedef dyn_array_type(%s *) Array_%s_Ptr;\n\n",
          type->union_.name, type->union_.name);
      }
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

  {
    int32_t value = 0;
    Enum_Item items[] = {
      { "None", value++ },
      { "Any", value++ },
      { "Eflags", value++ },
      { "Register", value++ },
      { "Xmm", value++ },
      { "Immediate_8", value++ },
      { "Immediate_16", value++ },
      { "Immediate_32", value++ },
      { "Immediate_64", value++ },
      { "Memory_Indirect", value++ },
      { "Sib", value++ },
      { "RIP_Relative", value++ },
      { "RIP_Relative_Import", value++ },
      { "Label_32", value++ },
    };
    push_type(type_enum("Operand_Type", items));
  }

  push_type(type_struct("Parse_Error", (Struct_Item[]){
    { "Slice", "message" },
    { "Source_Location", "location" },
  }));

  push_type(type_union("Parse_Result", (Struct[]){
    struct_empty("Success"),
    struct_fields("Error", (Struct_Item[]){
      { "list", "Array_Parse_Error" },
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