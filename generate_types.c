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

typedef enum {
  Type_Tag_Struct,
  Type_Tag_Enum,
} Type_Tag;

typedef struct {
  Type_Tag tag;
  union {
    Struct struct_;
    Enum enum_;
  };
} Type;

typedef void (*print_type_proc)(FILE *file, Type *type);

void
print_c_type(
  FILE *file,
  Type *type
) {
  switch(type->tag) {
    case Type_Tag_Struct: {
      fprintf(file, "typedef struct {\n");
      for (uint64_t i = 0; i < type->struct_.item_count; ++i) {
        Struct_Item *item = &type->struct_.items[i];
        fprintf(file, "  %s %s;\n", item->type, item->name);
      }
      fprintf(file, "} %s;\n\n", type->struct_.name);
      break;
    }
    case Type_Tag_Enum: {
      fprintf(file, "typedef enum {\n");
      for (uint64_t i = 0; i < type->enum_.item_count; ++i) {
        Enum_Item *item = &type->enum_.items[i];
        fprintf(file, "  %s_%s = %d,\n", type->enum_.name, item->name, item->value);
      }
      fprintf(file, "} %s;\n\n", type->enum_.name);
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
  }
}

#define print_enum(_FILE_, _NAME_STRING_, ...)\
  print_type(_FILE_, &(Type){\
    .tag = Type_Tag_Enum,\
    .enum_ = {\
      .name = _NAME_STRING_,\
      .items = (__VA_ARGS__),\
      .item_count = countof(__VA_ARGS__),\
    }\
  })

#define print_struct(_FILE_, _NAME_STRING_, ...)\
  print_type(_FILE_, &(Type){\
    .tag = Type_Tag_Struct,\
    .struct_ = {\
      .name = _NAME_STRING_,\
      .items = (__VA_ARGS__),\
      .item_count = countof(__VA_ARGS__),\
    }\
  })

void
write_types(
  const char *path,
  print_type_proc print_type
) {
  #pragma warning(disable : 4996)
  FILE *file = fopen(path, "w");
  if (!file) exit(1);
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
    print_enum(file, "Operand_Type", items);
  }

  print_struct(file, "Label", (Struct_Item[]){
    { "bool", "resolved" },
    { "u32", "target_rva" },
  });
  fclose(file);
}

int
main(void) {
  {
    const char *filename = "..\\types.h";
    write_types(filename, print_c_type);
    printf("C Types Generated at: %s\n", filename);
  }
  {
    const char *filename = "..\\lib\\compiler_types.mass";
    write_types(filename, print_mass_type);
    printf("Mass Types Generated at: %s\n", filename);
  }
  return 0;
}