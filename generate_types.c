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

typedef enum {
  Output_Mode_C,
  Output_Mode_Mass,
} Output_Mode;

Output_Mode output_mode = Output_Mode_C;

void
print_c_type(
  Type *type
) {
  switch(type->tag) {
    case Type_Tag_Struct: {
      printf("typedef struct {\n");
      for (uint64_t i = 0; i < type->struct_.item_count; ++i) {
        Struct_Item *item = &type->struct_.items[i];
        printf("  %s %s;\n", item->type, item->name);
      }
      printf("} %s;\n\n", type->struct_.name);
      break;
    }
    case Type_Tag_Enum: {
      printf("typedef enum {\n");
      for (uint64_t i = 0; i < type->enum_.item_count; ++i) {
        Enum_Item *item = &type->enum_.items[i];
        printf("  %s_%s = %d,\n", type->enum_.name, item->name, item->value);
      }
      printf("} %s;\n\n", type->enum_.name);
      break;
    }
  }
}

void
print_mass_type(
  Type *type
) {
  switch(type->tag) {
    case Type_Tag_Struct: {
      printf("Mass_%s :: struct {\n", type->struct_.name);
      for (uint64_t i = 0; i < type->struct_.item_count; ++i) {
        Struct_Item *item = &type->struct_.items[i];
        printf("  %s : %s\n", item->name, item->type);
      }
      printf("}\n\n");
      break;
    }
    case Type_Tag_Enum: {
      for (uint64_t i = 0; i < type->enum_.item_count; ++i) {
        Enum_Item *item = &type->enum_.items[i];
        printf("Mass_%s_%s :: %d\n", type->enum_.name, item->name, item->value);
      }
      printf("\n");
      break;
    }
  }
}

void
print_type(
  Type *type
) {
  switch(output_mode) {
    case Output_Mode_C: print_c_type(type); break;
    case Output_Mode_Mass: print_mass_type(type); break;
  }
}

int
main(void) {
  output_mode = Output_Mode_Mass;
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
    Enum type = {
      .name = "Operand_Type",
      .items = items,
      .item_count = countof(items),
    };
    print_type(&(Type){ .tag = Type_Tag_Enum, .enum_ = type });
  }

  {
    Struct_Item items[] = {
      { "bool", "resolved" },
      { "u32", "target_rva" },
    };
    Struct type = {
      .name = "Label",
      .items = items,
      .item_count = countof(items),
    };
    print_type(&(Type){ .tag = Type_Tag_Struct, .struct_ = type });
  }

  return 0;
}