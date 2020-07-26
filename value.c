#include "value.h"

Scope *
scope_make(
  Scope *parent
) {
  Scope *scope = temp_allocate(Scope);
  *scope = (Scope) {
    .parent = parent,
    .map = hash_map_make(Scope_Map),
  };
  return scope;
}

Value *
scope_lookup(
  Scope *scope,
  Slice name
) {
  while (scope) {
    Value **result = hash_map_get(scope->map, name);
    if (result) return *result;
    scope = scope->parent;
  }
  return 0;
}

void
scope_define(
  Scope *scope,
  Slice name,
  Value *value
) {
  // TODO think about what should happen when trying to redefine existing thing
  hash_map_set(scope->map, name, value);
}

inline bool
same_value_type(
  Value *a,
  Value *b
);

bool
same_type(
  Descriptor *a,
  Descriptor *b
) {
  if (a->type != b->type) return false;
  switch(a->type) {
    case Descriptor_Type_Pointer: {
      if (
        a->pointer_to->type == Descriptor_Type_Fixed_Size_Array &&
        same_type(a->pointer_to->array.item, b->pointer_to)
      ) return true;
      if (
        b->pointer_to->type == Descriptor_Type_Fixed_Size_Array &&
        same_type(b->pointer_to->array.item, a->pointer_to)
      ) return true;
      if (
        a->pointer_to->type == Descriptor_Type_Void ||
        b->pointer_to->type == Descriptor_Type_Void
      ) {
        return true;
      }
      return same_type(a->pointer_to, b->pointer_to);
    }
    case Descriptor_Type_Fixed_Size_Array: {
      return same_type(a->array.item, b->array.item) && a->array.length == b->array.length;
    }
    case Descriptor_Type_Tagged_Union:
    case Descriptor_Type_Struct: {
      return a == b;
    }
    case Descriptor_Type_Function: {
      if (!same_type(a->function.returns->descriptor, b->function.returns->descriptor)) {
        return false;
      }
      if (dyn_array_length(a->function.arguments) != dyn_array_length(b->function.arguments)) {
        return false;
      }
      for (u64 i = 0; i < dyn_array_length(a->function.arguments); ++i) {
        if (!same_value_type(
          *dyn_array_get(a->function.arguments, i),
          *dyn_array_get(b->function.arguments, i)
        )) {
          return false;
        }
      }
      return true;
    }
    case Descriptor_Type_Void:
    case Descriptor_Type_Integer: {
      return descriptor_byte_size(a) == descriptor_byte_size(b);
    }
    case Descriptor_Type_Type:
    default: {
      assert(!"Unsupported descriptor type");
      return false;
    }
  }
}

inline bool
same_value_type(
  Value *a,
  Value *b
) {
  return same_type(a->descriptor, b->descriptor);
}

u32
struct_byte_size(
  const Descriptor_Struct *struct_
) {
  s64 count = struct_->field_count;
  assert(count);
  u32 alignment = 0;
  u32 raw_size = 0;
  for (s32 i = 0; i < count; ++i) {
    Descriptor_Struct_Field *field = &struct_->field_list[i];
    u32 field_size = descriptor_byte_size(field->descriptor);
    alignment = max(alignment, field_size);
    bool is_last_field = i == count - 1;
    if (is_last_field) {
      raw_size = field->offset + field_size;
    }
  }
  return s32_align(raw_size, alignment);
}

u32
descriptor_byte_size(
  const Descriptor *descriptor
) {
  assert(descriptor);
  switch(descriptor->type) {
    case Descriptor_Type_Void: {
      return 0;
    }
    case Descriptor_Type_Tagged_Union: {
      s64 count = descriptor->tagged_union.struct_count;
      u32 tag_size = sizeof(s64);
      u32 body_size = 0;
      for (s32 i = 0; i < count; ++i) {
        Descriptor_Struct *struct_ = &descriptor->tagged_union.struct_list[i];
        u32 struct_size = struct_byte_size(struct_);
        body_size = max(body_size, struct_size);
      }
      return tag_size + body_size;
    }
    case Descriptor_Type_Struct: {
      return struct_byte_size(&descriptor->struct_);
    }
    case Descriptor_Type_Integer: {
      return descriptor->integer.byte_size;
    }
    case Descriptor_Type_Fixed_Size_Array: {
      return descriptor_byte_size(descriptor->array.item) * descriptor->array.length;
    }
    case Descriptor_Type_Pointer:
    case Descriptor_Type_Function: {
      return 8;
    }
    case Descriptor_Type_Type:
    default: {
      assert(!"Unknown Descriptor Type");
    }
  }
  return 0;
}


// @Volatile @Reflection
typedef struct {
  s32 field_count;
} Descriptor_Struct_Reflection;

// @Volatile @Reflection
Descriptor_Struct_Field struct_reflection_fields[] = {
  {
    .name = "field_count",
    .offset = 0,
    .descriptor = &descriptor_s32,
  }
};

Descriptor descriptor_struct_reflection = {
  .type = Descriptor_Type_Struct,
  .struct_ = {
    .field_list = struct_reflection_fields,
    .field_count = countof(struct_reflection_fields),
  },
};

void
print_operand(
  const Operand *operand
) {
  switch (operand->type) {
    case Operand_Type_None: {
      printf("_");
      break;
    }
    case Operand_Type_Register: {
      u32 bits = operand->byte_size * 8;
      printf("r%d", bits);
      break;
    }
    case Operand_Type_Immediate_8: {
      printf("imm8(0x%02x)", operand->imm8);
      break;
    }
    case Operand_Type_Immediate_32: {
      printf("imm32(0x%08x)", operand->imm32);
      break;
    }
    case Operand_Type_Immediate_64: {
      printf("imm64(0x%016llx)", operand->imm64);
      break;
    }
    case Operand_Type_Memory_Indirect: {
      u32 bits = operand->byte_size * 8;
      printf("m%d", bits);
      break;
    }
    case Operand_Type_RIP_Relative: {
      printf("[.rdata + 0x%08llx]", operand->rip_offset_in_data);
      break;
    }
    case Operand_Type_RIP_Relative_Import: {
      printf("rip_import(%s:%s)", operand->import.library_name, operand->import.symbol_name);
      break;
    }
    case Operand_Type_Label_32: {
      printf("rel....UNIMPLEMENTED");
      break;
    }
    default: {
      printf("<unknown>");
      break;
    }
  }
}

#define define_register(reg_name, reg_index, reg_byte_size) \
const Operand reg_name = { \
  .type = Operand_Type_Register, \
  .byte_size = (reg_byte_size), \
  .reg = (reg_index), \
};

define_register(rax, 0, 8); // 0b0000
define_register(rcx, 1, 8);
define_register(rdx, 2, 8);
define_register(rbx, 3, 8);
define_register(rsp, 4, 8);
define_register(rbp, 5, 8);
define_register(rsi, 6, 8);
define_register(rdi, 7, 8);

define_register(eax, 0, 4);
define_register(ecx, 1, 4);
define_register(edx, 2, 4);
define_register(ebx, 3, 4);
define_register(esp, 4, 4);
define_register(ebp, 5, 4);
define_register(esi, 6, 4);
define_register(edi, 7, 4);

define_register(r8,  8, 8);
define_register(r9,  9, 8);
define_register(r10, 10, 8);
define_register(r11, 11, 8);
define_register(r12, 12, 8);
define_register(r13, 13, 8);
define_register(r14, 14, 8);
define_register(r15, 15, 8);

define_register(r8d,  8, 4);
define_register(r9d,  9, 4);
define_register(r10d, 10, 4);
define_register(r11d, 11, 4);
define_register(r12d, 12, 4);
define_register(r13d, 13, 4);
define_register(r14d, 14, 4);
define_register(r15d, 15, 4);
#undef define_register

inline Label*
make_label() {
  Label *label = temp_allocate(Label);
  *label = (Label) {
    .locations = dyn_array_make(Array_Label_Location, .allocator = temp_allocator),
  };
  return label;
}

inline Operand
label32(
  Label *label
) {
  return (const Operand) {
    .type = Operand_Type_Label_32,
    .byte_size = 4,
    .label32 = label
  };
}

inline Operand
imm8(
  s8 value
) {
  return (const Operand) {
    .type = Operand_Type_Immediate_8,
    .byte_size = 1,
    .imm8 = value
  };
}

inline Operand
imm32(
  s32 value
) {
  return (const Operand) {
    .type = Operand_Type_Immediate_32,
    .byte_size = 4,
    .imm32 = value
  };
}

inline Operand
imm64(
  s64 value
) {
  return (const Operand) {
    .type = Operand_Type_Immediate_64,
    .byte_size = 8,
    .imm64 = value
  };
}

inline Operand
imm_auto(
  s64 value
) {
  if (s64_fits_into_s8(value)) {
    return imm8((s8) value);
  }
  if (s64_fits_into_s32(value)) {
    return imm32((s32) value);
  }
  return imm64(value);
}

inline Operand
stack(
  s32 offset,
  u32 byte_size
) {
  assert(byte_size);
  return (const Operand) {
    .type = Operand_Type_Memory_Indirect,
    .byte_size = byte_size,
    .indirect = (const Operand_Memory_Indirect) {
      .reg = rsp.reg,
      .displacement = offset,
    }
  };
}

Value *
value_from_s64(
  s64 integer
) {
  Value *result = temp_allocate(Value);
  *result = (Value) {
    .descriptor = &descriptor_s64,
    .operand = imm64(integer),
  };
  return result;
}

Value *
value_from_s32(
  s32 integer
) {
  Value *result = temp_allocate(Value);
  *result = (const Value) {
    .descriptor = &descriptor_s32,
    .operand = imm32(integer),
  };
  return result;
}

Value *
value_from_s8(
  s8 integer
) {
  Value *result = temp_allocate(Value);
  *result = (const Value) {
    .descriptor = &descriptor_s8,
    .operand = imm8(integer),
  };
  return result;
}

Value *
value_byte_size(
  Value *value
) {
  s32 byte_size = descriptor_byte_size(value->descriptor);
  return value_from_s32(byte_size);
}


Value *
value_register_for_descriptor(
  Register reg,
  Descriptor *descriptor
) {
  u32 byte_size = descriptor_byte_size(descriptor);
  assert(byte_size == 1 || byte_size == 2 || byte_size == 4 || byte_size == 8);

  Value *result = temp_allocate(Value);
  *result = (const Value) {
    .descriptor = descriptor,
    .operand = {
      .type = Operand_Type_Register,
      .reg = reg,
      .byte_size = byte_size,
    },
  };
  return result;
}

void *
rip_value_pointer(
  Program *program,
  Value *value
) {
  assert(value->operand.type == Operand_Type_RIP_Relative);
  return program->data_buffer->memory + value->operand.rip_offset_in_data;
}

Value *
value_global(
  Program *program,
  Descriptor *descriptor
) {
  u32 byte_size = descriptor_byte_size(descriptor);
  s8 *address = fixed_buffer_allocate_bytes(program->data_buffer, byte_size, sizeof(s8));
  s64 offset_in_data_section = address - program->data_buffer->memory;

  Value *result = temp_allocate(Value);
  *result = (Value) {
    .descriptor = descriptor,
    .operand = {
      .type = Operand_Type_RIP_Relative,
      .byte_size = byte_size,
      .rip_offset_in_data = (s64) offset_in_data_section,
    },
  };
  return result;
}

Value *
value_global_c_string(
  Program *program,
  const char *string
) {
  s32 length = (s32)strlen(string) + 1;
  Descriptor *descriptor = temp_allocate(Descriptor);
  *descriptor = (Descriptor) {
    .type = Descriptor_Type_Fixed_Size_Array,
    .array = {
      .item = &descriptor_s8,
      .length = length,
    },
  };

  Value *string_value = value_global(program, descriptor);
  memcpy(rip_value_pointer(program, string_value), string, length);
  return string_value;
}

Descriptor *
descriptor_pointer_to(
  Descriptor *descriptor
) {
  Descriptor *result = temp_allocate(Descriptor);
  *result = (const Descriptor) {
    .type = Descriptor_Type_Pointer,
    .pointer_to = descriptor,
  };
  return result;
}

Descriptor *
descriptor_array_of(
  Descriptor *descriptor,
  u32 length
) {
  Descriptor *result = temp_allocate(Descriptor);
  *result = (const Descriptor) {
    .type = Descriptor_Type_Fixed_Size_Array,
    .array = {
      .item = descriptor,
      .length = length,
    },
  };
  return result;
}

fn_type_opaque
helper_value_as_function(
  Value *value
) {
  assert(value->operand.type == Operand_Type_Label_32);
  assert(value->operand.label32->target);
  return (fn_type_opaque)value->operand.label32->target;
}

#define value_as_function(_value_, _type_) \
  ((_type_)helper_value_as_function(_value_))


bool
memory_range_equal_to_c_string(
  const void *memory_range_start,
  const void *memory_range_end,
  const char *string
) {
  s64 length = ((char *)memory_range_end) - ((char *)memory_range_start);
  s64 string_length = strlen(string);
  if (string_length != length) return false;
  return memcmp(memory_range_start, string, string_length) == 0;
}

Value *
function_push_argument(
  Descriptor_Function *function,
  Descriptor *arg_descriptor
) {
  u32 byte_size = descriptor_byte_size(arg_descriptor);
  assert(byte_size <= 8);
  switch (dyn_array_length(function->arguments)) {
    case 0: {
      return *dyn_array_push(
        function->arguments,
        value_register_for_descriptor(Register_C, arg_descriptor)
      );
    }
    case 1: {
      return *dyn_array_push(
        function->arguments,
        value_register_for_descriptor(Register_D, arg_descriptor)
      );
    }
    case 2: {
      return *dyn_array_push(
        function->arguments,
        value_register_for_descriptor(Register_R8, arg_descriptor)
      );
    }
    case 3: {
      return *dyn_array_push(
        function->arguments,
        value_register_for_descriptor(Register_R9, arg_descriptor)
      );
    }
    default: {
      s32 offset = u64_to_s32(dyn_array_length(function->arguments) * 8);
      Operand operand = stack(offset, byte_size);
      Value *value = temp_allocate(Value);
      *value = (Value) {
        .descriptor = arg_descriptor,
        .operand = operand,
      };
      return *dyn_array_push(function->arguments, value);
    }
  }
}

Descriptor *
parse_c_type(
  const char *range_start,
  const char *range_end
) {
  Descriptor *descriptor = 0;

  const char *start = range_start;
  for(const char *ch = range_start; ch <= range_end; ++ch) {
    if (!(*ch == ' ' || *ch == '*' || ch == range_end)) continue;
    if (start != ch) {
      if (
        memory_range_equal_to_c_string(start, ch, "char") ||
        memory_range_equal_to_c_string(start, ch, "s8")
      ) {
        descriptor = &descriptor_s8;
      } else if (
        memory_range_equal_to_c_string(start, ch, "int") ||
        memory_range_equal_to_c_string(start, ch, "s32")
      ) {
        descriptor = &descriptor_s32;
      } else if (
        memory_range_equal_to_c_string(start, ch, "void *") ||
        memory_range_equal_to_c_string(start, ch, "s64")
      ) {
        descriptor = &descriptor_s64;
      } else if (memory_range_equal_to_c_string(start, ch, "void")) {
        descriptor = &descriptor_void;
      } else if (memory_range_equal_to_c_string(start, ch, "const")) {
        // TODO support const values?
      } else {
        assert(!"Unsupported argument type");
      }
    }
    if (*ch == '*') {
      assert(descriptor);
      Descriptor *previous_descriptor = descriptor;
      descriptor = temp_allocate(Descriptor);
      *descriptor = (const Descriptor) {
        .type = Descriptor_Type_Pointer,
        .pointer_to = previous_descriptor,
      };
    }
    start = ch + 1;
  }
  return descriptor;
}

Value *
c_function_return_value(
  const char *forward_declaration
) {
  char *ch = strchr(forward_declaration, '(');
  assert(ch);
  --ch;

  // skip whitespace before (
  for(; *ch == ' '; --ch);
  for(;
    (*ch >= 'a' && *ch <= 'z') ||
    (*ch >= 'A' && *ch <= 'Z') ||
    (*ch >= '0' && *ch <= '9') ||
    *ch == '_';
    --ch
  )
  // skip whitespace before function name
  for(; *ch == ' '; --ch);
  ++ch;
  Descriptor *descriptor = parse_c_type(forward_declaration, ch);
  assert(descriptor);
  switch(descriptor->type) {
    case Descriptor_Type_Void: {
      return &void_value;
    }
    case Descriptor_Type_Function:
    case Descriptor_Type_Integer:
    case Descriptor_Type_Pointer: {
      Value *return_value = value_register_for_descriptor(Register_A, descriptor);
      return return_value;
    }
    case Descriptor_Type_Tagged_Union:
    case Descriptor_Type_Fixed_Size_Array:
    case Descriptor_Type_Struct:
    case Descriptor_Type_Type:
    default: {
      assert(!"Unsupported return type");
    }
  }
  return 0;
}

Descriptor *
c_function_descriptor(
  const char *forward_declaration
) {
  Descriptor *descriptor = temp_allocate(Descriptor);
  *descriptor = (const Descriptor) {
    .type = Descriptor_Type_Function,
    .function = {
      .arguments = dyn_array_make(Array_Value_Ptr, .allocator = temp_allocator),
      .returns = 0,
    },
  };

  descriptor->function.returns = c_function_return_value(forward_declaration);
  char *ch = strchr(forward_declaration, '(');
  assert(ch);
  ch++;

  char *start = ch;
  Descriptor *argument_descriptor = 0;
  for (; *ch; ++ch) {
    if (*ch == ',' || *ch == ')') {
      if (start != ch) {
        argument_descriptor = parse_c_type(start, ch);
        // support for foo(void) fn signature
        if (
          dyn_array_length(descriptor->function.arguments) == 0 &&
          argument_descriptor->type == Descriptor_Type_Void
        ) {
          assert(*ch == ')');
          break;
        }
        function_push_argument(&descriptor->function, argument_descriptor);
        assert(argument_descriptor);
      }
      start = ch + 1;
    }
  }

  return descriptor;
}

Value *
c_function_value(
  const char *forward_declaration,
  fn_type_opaque fn
) {
  Value *result = temp_allocate(Value);
  *result = (const Value) {
    .descriptor = c_function_descriptor(forward_declaration),
    .operand = imm64((s64) fn),
  };
  return result;
}

void
program_free(
  Program *program
) {
  for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
    Import_Library *library = dyn_array_get(program->import_libraries, i);
    dyn_array_destroy(library->symbols);
  }
  dyn_array_destroy(program->import_libraries);
  free(program);
}

Operand
import_symbol(
  Program *program,
  const char *library_name,
  const char *symbol_name
) {
  Import_Library *library = 0;

  for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
    Import_Library *lib = dyn_array_get(program->import_libraries, i);
    if (_stricmp(lib->name, library_name) == 0) {
      library = lib;
    }
  }
  if (!library) {
    library = dyn_array_push(program->import_libraries, (Import_Library) {
      .name = library_name,
      .name_rva = 0xCCCCCCCC,
      .rva = 0xCCCCCCCC,
      .image_thunk_rva = 0xCCCCCCCC,
      .symbols = dyn_array_make(Array_Import_Symbol, .allocator = temp_allocator),
    });
  }

  Import_Symbol *symbol = 0;
  for (u64 i = 0; i < dyn_array_length(library->symbols); ++i) {
    Import_Symbol *it = dyn_array_get(library->symbols, i);
    if (strcmp(it->name, symbol_name) == 0) {
      symbol = it;
    }
  }

  if (!symbol) {
    symbol = dyn_array_push(library->symbols, (Import_Symbol) {
      .name = symbol_name,
      .name_rva = 0xCCCCCCCC,
      .offset_in_data = 0
    });
  }

  return (Operand) {
    .type = Operand_Type_RIP_Relative_Import,
    .byte_size = 8, // Size of the pointer
    .import = {
      .library_name = library_name,
      .symbol_name = symbol_name
    },
  };
}

Value *
c_function_import(
  Program *program,
  const char *library_name,
  const char *forward_declaration
) {
  char *symbol_name_end = strchr(forward_declaration, '(');
  assert(symbol_name_end);
  char *symbol_name_start = symbol_name_end;
  while (symbol_name_start != forward_declaration && !isspace(*symbol_name_start)) {
    --symbol_name_start;
  }
  ++symbol_name_start;
  u64 length = symbol_name_end - symbol_name_start;
  char *symbol_name = temp_allocate_array(s8, length + 1);
  memcpy(symbol_name, symbol_name_start, length);
  symbol_name[length] = 0;

  Value *result = temp_allocate(Value);
  *result = (const Value) {
    .descriptor = c_function_descriptor(forward_declaration),
    .operand = import_symbol(program, library_name, symbol_name),
  };
  return result;
}

Import_Symbol *
program_find_import(
  const Program *program,
  const char *library_name,
  const char *symbol_name
) {
  for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
    Import_Library *lib = dyn_array_get(program->import_libraries, i);
    if (strcmp(lib->name, library_name) != 0) continue;

    for (u64 i = 0; i < dyn_array_length(lib->symbols); ++i) {
      Import_Symbol *symbol = dyn_array_get(lib->symbols, i);
      if (strcmp(symbol->name, symbol_name) == 0) {
        return symbol;
      }
    }
  }
  return 0;
}

#define FUNCTION_PROLOG_EPILOG_MAX_INSTRUCTION_COUNT 16

u64
estimate_max_code_size_in_bytes(
  Program *program
) {
  u64 total_instruction_count = 0;
  for (u64 i = 0; i < dyn_array_length(program->functions); ++i) {
    Function_Builder *builder = dyn_array_get(program->functions, i);
    total_instruction_count += dyn_array_length(builder->instructions);
    total_instruction_count += FUNCTION_PROLOG_EPILOG_MAX_INSTRUCTION_COUNT;
  }
  // TODO this should architecture-dependent
  const u64 max_bytes_per_instruction = 15;
  return total_instruction_count * max_bytes_per_instruction;
}










