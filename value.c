#include "value.h"
#include "function.h"
#include "source.h"

#ifdef _WIN32
#include "win32_runtime.h"
#endif

Slice
source_from_source_range(
  const Source_Range *source_range
) {
  return slice_sub_range(source_range->file->text, source_range->offsets);
}

Source_Position
source_file_offset_to_position(
  const Source_File *file,
  u64 offset
) {
  // Binary search in lines
  s64 left_bound = 0;
  s64 right_bound = dyn_array_length(file->line_ranges) - 1;
  s64 line_index = 0;
  while (left_bound <= right_bound) {
    line_index = left_bound + (right_bound - left_bound) / 2;
    Range_u64 *line = dyn_array_get(file->line_ranges, line_index);
    if (offset < line->from) {
      right_bound = line_index - 1;
    } else if (offset >= line->to) {
      left_bound = line_index + 1;
    } else {
      break;
    }
  }

  u64 column = offset - dyn_array_get(file->line_ranges, line_index)->from;
  return (Source_Position) {
    .line = line_index + 1,
    .column = column,
  };
}

void
source_range_print_start_position(
  const Source_Range *source_range
) {
  if (!source_range->file || !dyn_array_is_initialized(source_range->file->line_ranges)) {
    printf(":(0:0)\n");
    return;
  }
  Source_Position from_position =
    source_file_offset_to_position(source_range->file, source_range->offsets.from);
  slice_print(source_range->file->path);
  printf(":(%" PRIu64 ":%" PRIu64 ")\n", from_position.line, from_position.column);
}

static Fixed_Buffer *
mass_error_to_string(
  Mass_Error const* error
) {
  #define APPEND_SLICE(_SLICE_)\
    fixed_buffer_resizing_append_slice(&result, (_SLICE_))
  #define APPEND_LITERAL(_STRING_)\
    APPEND_SLICE(slice_literal(_STRING_))
  char number_buffer[128] = {0};
  Fixed_Buffer *result = fixed_buffer_make(.allocator = allocator_system, .capacity = 4000);
  switch(error->tag) {
    case Mass_Error_Tag_Unknown: {
      APPEND_LITERAL("Unknown Error: ");
      APPEND_SLICE(error->detailed_message);
    } break;
    case Mass_Error_Tag_Unimplemented: {
      APPEND_LITERAL("Unimplemented Feature: ");
      APPEND_SLICE(error->detailed_message);
    } break;
    case Mass_Error_Tag_Parse: {
      APPEND_LITERAL("Unable to parse the expression");
    } break;
    case Mass_Error_Tag_Unexpected_Token: {
      APPEND_LITERAL("Unexpected token");
      if (error->Unexpected_Token.expected.length) {
        APPEND_LITERAL(", expected '");
        APPEND_SLICE(error->Unexpected_Token.expected);
        APPEND_LITERAL("'");
      }
    } break;
    case Mass_Error_Tag_Undefined_Variable: {
      APPEND_LITERAL("Undefined variable ");
      APPEND_SLICE(error->Undefined_Variable.name);
    } break;
    case Mass_Error_Tag_Unknown_Field: {
      APPEND_LITERAL("Field ");
      APPEND_SLICE(error->Unknown_Field.name);
      APPEND_LITERAL("does not exist on type ");
      APPEND_SLICE(error->Unknown_Field.type->name);
    } break;
    case Mass_Error_Tag_Invalid_Identifier: {
      APPEND_LITERAL("Invalid identifier");
    } break;
    case Mass_Error_Tag_Expected_Static: {
      APPEND_LITERAL("Expected value to be static (compile-time known)");
    } break;
    case Mass_Error_Tag_Operator_Infix_Suffix_Conflict: {
      APPEND_LITERAL("There is already a infix or postfix operator ");
      APPEND_SLICE(error->Operator_Infix_Suffix_Conflict.symbol);
      APPEND_LITERAL(" defined in this scope");
    } break;
    case Mass_Error_Tag_Operator_Prefix_Conflict: {
      APPEND_LITERAL("There is already a prefix operator ");
      APPEND_SLICE(error->Operator_Infix_Suffix_Conflict.symbol);
      APPEND_LITERAL(" defined in this scope");
    } break;
    case Mass_Error_Tag_Non_Trailing_Default_Argument: {
      APPEND_LITERAL("An argument without a default value can not come after an argument that has one");
    } break;
    case Mass_Error_Tag_Type_Mismatch: {
      Mass_Error_Type_Mismatch const *mismatch = &error->Type_Mismatch;
      APPEND_LITERAL("Type mismatch: expected ");
      APPEND_SLICE(mismatch->expected->name);
      APPEND_LITERAL(", got ");
      APPEND_SLICE(mismatch->actual->name);
    } break;
    case Mass_Error_Tag_Integer_Range: {
      APPEND_LITERAL("Value does not fit into integer of type ");
      APPEND_SLICE(error->Integer_Range.descriptor->name);
    } break;
    case Mass_Error_Tag_Epoch_Mismatch: {
      Mass_Error_Epoch_Mismatch const *mismatch = &error->Epoch_Mismatch;
      APPEND_LITERAL("Trying to access a runtime variable with epoch ");
      snprintf(number_buffer, sizeof(number_buffer), "%"PRIu64, mismatch->value->epoch);
      APPEND_SLICE(slice_from_c_string(number_buffer));
      APPEND_LITERAL(" from a different epoch %");
      snprintf(number_buffer, sizeof(number_buffer), "%"PRIu64, mismatch->expected_epoch);
      APPEND_SLICE(slice_from_c_string(number_buffer));
      APPEND_LITERAL(". ");
    } break;
    case Mass_Error_Tag_Undecidable_Overload: {
      Mass_Error_Undecidable_Overload const *overloads = &error->Undecidable_Overload;
      APPEND_LITERAL("Could not decide which overload is better: ");
      APPEND_SLICE(overloads->a->descriptor->name);
      APPEND_LITERAL(" or ");
      APPEND_SLICE(overloads->b->descriptor->name);
    } break;
    case Mass_Error_Tag_No_Matching_Overload: {
      // TODO provide better error message with argument types
      APPEND_LITERAL("Could not find matching overload for call ");
      Slice source = source_from_source_range(&error->source_range);
      APPEND_SLICE(source);
    } break;
  }
  #undef APPEND_SLICE
  #undef APPEND_LITERAL
  return result;
}

static inline const void *
storage_static_as_c_type_internal(
  const Storage *storage,
  u64 byte_size
) {
  assert(storage->byte_size == byte_size);
  assert(storage->tag == Storage_Tag_Static);

  switch(byte_size) {
    case 1: {
      assert(storage->Static.memory.tag == Static_Memory_Tag_U8);
      return &storage->Static.memory.U8.value;
    }
    case 2: {
      assert(storage->Static.memory.tag == Static_Memory_Tag_U16);
      return &storage->Static.memory.U16.value;
    }
    case 4: {
      assert(storage->Static.memory.tag == Static_Memory_Tag_U32);
      return &storage->Static.memory.U32.value;
    }
    case 8: {
      assert(storage->Static.memory.tag == Static_Memory_Tag_U64);
      return &storage->Static.memory.U64.value;
    }
    default: {
      assert(storage->Static.memory.tag == Static_Memory_Tag_Heap);
      return storage->Static.memory.Heap.pointer;
    }
  }
}

#define storage_static_as_c_type(_OPERAND_, _TYPE_)\
  ((_TYPE_ *)storage_static_as_c_type_internal(_OPERAND_, sizeof(_TYPE_)))

#define DEFINE_VALUE_IS_AS_HELPERS(_C_TYPE_, _SUFFIX_)\
  static inline bool\
  value_is_##_SUFFIX_(\
    const Value *value\
  ) {\
    if (!value) return false;\
    return value->descriptor == &descriptor_##_SUFFIX_;\
  }\
  static inline const _C_TYPE_ *\
  value_as_##_SUFFIX_(\
    const Value *value\
  ) {\
    assert(value_is_##_SUFFIX_(value));\
    return storage_static_as_c_type(&value->storage, _C_TYPE_);\
  }

DEFINE_VALUE_IS_AS_HELPERS(Slice, slice)
DEFINE_VALUE_IS_AS_HELPERS(Symbol, symbol)
DEFINE_VALUE_IS_AS_HELPERS(Group, group)
DEFINE_VALUE_IS_AS_HELPERS(Number_Literal, number_literal)
DEFINE_VALUE_IS_AS_HELPERS(External_Symbol, external_symbol)

static inline Label *
program_get_label(
  Program *program,
  Label_Index label
) {
  return dyn_array_get(program->labels, label.value);
}

inline bool
same_value_type(
  Value *a,
  Value *b
);

bool
same_type(
  const Descriptor *a,
  const Descriptor *b
) {
  if (a->tag != b->tag) return false;
  switch(a->tag) {
    case Descriptor_Tag_Pointer: {
      if (
        a->Pointer.to->tag == Descriptor_Tag_Fixed_Size_Array &&
        same_type(a->Pointer.to->Fixed_Size_Array.item, b->Pointer.to)
      ) return true;
      if (
        b->Pointer.to->tag == Descriptor_Tag_Fixed_Size_Array &&
        same_type(b->Pointer.to->Fixed_Size_Array.item, a->Pointer.to)
      ) return true;
      if (
        a->Pointer.to->tag == Descriptor_Tag_Void ||
        b->Pointer.to->tag == Descriptor_Tag_Void
      ) {
        return true;
      }
      return same_type(a->Pointer.to, b->Pointer.to);
    }
    case Descriptor_Tag_Fixed_Size_Array: {
      return same_type(a->Fixed_Size_Array.item, b->Fixed_Size_Array.item) &&
        a->Fixed_Size_Array.length == b->Fixed_Size_Array.length;
    }
    case Descriptor_Tag_Void:
    case Descriptor_Tag_Opaque:
    case Descriptor_Tag_Struct: {
      return a == b;
    }
    case Descriptor_Tag_Function: {
      if (!same_type(a->Function.info.returns.descriptor, b->Function.info.returns.descriptor)) {
        return false;
      }
      if (
        dyn_array_length(a->Function.info.memory_layout.items) !=
        dyn_array_length(b->Function.info.memory_layout.items)
      ) {
        return false;
      }
      for (u64 i = 0; i < dyn_array_length(a->Function.info.memory_layout.items); ++i) {
        Memory_Layout_Item *a_arg = dyn_array_get(a->Function.info.memory_layout.items, i);
        Memory_Layout_Item *b_arg = dyn_array_get(b->Function.info.memory_layout.items, i);
        if(!same_type(a_arg->descriptor, b_arg->descriptor)) return false;
      }
      return true;
    }
    default: {
      assert(!"Unsupported descriptor type");
      return false;
    }
  }
}

u64
descriptor_alignment(
  const Descriptor *descriptor
) {
  if (descriptor->tag == Descriptor_Tag_Fixed_Size_Array) {
    return descriptor_alignment(descriptor->Fixed_Size_Array.item);
  }
  return descriptor_byte_size(descriptor);
}

u64
struct_byte_size(
  const Descriptor_Struct *Struct
) {
  u64 count = dyn_array_length(Struct->memory_layout.items);
  assert(count);
  u64 alignment = 0;
  u64 raw_size = 0;
  for (u64 i = 0; i < count; ++i) {
    Memory_Layout_Item *field = dyn_array_get(Struct->memory_layout.items, i);
    u64 field_alignment = descriptor_alignment(field->descriptor);
    alignment = u64_max(alignment, field_alignment);
    bool is_last_field = i == count - 1;
    u64 field_size_with_alignment = u64_max(field_alignment, descriptor_byte_size(field->descriptor));
    assert(field_size_with_alignment);
    assert(field->tag == Memory_Layout_Item_Tag_Base_Relative);
    // FIXME do padding when memory layout is created
    if (is_last_field) {
      raw_size = field->Base_Relative.offset + field_size_with_alignment;
    }
  }
  return u64_align(raw_size, alignment);
}

u64
descriptor_byte_size(
  const Descriptor *descriptor
) {
  assert(descriptor);
  switch(descriptor->tag) {
    case Descriptor_Tag_Void: {
      return 0;
    }
    case Descriptor_Tag_Struct: {
      return struct_byte_size(&descriptor->Struct);
    }
    case Descriptor_Tag_Opaque: {
      u64 size_of_byte = 8;
      return u64_to_u32((descriptor->Opaque.bit_size + (size_of_byte - 1)) / size_of_byte);
    }
    case Descriptor_Tag_Fixed_Size_Array: {
      return descriptor_byte_size(descriptor->Fixed_Size_Array.item) *
        descriptor->Fixed_Size_Array.length;
    }
    case Descriptor_Tag_Pointer:
    case Descriptor_Tag_Function: {
      return 8;
    }
    default: {
      assert(!"Unknown Descriptor Type");
    }
  }
  return 0;
}

s64
storage_static_value_up_to_s64(
  const Storage *operand
) {
  switch(operand->byte_size) {
    case 1: return *storage_static_as_c_type(operand, s8);
    case 2: return *storage_static_as_c_type(operand, s16);
    case 4: return *storage_static_as_c_type(operand, s32);
    case 8: return *storage_static_as_c_type(operand, s64);
    default: {
      panic("Unsupported integer immediate size");
      return 0;
    }
  }
}

u64
storage_static_value_up_to_u64(
  const Storage *operand
) {
  switch(operand->byte_size) {
    case 1: return *storage_static_as_c_type(operand, u8);
    case 2: return *storage_static_as_c_type(operand, u16);
    case 4: return *storage_static_as_c_type(operand, u32);
    case 8: return *storage_static_as_c_type(operand, u64);
    default: {
      panic("Unsupported integer immediate size");
      return 0;
    }
  }
}

void
print_operand(
  const Storage *operand
) {
  switch (operand->tag) {
    case Storage_Tag_None: {
      printf("_");
      break;
    }
    case Storage_Tag_Any: {
      printf("any");
      break;
    }
    case Storage_Tag_Eflags: {
      printf("eflags");
      break;
    }
    case Storage_Tag_Register: {
      u64 bits = operand->byte_size * 8;
      printf("r%"PRIu64, bits);
      break;
    }
    case Storage_Tag_Xmm: {
      u64 bits = operand->byte_size * 8;
      printf("xmm%"PRIu64, bits);
      break;
    }
    case Storage_Tag_Static: {
      switch(operand->byte_size) {
        case 1: {
          printf("imm8(0x%02x)", *storage_static_as_c_type(operand, u8));
          break;
        }
        case 2: {
          printf("imm16(0x%04x)", *storage_static_as_c_type(operand, u16));
          break;
        }
        case 4: {
          printf("imm32(0x%08x)", *storage_static_as_c_type(operand, u32));
          break;
        }
        case 8: {
          printf("imm64(0x%016" PRIx64 ")", *storage_static_as_c_type(operand, u64));
          break;
        }
        default: {
          panic("Unsupported immediate size when printing");
          break;
        }
      }
      break;
    }
    case Storage_Tag_Memory: {
      // TODO print better info
      u64 bits = operand->byte_size * 8;
      printf("m%"PRIu64, bits);
      break;
    }
    default: {
      printf("<unknown>");
      break;
    }
  }
}

#define define_register(reg_name, reg_index, reg_byte_size) \
const Storage reg_name = { \
  .tag = Storage_Tag_Register, \
  .byte_size = (reg_byte_size), \
  .Register = {.index = (reg_index)}, \
};
define_register(al, 0b0000, 1);

define_register(rax, 0b0000, 8);
define_register(rcx, 0b0001, 8);
define_register(rdx, 0b0010, 8);
define_register(rbx, 0b0011, 8);
define_register(rsp, 0b0100, 8);
define_register(rbp, 0b0101, 8);
define_register(rsi, 0b0110, 8);
define_register(rdi, 0b0111, 8);

define_register(eax, 0b0000, 4);
define_register(ecx, 0b0001, 4);
define_register(edx, 0b0010, 4);
define_register(ebx, 0b0011, 4);
define_register(esp, 0b0100, 4);
define_register(ebp, 0b0101, 4);
define_register(esi, 0b0110, 4);
define_register(edi, 0b0111, 4);

define_register(r8,  0b1000, 8);
define_register(r9,  0b1001, 8);
define_register(r10, 0b1010, 8);
define_register(r11, 0b1011, 8);
define_register(r12, 0b1100, 8);
define_register(r13, 0b1101, 8);
define_register(r14, 0b1110, 8);
define_register(r15, 0b1111, 8);

define_register(r8d,  0b1000, 4);
define_register(r9d,  0b1001, 4);
define_register(r10d, 0b1010, 4);
define_register(r11d, 0b1011, 4);
define_register(r12d, 0b1100, 4);
define_register(r13d, 0b1101, 4);
define_register(r14d, 0b1110, 4);
define_register(r15d, 0b1111, 4);
#undef define_register

#define define_xmm_register(reg_name, reg_index) \
const Storage reg_name##_32 = { \
  .tag = Storage_Tag_Xmm, \
  .byte_size = 4, \
  .Register = {.index = (reg_index)}, \
};\
const Storage reg_name##_64 = { \
  .tag = Storage_Tag_Xmm, \
  .byte_size = 8, \
  .Register = {.index = (reg_index)}, \
};
define_xmm_register(xmm0, 0b000);
define_xmm_register(xmm1, 0b001);
define_xmm_register(xmm2, 0b010);
define_xmm_register(xmm3, 0b011);
define_xmm_register(xmm4, 0b100);
define_xmm_register(xmm5, 0b101);
define_xmm_register(xmm6, 0b110);
define_xmm_register(xmm7, 0b111);
#undef define_xmm_register

static inline Label_Index
make_label(
  Program *program,
  Section *section,
  Slice name
) {
  Label_Index index = {dyn_array_length(program->labels)};
  dyn_array_push(program->labels, (Label) {.section = section, .name = name});
  return index;
}

static inline Storage
data_label32(
  Label_Index label_index,
  u64 byte_size
) {
  return (const Storage) {
    .tag = Storage_Tag_Memory,
    .byte_size = byte_size,
    .Memory.location = {
      .tag = Memory_Location_Tag_Instruction_Pointer_Relative,
      .Instruction_Pointer_Relative.label_index = label_index
    }
  };
}

static inline Storage
code_label32(
  Label_Index label_index
) {
  return (const Storage) {
    .tag = Storage_Tag_Memory,
    // FIXME this is set at 4 as otherwise current encoder is unhappy
    //       about the size mismatch. It should be zero instead.
    .byte_size = 4,
    .Memory.location = {
      .tag = Memory_Location_Tag_Instruction_Pointer_Relative,
      .Instruction_Pointer_Relative.label_index = label_index,
    }
  };
}

static inline Storage
storage_static_internal(
  void *value,
  u64 byte_size
) {
  Storage result = {
    .tag = Storage_Tag_Static,
    .byte_size = byte_size,
  };
  switch(byte_size) {
    case 1: {
      result.Static.memory.tag = Static_Memory_Tag_U8;
      result.Static.memory.U8.value = *(u8 *)value;
      break;
    }
    case 2: {
      result.Static.memory.tag = Static_Memory_Tag_U16;
      result.Static.memory.U16.value = *(u16 *)value;
      break;
    }
    case 4: {
      result.Static.memory.tag = Static_Memory_Tag_U32;
      result.Static.memory.U32.value = *(u32 *)value;
      break;
    }
    case 8: {
      result.Static.memory.tag = Static_Memory_Tag_U64;
      result.Static.memory.U64.value = *(u64 *)value;
      break;
    }
    default: {
      result.Static.memory.tag = Static_Memory_Tag_Heap;
      result.Static.memory.Heap.pointer = value;
      break;
    }
  }
  return result;
}

#define storage_static(_VALUE_)\
  storage_static_internal((_VALUE_), sizeof(*(_VALUE_)))

#define DEFINE_IMM_X(_BIT_SIZE_)\
  static inline Storage\
  imm##_BIT_SIZE_(\
    u##_BIT_SIZE_ value\
  ) {\
    return storage_static_internal(&value, (_BIT_SIZE_) / 8);\
  }

DEFINE_IMM_X(8)
DEFINE_IMM_X(16)
DEFINE_IMM_X(32)
DEFINE_IMM_X(64)

static inline Storage
imm_auto_8_or_32(
  s64 value
) {
  if (s64_fits_into_s8(value)) {
    return imm8((s8) value);
  }
  if (s64_fits_into_s32(value)) {
    return imm32((s32) value);
  }
  panic("Storage is does not fit into either s8 or s32");
  return (Storage){0};
}

static inline Storage
imm_auto(
  s64 value
) {
  if (s64_fits_into_s8(value)) {
    return imm8((s8) value);
  }
  if (s64_fits_into_s16(value)) {
    return imm16((s16) value);
  }
  if (s64_fits_into_s32(value)) {
    return imm32((s32) value);
  }
  return imm64(value);
}

static inline Storage
stack(
  s32 offset,
  u64 byte_size
) {
  assert(byte_size);
  return (const Storage) {
    .tag = Storage_Tag_Memory,
    .byte_size = byte_size,
    .Memory.location = {
      .tag = Memory_Location_Tag_Indirect,
      .Indirect = {
        .base_register = Register_SP,
        .offset = offset,
      }
    }
  };
}

Descriptor *
descriptor_struct_make(
  Allocator *allocator
) {
  Descriptor *descriptor = allocator_allocate(allocator, Descriptor);
  *descriptor = (Descriptor) {
    .tag = Descriptor_Tag_Struct,
    .Struct = {
      .memory_layout.items = dyn_array_make(Array_Memory_Layout_Item),
    }
  };
  return descriptor;
}

void
descriptor_struct_add_field(
  Descriptor *struct_descriptor,
  const Descriptor *field_descriptor,
  Slice field_name
) {
  u64 offset = 0;
  for (u64 i = 0; i < dyn_array_length(struct_descriptor->Struct.memory_layout.items); ++i) {
    Memory_Layout_Item *field = dyn_array_get(struct_descriptor->Struct.memory_layout.items, i);
    u64 size = descriptor_byte_size(field->descriptor);
    offset = u64_align(offset, size);
    offset += size;
  }

  u64 size = descriptor_byte_size(field_descriptor);
  offset = u64_align(offset, size);
  dyn_array_push(struct_descriptor->Struct.memory_layout.items, (Memory_Layout_Item) {
    .tag = Memory_Layout_Item_Tag_Base_Relative,
    .name = field_name,
    .descriptor = field_descriptor,
    .Base_Relative.offset = offset,
  });
}

static inline void
register_bitset_set(
  u64 *bitset,
  Register reg
) {
  *bitset |= 1llu << reg;
}

static inline void
register_bitset_unset(
  u64 *bitset,
  Register reg
) {
  *bitset &= ~(1llu << reg);
}

static inline bool
register_bitset_get(
  u64 bitset,
  Register reg
) {
  return !!(bitset & (1llu << reg));
}

static inline bool
storage_is_label(
  const Storage *operand
) {
  return operand->tag == Storage_Tag_Memory
    && operand->Memory.location.tag == Memory_Location_Tag_Instruction_Pointer_Relative;
}

static inline bool
storage_is_register_or_memory(
  const Storage *operand
) {
  return operand->tag == Storage_Tag_Register || operand->tag == Storage_Tag_Memory;
}

static inline bool
storage_is_register_index(
  const Storage *storage,
  Register reg_index
) {
  return storage->tag == Storage_Tag_Register && storage->Register.index == reg_index;
}

bool
storage_static_equal_internal(
  const Descriptor *a_descriptor,
  void *a_memory,
  const Descriptor *b_descriptor,
  void *b_memory
) {
  if (!same_type(a_descriptor, b_descriptor)) return false;
  if (a_descriptor == &descriptor_slice) {
    return slice_equal(*(Slice *)a_memory, *(Slice *)b_memory);
  }
  u64 byte_size = descriptor_byte_size(a_descriptor);
  switch(a_descriptor->tag) {
    case Descriptor_Tag_Void: return true;
    // Opaques and pointers can be compared with memcmp
    case Descriptor_Tag_Opaque:
    case Descriptor_Tag_Pointer: {
      return memcmp(a_memory, b_memory, byte_size) == 0;
    }
    case Descriptor_Tag_Fixed_Size_Array: {
      // compare field by field
      if (a_descriptor->Fixed_Size_Array.length != b_descriptor->Fixed_Size_Array.length) {
        return false;
      }
      for (u64 i = 0; i < a_descriptor->Fixed_Size_Array.length; ++i) {
        const Descriptor *a_item = a_descriptor->Fixed_Size_Array.item;
        const Descriptor *b_item = b_descriptor->Fixed_Size_Array.item;
        u64 offset = descriptor_byte_size(a_item) * i;
        if (!storage_static_equal_internal(
          a_item, (s8 *)a_memory + offset, b_item, (s8 *)b_memory + offset
        )) {
          return false;
        }
      }
      break;
    }
    case Descriptor_Tag_Struct: {
      // compare field by field
      u64 a_field_count = dyn_array_length(a_descriptor->Struct.memory_layout.items);
      u64 b_field_count = dyn_array_length(b_descriptor->Struct.memory_layout.items);
      if (a_field_count != b_field_count) {
        return false;
      }
      for (u64 i = 0; i < a_field_count; ++i) {
        Memory_Layout_Item *a_field = dyn_array_get(a_descriptor->Struct.memory_layout.items, i);
        Memory_Layout_Item *b_field = dyn_array_get(b_descriptor->Struct.memory_layout.items, i);
        if (a_field->tag != b_field->tag) return false;
        switch(a_field->tag) {
          case Memory_Layout_Item_Tag_Base_Relative: {
            if (!storage_static_equal_internal(
              a_field->descriptor, (s8 *)a_memory + a_field->Base_Relative.offset,
              b_field->descriptor, (s8 *)b_memory + b_field->Base_Relative.offset
            )) {
              return false;
            }
          } break;
          case Memory_Layout_Item_Tag_Absolute: {
            panic("TODO");
          } break;
        }
      }
      break;
    }
    case Descriptor_Tag_Function: {
      panic("Unexpected static storage function");
      break;
    }
  }
  return true;
}

bool
storage_static_equal(
  const Value *a,
  const Value *b
) {
  assert(a->storage.tag == Storage_Tag_Static);
  assert(b->storage.tag == Storage_Tag_Static);
  assert(a->storage.byte_size == b->storage.byte_size);
  assert(descriptor_byte_size(a->descriptor) == a->storage.byte_size);
  assert(a->storage.Static.memory.tag == b->storage.Static.memory.tag);
  switch(a->storage.Static.memory.tag) {
    case Static_Memory_Tag_U8: {
      return a->storage.Static.memory.U8.value == b->storage.Static.memory.U8.value;
    }
    case Static_Memory_Tag_U16: {
      return a->storage.Static.memory.U16.value == b->storage.Static.memory.U16.value;
    }
    case Static_Memory_Tag_U32: {
      return a->storage.Static.memory.U32.value == b->storage.Static.memory.U32.value;
    }
    case Static_Memory_Tag_U64: {
      return a->storage.Static.memory.U64.value == b->storage.Static.memory.U64.value;
    }
    case Static_Memory_Tag_Heap: {
      return storage_static_equal_internal(
        a->descriptor, a->storage.Static.memory.Heap.pointer,
        b->descriptor, b->storage.Static.memory.Heap.pointer
      );
    }
    default: {
      panic("Unexpected Static_Memory_Tag");
      return false;
    }
  }
}

static inline bool
storage_equal(
  const Storage *a,
  const Storage *b
) {
  if (a->tag != b->tag) return false;
  if (a->byte_size != b->byte_size) return false;
  switch(a->tag) {
    case Storage_Tag_Eflags: {
      return a->Eflags.compare_type == b->Eflags.compare_type;
    }
    case Storage_Tag_Static: {
      // We need to know the memory layout (the descriptor) of the
      // static values to properly compare them. `memcmp` does not
      // work due to padding.
      panic("Static values must be compared using storage_static_equal");
      break;
    }
    case Storage_Tag_Memory: {
      const Memory_Location *a_location = &a->Memory.location;
      const Memory_Location *b_location = &b->Memory.location;
      if (a_location->tag != b_location->tag) return false;
      switch(a_location->tag) {
        case Memory_Location_Tag_Instruction_Pointer_Relative: {
          return a_location->Instruction_Pointer_Relative.label_index.value
            == b_location->Instruction_Pointer_Relative.label_index.value;
        }
        case Memory_Location_Tag_Indirect: {
          return (
            a_location->Indirect.base_register == b_location->Indirect.base_register &&
            a_location->Indirect.maybe_index_register.has_value == b_location->Indirect.maybe_index_register.has_value &&
            a_location->Indirect.maybe_index_register.index == b_location->Indirect.maybe_index_register.index &&
            a_location->Indirect.offset == b_location->Indirect.offset
          );
          break;
        }
      }
      panic("Internal Error: Unexpected Memory_Location_Tag");
      return false;
    }
    case Storage_Tag_Xmm:
    case Storage_Tag_Register: {
      return a->Register.index == b->Register.index;
    }
    case Storage_Tag_Any: {
      return false; // Is this the semantics I want though?
    }
    case Storage_Tag_None: {
      return true;
    }
  }
  panic("Unknown operand type");
  return false;
}

bool
instruction_equal(
  const Instruction *a,
  const Instruction *b
) {
  if (a->type != b->type) return false;
  switch(a->type) {
    case Instruction_Type_Assembly: {
      if (a->assembly.mnemonic != b->assembly.mnemonic) return false;
      for (u64 i = 0; i < countof(a->assembly.operands); ++i) {
        const Storage *a_storage = &a->assembly.operands[i];
        const Storage *b_storage = &b->assembly.operands[i];
        if (a_storage->tag != b_storage->tag) return false;
        // FIXME Use immediates instead of static storage for instructions
        if (a_storage->tag == Storage_Tag_Static) {
          assert(a_storage->byte_size == b_storage->byte_size);
          assert(a_storage->Static.memory.tag == b_storage->Static.memory.tag);

          switch(a_storage->Static.memory.tag) {
            case Static_Memory_Tag_U8: {
              return a_storage->Static.memory.U8.value == b_storage->Static.memory.U8.value;
            }
            case Static_Memory_Tag_U16: {
              return a_storage->Static.memory.U16.value == b_storage->Static.memory.U16.value;
            }
            case Static_Memory_Tag_U32: {
              return a_storage->Static.memory.U32.value == b_storage->Static.memory.U32.value;
            }
            case Static_Memory_Tag_U64: {
              return a_storage->Static.memory.U64.value == b_storage->Static.memory.U64.value;
            }
            case Static_Memory_Tag_Heap: {
              return memcmp(
                a_storage->Static.memory.Heap.pointer,
                b_storage->Static.memory.Heap.pointer,
                a_storage->byte_size
              ) == 0;
            }
            default: {
              panic("Unexpected Static_Memory_Tag");
              return false;
            }
          }
        }
        if (!storage_equal(a_storage, b_storage)) return false;
      }
      break;
    }
    case Instruction_Type_Label: {
      return a->label.value == b->label.value;
    }
    case Instruction_Type_Bytes: {
      if (a->Bytes.length != b->Bytes.length) return false;
      return !!memcmp(a->Bytes.memory, b->Bytes.memory, a->Bytes.length);
    }
  }
  return true;
}

static inline Value *
value_init_internal(
  Compiler_Source_Location compiler_source_location,
  Value *result,
  u64 epoch,
  const Descriptor *descriptor,
  Storage storage,
  Source_Range source_range
) {
  *result = (Value) {
    .epoch = epoch,
    .descriptor = descriptor,
    .storage = storage,
    .source_range = source_range,
    .compiler_source_location = compiler_source_location,
  };
  return result;
}

#define value_init(...) value_init_internal(\
  COMPILER_SOURCE_LOCATION, ##__VA_ARGS__\
)

static inline Value *
value_make_internal(
  Compiler_Source_Location compiler_source_location,
  Execution_Context *context,
  const Descriptor *descriptor,
  Storage storage,
  Source_Range source_range
) {
  return value_init_internal(
    compiler_source_location,
    allocator_allocate(context->allocator, Value),
    context->epoch,
    descriptor,
    storage,
    source_range
  );
}

#define value_make(...) value_make_internal(\
  COMPILER_SOURCE_LOCATION, ##__VA_ARGS__\
)

static inline Value *
value_number_literal(
  const Allocator *allocator,
  Slice digits,
  Number_Base base,
  Source_Range source_range
) {
  Number_Literal *literal = allocator_allocate(allocator, Number_Literal);
  u64 bits = 0;
  bool ok = true;
  switch(base) {
    case Number_Base_2: bits = slice_parse_binary(digits, &ok); break;
    case Number_Base_10: bits = slice_parse_u64(digits, &ok); break;
    case Number_Base_16: bits = slice_parse_hex(digits, &ok); break;
    default: panic("Internal Error: Unexpected number base"); break;
  }
  if (!ok) panic("Internal Error: Mismatch between number tokenizer and parser");

  *literal = (Number_Literal) {
    .base = base,
    .negative = false,
    .bits = bits,
  };
  return value_init(
    allocator_allocate(allocator, Value),
    VALUE_STATIC_EPOCH, &descriptor_number_literal, storage_static(literal), source_range
  );
}

Label_Index
allocate_section_memory(
  Program *program,
  Section *section,
  u64 byte_size,
  u64 alignment
) {
  virtual_memory_buffer_allocate_bytes(&section->buffer, byte_size, alignment);
  u64 offset_in_data_section = section->buffer.occupied - byte_size;

  Label_Index label_index = make_label(program, section, slice_literal("global"));
  Label *label = program_get_label(program, label_index);
  label->offset_in_section = u64_to_u32(offset_in_data_section);
  label->resolved = true;

  return label_index;
}

static inline Value *
value_global_internal(
  Compiler_Source_Location compiler_source_location,
  Execution_Context *context,
  Descriptor *descriptor,
  Source_Range source_range
) {
  Program *program = context->program;
  Section *section = &program->memory.sections.rw_data;
  u64 byte_size = descriptor_byte_size(descriptor);
  u64 alignment = descriptor_alignment(descriptor);

  Label_Index label_index = allocate_section_memory(context->program, section, byte_size, alignment);
  return value_make_internal(
    compiler_source_location, context, descriptor, data_label32(label_index, byte_size), source_range
  );
}
#define value_global(...)\
  value_global_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

static inline Storage
storage_eflags(
  Compare_Type compare_type
) {
  return (Storage){
    .tag = Storage_Tag_Eflags,
    .byte_size = 1,
    .Eflags = { .compare_type = compare_type }
  };
}

// TODO consider adding explicit boolean descriptor type
#define value_from_compare(_allocator_, _compare_type_, _SOURCE_RANGE_) value_make_internal(\
  COMPILER_SOURCE_LOCATION, _allocator_, &descriptor_s8, storage_eflags(_compare_type_), (_SOURCE_RANGE_)\
)

#define value_from_s64(_CONTEXT_, _integer_, _SOURCE_RANGE_) value_make_internal(\
  COMPILER_SOURCE_LOCATION, (_CONTEXT_), &descriptor_s64, imm64((_integer_)), (_SOURCE_RANGE_)\
)

#define value_from_s32(_CONTEXT_, _integer_, _SOURCE_RANGE_) value_make_internal(\
  COMPILER_SOURCE_LOCATION, (_CONTEXT_), &descriptor_s32, imm32((_integer_)), (_SOURCE_RANGE_)\
)

#define value_from_s16(_CONTEXT_, _integer_, _SOURCE_RANGE_) value_make_internal(\
  COMPILER_SOURCE_LOCATION, (_CONTEXT_), &descriptor_s16, imm16((_integer_)), (_SOURCE_RANGE_)\
)

#define value_from_s8(_CONTEXT_, _integer_, _SOURCE_RANGE_) value_make_internal(\
  COMPILER_SOURCE_LOCATION, (_CONTEXT_), &descriptor_s8, imm8((_integer_)), (_SOURCE_RANGE_)\
)

#define value_from_u64(_CONTEXT_, _integer_, _SOURCE_RANGE_) value_make_internal(\
  COMPILER_SOURCE_LOCATION, (_CONTEXT_), &descriptor_u64, imm64((_integer_)), (_SOURCE_RANGE_)\
)

#define value_from_u32(_CONTEXT_, _integer_, _SOURCE_RANGE_) value_make_internal(\
  COMPILER_SOURCE_LOCATION, (_CONTEXT_), &descriptor_u32, imm32((_integer_)), (_SOURCE_RANGE_)\
)

#define value_from_u16(_CONTEXT_, _integer_, _SOURCE_RANGE_) value_make_internal(\
  COMPILER_SOURCE_LOCATION, (_CONTEXT_), &descriptor_u16, imm16((_integer_)), (_SOURCE_RANGE_)\
)

#define value_from_u8(_CONTEXT_, _integer_, _SOURCE_RANGE_) value_make_internal(\
  COMPILER_SOURCE_LOCATION, (_CONTEXT_), &descriptor_u8, imm8((_integer_), (_SOURCE_RANGE_)\
)

static inline Value *
value_from_signed_immediate_internal(
  Compiler_Source_Location location,
  Execution_Context *context,
  s64 value,
  Source_Range source_range
) {
  if (s64_fits_into_s8(value)) {
    return value_make_internal(
      location, context, &descriptor_s8, imm8((s8)value), source_range
    );
  }
  if (s64_fits_into_s16(value)) {
    return value_make_internal(
      location, context, &descriptor_s16, imm16((s16)value), source_range
    );
  }
  if (s64_fits_into_s32(value)) {
    return value_make_internal(
      location, context, &descriptor_s32, imm32((s32)value), source_range
    );
  }
  return value_make_internal(
    location, context, &descriptor_s64, imm64(value), source_range
  );
}
#define value_from_signed_immediate(...)\
  value_from_signed_immediate_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

static inline Value *
value_from_unsigned_immediate_internal(
  Compiler_Source_Location location,
  Execution_Context *context,
  u64 value,
  Source_Range source_range
) {
  if (u64_fits_into_u8(value)) {
    return value_make_internal(
      location, context, &descriptor_u8, imm8((u8)value), source_range
    );
  }
  if (u64_fits_into_u16(value)) {
    return value_make_internal(
      location, context, &descriptor_u16, imm16((u16)value), source_range
    );
  }
  if (u64_fits_into_u32(value)) {
    return value_make_internal(
      location, context, &descriptor_u32, imm32((u32)value), source_range
    );
  }
  return value_make_internal(
    location, context, &descriptor_u64, imm64(value), source_range
  );
}
#define value_from_unsigned_immediate(...)\
  value_from_unsigned_immediate_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

static inline Storage
storage_register_for_descriptor(
  Register reg,
  const Descriptor *descriptor
) {
  u64 byte_size = descriptor_byte_size(descriptor);
  assert(byte_size == 1 || byte_size == 2 || byte_size == 4 || byte_size == 8);

  Storage result = {
    .tag = register_is_xmm(reg) ? Storage_Tag_Xmm : Storage_Tag_Register,
    .Register.index = reg,
    .byte_size = byte_size,
  };
  return result;
}

static inline Value *
value_register_for_descriptor_internal(
  Compiler_Source_Location compiler_source_location,
  Execution_Context *context,
  Register reg,
  const Descriptor *descriptor,
  Source_Range source_range
) {
  return value_make_internal(
    compiler_source_location, context, descriptor,
    storage_register_for_descriptor(reg, descriptor),
    source_range
  );
}
#define value_register_for_descriptor(...)\
  value_register_for_descriptor_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

static inline Value *
value_specific_temporary_register_for_descriptor_internal(
  Compiler_Source_Location compiler_source_location,
  Execution_Context *context,
  Register reg,
  const Descriptor *descriptor,
  Source_Range source_range
) {
  register_acquire(context->builder, reg);
  Value *value = value_make_internal(
    compiler_source_location, context, descriptor,
    storage_register_for_descriptor(reg, descriptor),
    source_range
  );
  value->is_temporary = true;
  context->builder->code_block.register_occupied_values[reg] = value;
  return value;
}

#define value_specific_temporary_register_for_descriptor(...)\
  value_specific_temporary_register_for_descriptor_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

static inline Value *
value_temporary_register_for_descriptor_internal(
  Compiler_Source_Location compiler_source_location,
  Execution_Context *context,
  const Descriptor *descriptor,
  Source_Range source_range
) {
  return value_specific_temporary_register_for_descriptor_internal(
    compiler_source_location,
    context,
    register_find_available(context->builder),
    descriptor,
    source_range
  );
}

#define value_temporary_register_for_descriptor(...)\
  value_temporary_register_for_descriptor_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

static inline void
value_release_if_temporary(
  Function_Builder *builder,
  Value *value
) {
  if (!value || !value->is_temporary) return;
  switch (value->storage.tag) {
    case Storage_Tag_Register: {
      register_release(builder, value->storage.Register.index);
      break;
    }
    case Storage_Tag_Xmm: {
      register_release(builder, value->storage.Xmm.index);
      break;
    }
    case Storage_Tag_Memory: {
      // @Volatile :TemporaryRegisterForIndirectMemory
      Memory_Location *location = &value->storage.Memory.location;
      assert(location->tag == Memory_Location_Tag_Indirect);
      Register reg = location->Indirect.base_register;
      if (reg != Register_SP) {
        register_release(builder, reg);
      }
      // TODO what about index register
      break;
    }
    case Storage_Tag_Any:
    case Storage_Tag_None:
    case Storage_Tag_Eflags:
    case Storage_Tag_Static: {
      panic("Unexpected temporary storage tag");
      break;
    }
  }
}

static inline void *
rip_value_pointer_from_label_index(
  Program *program,
  Label_Index label_index
) {
  Label *label = program_get_label(program, label_index);
  assert(label->resolved);
  return (s8 *)label->section->buffer.memory + label->offset_in_section;
}


void *
rip_value_pointer(
  Program *program,
  Value *value
) {
  assert(storage_is_label(&value->storage));
  return rip_value_pointer_from_label_index(
    program, value->storage.Memory.location.Instruction_Pointer_Relative.label_index
  );
}

static inline Value *
value_global_c_string_from_slice_internal(
  Compiler_Source_Location compiler_source_location,
  Execution_Context *context,
  Slice slice,
  Source_Range source_range
) {
  s32 length = (s32)slice.length + 1;
  Descriptor *descriptor = allocator_allocate(context->allocator, Descriptor);
  *descriptor = (Descriptor) {
    .tag = Descriptor_Tag_Fixed_Size_Array,
    .Fixed_Size_Array = {
      .item = &descriptor_u8,
      .length = length,
    },
  };

  Value *string_value =
    value_global_internal(compiler_source_location, context, descriptor, source_range);
  s8 *memory = rip_value_pointer(context->program, string_value);
  memcpy(memory, slice.bytes, slice.length);
  memory[length - 1] = 0;
  return string_value;
}
#define value_global_c_string_from_slice(...)\
  value_global_c_string_from_slice_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

Descriptor *
descriptor_pointer_to(
  const Allocator *allocator,
  const Descriptor *descriptor
) {
  Descriptor *result = allocator_allocate(allocator, Descriptor);
  *result = (const Descriptor) {
    .tag = Descriptor_Tag_Pointer,
    .Pointer = {.to = descriptor },
  };
  return result;
}

Descriptor *
descriptor_array_of(
  Allocator *allocator,
  Descriptor *descriptor,
  u32 length
) {
  Descriptor *result = allocator_allocate(allocator, Descriptor);
  *result = (Descriptor) {
    .tag = Descriptor_Tag_Fixed_Size_Array,
    .Fixed_Size_Array = {
      .item = descriptor,
      .length = length,
    },
  };
  return result;
}

fn_type_opaque
value_as_function(
  const Jit *jit,
  Value *value
) {
  assert(storage_is_label(&value->storage));
  Label *label = program_get_label(
    jit->program, value->storage.Memory.location.Instruction_Pointer_Relative.label_index
  );
  Section *section = label->section;
  assert(section == &jit->program->memory.sections.code);
  s8 *target = section->buffer.memory + label->offset_in_section;
  return (fn_type_opaque)target;
}

static Storage
function_argument_storage_for_index(
  const Allocator *allocator,
  const Function_Info *function,
  const Descriptor *arg_descriptor,
  u64 argument_index,
  Function_Argument_Mode mode
) {
  u64 byte_size = descriptor_byte_size(arg_descriptor);

  // :ReturnTypeLargerThanRegister
  // If return type is larger than register, the pointer to stack location
  // where it needs to be written to is passed as the first argument
  // shifting registers for actual arguments by one
  if (descriptor_byte_size(function->returns.descriptor) > 8) {
    argument_index++;
  }

  Register general_registers[] = {Register_C, Register_D, Register_R8, Register_R9};
  Register float_registers[] = {Register_Xmm0, Register_Xmm1, Register_Xmm2, Register_Xmm3};

  assert(countof(general_registers) == countof(float_registers));

  if (argument_index < countof(general_registers)) {
    Register *registers = descriptor_is_float(arg_descriptor) ? float_registers : general_registers;
    Register reg = registers[argument_index];
    if (byte_size <= 8) {
      return storage_register_for_descriptor(reg, arg_descriptor);
    } else {
      switch(mode) {
        case Function_Argument_Mode_Call: {
          // For the caller we pretend that the type is a pointer since we do not have references
          const Descriptor *pointer_descriptor = descriptor_pointer_to(allocator, arg_descriptor);
          return storage_register_for_descriptor(reg, pointer_descriptor);
        }
        case Function_Argument_Mode_Body: {
          // Large arguments are passed "by reference", i.e. their memory location in the register
          return (Storage) {
            .tag = Storage_Tag_Memory,
            .byte_size = byte_size,
            .Memory.location = {
              .tag = Memory_Location_Tag_Indirect,
              .Indirect = { .base_register = reg },
            }
          };
        }
      }
      panic("Unexpected function argument mode");
      return (Storage){0};
    }
  } else {
    s32 offset = u64_to_s32(argument_index * 8);
    return stack(offset, byte_size);
  }
}

Value *
function_argument_value_at_index_internal(
  Compiler_Source_Location source_location,
  Execution_Context *context,
  const Function_Info *function,
  u64 argument_index,
  Function_Argument_Mode mode
) {
  Memory_Layout_Item *argument = dyn_array_get(function->memory_layout.items, argument_index);
  const Descriptor *arg_descriptor = argument->descriptor;
  Source_Range source_range = argument->source_range;
  Storage storage =
    function_argument_storage_for_index(context->allocator, function, arg_descriptor, argument_index, mode);
  return value_make_internal(source_location, context, arg_descriptor, storage, source_range);
}
#define function_argument_value_at_index(...)\
  function_argument_value_at_index_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

void
program_init(
  Allocator *allocator,
  Program *program
) {
  *program = (Program) {
    .labels = dyn_array_make(Array_Label, .capacity = 128, .allocator = allocator),
    .patch_info_array = dyn_array_make(Array_Label_Location_Diff_Patch_Info, .capacity = 128, .allocator = allocator),
    .import_libraries = dyn_array_make(Array_Import_Library, .capacity = 16, .allocator = allocator),
    .startup_functions = dyn_array_make(Array_Value_Ptr, .capacity = 16, .allocator = allocator),
    .relocations = dyn_array_make(Array_Relocation, .capacity = 16, .allocator = allocator),
    .functions = dyn_array_make(Array_Function_Builder, .capacity = 16, .allocator = allocator),
  };

  #define MAX_CODE_SIZE (640llu * 1024llu * 1024llu) // 640Mb
  #define MAX_RW_DATA_SIZE (640llu * 1024llu * 1024llu) // 640Mb
  #define MAX_RO_DATA_SIZE (640llu * 1024llu * 1024llu) // 640Mb
  // :FunctionTableCallbackMax2Gb
  // (640 + 640 + 640 == 1920) < 2048
  #define MAX_PROGRAM_SIZE (MAX_RW_DATA_SIZE + MAX_CODE_SIZE + MAX_RO_DATA_SIZE)
  virtual_memory_buffer_init(&program->memory.buffer, MAX_PROGRAM_SIZE);

  u64 offset_in_memory = 0;

  program->memory.sections.code = (Section){
    .buffer = {
      .memory = program->memory.buffer.memory + offset_in_memory,
      .capacity = MAX_CODE_SIZE,
    },
    .base_rva = u64_to_u32(offset_in_memory),
    .permissions = Section_Permissions_Execute,
  };
  offset_in_memory += program->memory.sections.code.buffer.capacity;

  program->memory.sections.rw_data = (Section){
    .buffer = {
      .memory = program->memory.buffer.memory + offset_in_memory,
      .capacity = MAX_RW_DATA_SIZE,
    },
    .base_rva = u64_to_u32(offset_in_memory),
    .permissions = Section_Permissions_Read | Section_Permissions_Write,
  };
  offset_in_memory += program->memory.sections.rw_data.buffer.capacity;

  program->memory.sections.ro_data = (Section){
    .buffer = {
      .memory = program->memory.buffer.memory + offset_in_memory,
      .capacity = MAX_RO_DATA_SIZE,
    },
    .base_rva = u64_to_u32(offset_in_memory),
    .permissions = Section_Permissions_Read,
  };
};

void
program_deinit(
  Program *program
) {
  for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
    Import_Library *library = dyn_array_get(program->import_libraries, i);
    dyn_array_destroy(library->symbols);
  }
  virtual_memory_buffer_deinit(&program->memory.buffer);
  dyn_array_destroy(program->labels);
  dyn_array_destroy(program->patch_info_array);
  dyn_array_destroy(program->import_libraries);
  dyn_array_destroy(program->functions);
  dyn_array_destroy(program->startup_functions);
  dyn_array_destroy(program->relocations);
}

void
jit_init(
  Jit *jit,
  Program *program
) {
  *jit = (Jit) {
    .import_library_handles = hash_map_make(Jit_Import_Library_Handle_Map),
    .program = program,
  };
}

void
jit_deinit(
  Jit *jit
) {
  program_deinit(jit->program);
  hash_map_destroy(jit->import_library_handles);
}

void
compilation_init(
  Compilation *compilation
) {
  *compilation = (Compilation) {
    .module_map = hash_map_make(Imported_Module_Map),
    .static_pointer_map = hash_map_make(Static_Pointer_Map),
    .jit = {0},
  };

  // Get 16 gigabytes of virtual space
  virtual_memory_buffer_init(&compilation->allocation_buffer, 16llu * 1024 * 1024 * 1024);
  compilation->allocator = virtual_memory_buffer_allocator_make(&compilation->allocation_buffer);

  compilation->result = allocator_allocate(compilation->allocator, Mass_Result);

  compilation->runtime_program = allocator_allocate(compilation->allocator, Program);
  program_init(compilation->allocator, compilation->runtime_program);

  compilation->root_scope = scope_make(compilation->allocator, 0);
  scope_define_builtins(compilation->allocator, compilation->root_scope);

  Scope *compiler_scope = scope_make(compilation->allocator, compilation->root_scope);
  compilation->compiler_module = (Module) {
    .source_file = {
      .path = slice_literal("__mass_internal__"),
    },
    .own_scope = compiler_scope,
    .export_scope = compiler_scope,
  };

  Program *jit_program = allocator_allocate(compilation->allocator, Program);
  program_init(compilation->allocator, jit_program);
  jit_init(&compilation->jit, jit_program);
}

void
compilation_deinit(
  Compilation *compilation
) {
  hash_map_destroy(compilation->module_map);
  hash_map_destroy(compilation->static_pointer_map);
  program_deinit(compilation->runtime_program);
  jit_deinit(&compilation->jit);
  virtual_memory_buffer_deinit(&compilation->allocation_buffer);
}

Execution_Context
execution_context_from_compilation(
  Compilation *compilation
) {
  return (Execution_Context) {
    .allocator = compilation->allocator,
    .program = compilation->runtime_program,
    .compilation = compilation,
    .scope = compilation->root_scope,
    .result = compilation->result,
  };
}

void
program_set_label_offset(
  Program *program,
  Label_Index label_index,
  u32 offset_in_section
) {
  Label *label = program_get_label(program, label_index);
  label->resolved = true;
  label->offset_in_section = offset_in_section;
}

static inline u32
program_resolve_label_to_rva(
  const Program *program,
  const Label *label
) {
  return label->section->base_rva + label->offset_in_section;
}

void
program_patch_labels(
  Program *program
) {
  for (
    u64 patch_index = 0;
    patch_index < dyn_array_length(program->patch_info_array);
    ++patch_index
  ) {
    Label_Location_Diff_Patch_Info *info = dyn_array_get(program->patch_info_array, patch_index);
    Label *target_label = program_get_label(program, info->target_label_index);
    assert(target_label->resolved);

    s64 from_rva = program_resolve_label_to_rva(program, &info->from);
    s64 target_rva = program_resolve_label_to_rva(program, target_label);

    s64 diff = target_rva - from_rva;
    *info->patch_target = s64_to_s32(diff);
  }
}

void
program_jit(
  Jit *jit
) {
  #ifdef _WIN32
  win32_program_jit(jit);
  #else
  panic("JIT compilation is (yet) not implemented for this system");
  #endif
}

Import_Library *
program_find_import_library(
  const Program *program,
  const Slice library_name
) {
  for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
    Import_Library *lib = dyn_array_get(program->import_libraries, i);
    if (slice_ascii_case_insensitive_equal(lib->name, library_name)) {
      return lib;
    }
  }
  return 0;
}

Import_Symbol *
import_library_find_symbol(
  const Import_Library *library,
  const Slice symbol_name
) {
  for (u64 i = 0; i < dyn_array_length(library->symbols); ++i) {
    Import_Symbol *symbol = dyn_array_get(library->symbols, i);
    if (slice_equal(symbol->name, symbol_name)) {
      return symbol;
    }
  }
  return 0;
}

Import_Symbol *
program_find_import(
  const Program *program,
  const Slice library_name,
  const Slice symbol_name
) {
  Import_Library *lib = program_find_import_library(program, library_name);
  if (!lib) return 0;
  return import_library_find_symbol(lib, symbol_name);
}

Storage
import_symbol(
  Execution_Context *context,
  const Slice library_name,
  const Slice symbol_name
) {
  Program *program = context->program;
  Import_Library *library = program_find_import_library(program, library_name);
  if (!library) {
    library = dyn_array_push(program->import_libraries, (Import_Library) {
      .name = library_name,
      .symbols = dyn_array_make(Array_Import_Symbol, .allocator = context->allocator),
    });
  }

  Import_Symbol *symbol = import_library_find_symbol(library, symbol_name);

  if (!symbol) {
    Label_Index label = make_label(program, &program->memory.sections.ro_data, symbol_name);
    symbol = dyn_array_push(library->symbols, (Import_Symbol) {
      .name = symbol_name,
      .label32 = label,
    });
  }

  u32 byte_size = 8;
  return data_label32(symbol->label32, byte_size);
}

inline bool
same_value_type(
  Value *a,
  Value *b
) {
  return same_type(a->descriptor, b->descriptor);
}

typedef enum {
  Literal_Cast_Result_Success,
  Literal_Cast_Result_Target_Not_An_Integer,
  Literal_Cast_Result_Target_Too_Small,
  Literal_Cast_Result_Target_Too_Big,
  Literal_Cast_Result_Unsigned_Target_For_Negative_Literal,
} Literal_Cast_Result;

Literal_Cast_Result
value_number_literal_cast_to(
  const Value *value,
  const Descriptor *target_descriptor,
  u64 *out_bits,
  u64 *out_bit_size
) {
  assert(value->descriptor == &descriptor_number_literal);
  assert(value->storage.tag == Storage_Tag_Static);

  if (!descriptor_is_integer(target_descriptor)) {
    return Literal_Cast_Result_Target_Not_An_Integer;
  }

  assert(value->storage.Static.memory.tag == Static_Memory_Tag_Heap);
  Number_Literal *literal = value->storage.Static.memory.Heap.pointer;

  u64 bits = literal->bits;
  u64 max = UINT64_MAX;
  u64 bit_size = target_descriptor->Opaque.bit_size;
  if (bit_size > 64) {
    return Literal_Cast_Result_Target_Too_Big;
  }
  u64 shift = 64 - bit_size;
  max >>= shift;
  if (descriptor_is_signed_integer(target_descriptor)) {
    max >>= 1;
    if (!literal->negative) max--;
  } else if (literal->negative) {
    return Literal_Cast_Result_Unsigned_Target_For_Negative_Literal;
  }

  if (bits > max) {
    return Literal_Cast_Result_Target_Too_Small;
  }

  if(literal->negative) {
    bits = UINT64_MAX - bits + 1;
  }
  *out_bits = bits;
  *out_bit_size = bit_size;
  return Literal_Cast_Result_Success;
}

bool
same_type_or_can_implicitly_move_cast(
  const Descriptor *target,
  const Descriptor *source
) {
  if (same_type(target, source)) return true;
  if (target == source) return true;
  if (target->tag != source->tag) return false;
  if (descriptor_is_integer(source) && descriptor_is_integer(target)) {
    if (
      descriptor_is_unsigned_integer(source) &&
      descriptor_byte_size(target) > descriptor_byte_size(source)
    ) {
      return true;
    } else if (
      descriptor_is_signed_integer(target) &&
      descriptor_byte_size(target) > descriptor_byte_size(source)
    ) {
      return true;
    }
  }
  return false;
}

bool
same_value_type_or_can_implicitly_move_cast(
  const Descriptor *target,
  Value *source
) {
  if (source->descriptor == &descriptor_number_literal) {
    // Allow literal `0` to be cast to a pointer
    if (target->tag == Descriptor_Tag_Pointer) {
      assert(source->storage.tag == Storage_Tag_Static);
      assert(source->storage.Static.memory.tag == Static_Memory_Tag_Heap);
      Number_Literal *literal = source->storage.Static.memory.Heap.pointer;
      return literal->bits == 0;
    } else {
      Literal_Cast_Result cast_result =
        value_number_literal_cast_to(source, target, &(u64){0}, &(u64){0});
      return cast_result == Literal_Cast_Result_Success;
    }
  }

  return same_type_or_can_implicitly_move_cast(target, value_or_lazy_value_descriptor(source));
}


