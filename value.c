#include "value.h"
#include "function.h"
#include "source.h"

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
    case Descriptor_Type_Float: {
      return descriptor_byte_size(a) == descriptor_byte_size(b);
    }
    case Descriptor_Type_Any:
    case Descriptor_Type_Type:
    default: {
      assert(!"Unsupported descriptor type");
      return false;
    }
  }
}

u32
descriptor_alignment(
  Descriptor *descriptor
) {
  if (descriptor->type == Descriptor_Type_Fixed_Size_Array) {
    return descriptor_alignment(descriptor->array.item);
  }
  return descriptor_byte_size(descriptor);
}

u32
struct_byte_size(
  const Descriptor_Struct *struct_
) {
  s64 count = dyn_array_length(struct_->fields);
  assert(count);
  u32 alignment = 0;
  u32 raw_size = 0;
  for (s32 i = 0; i < count; ++i) {
    Descriptor_Struct_Field *field = dyn_array_get(struct_->fields, i);
    u32 field_alignment = descriptor_alignment(field->descriptor);
    alignment = max(alignment, field_alignment);
    bool is_last_field = i == count - 1;
    u32 field_size_with_alignment = max(field_alignment, descriptor_byte_size(field->descriptor));
    if (is_last_field) {
      raw_size = field->offset + field_size_with_alignment;
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
    case Descriptor_Type_Float: {
      return descriptor->float_.byte_size;
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
    case Descriptor_Type_Any:
    case Descriptor_Type_Type:
    default: {
      assert(!"Unknown Descriptor Type");
    }
  }
  return 0;
}

void
print_operand(
  const Operand *operand
) {
  switch (operand->type) {
    case Operand_Type_None: {
      printf("_");
      break;
    }
    case Operand_Type_Any: {
      printf("any");
      break;
    }
    case Operand_Type_Eflags: {
      printf("eflags");
      break;
    }
    case Operand_Type_Register: {
      u32 bits = operand->byte_size * 8;
      printf("r%d", bits);
      break;
    }
    case Operand_Type_Xmm: {
      u32 bits = operand->byte_size * 8;
      printf("xmm%d", bits);
      break;
    }
    case Operand_Type_Immediate_8: {
      printf("imm8(0x%02x)", operand->imm8);
      break;
    }
    case Operand_Type_Immediate_16: {
      printf("imm16(0x%04x)", operand->imm16);
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
    case Operand_Type_Sib:
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
      printf("rip_import(");
      slice_print(operand->import.library_name);
      printf(":");
      slice_print(operand->import.library_name);
      printf(")");
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
const Operand reg_name##_32 = { \
  .type = Operand_Type_Xmm, \
  .byte_size = 4, \
  .reg = (reg_index), \
};\
const Operand reg_name##_64 = { \
  .type = Operand_Type_Xmm, \
  .byte_size = 8, \
  .reg = (reg_index), \
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
imm16(
  s16 value
) {
  return (const Operand) {
    .type = Operand_Type_Immediate_16,
    .byte_size = 2,
    .imm16 = value
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
imm_auto_8_or_32(
  s64 value
) {
  if (s64_fits_into_s8(value)) {
    return imm8((s8) value);
  }
  if (s64_fits_into_s32(value)) {
    return imm32((s32) value);
  }
  panic("Operand is does not fit into either s8 or s32");
  return (Operand){0};
}

inline Operand
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

Descriptor *
descriptor_struct_make() {
  Descriptor *descriptor = temp_allocate(Descriptor);
  *descriptor = (Descriptor) {
    .type = Descriptor_Type_Struct,
    .struct_ = {
      .fields = dyn_array_make(Array_Descriptor_Struct_Field),
    }
  };
  return descriptor;
}

void
descriptor_struct_add_field(
  Descriptor *struct_descriptor,
  Descriptor *field_descriptor,
  Slice field_name
) {
  u32 offset = 0;
  for (u64 i = 0; i < dyn_array_length(struct_descriptor->struct_.fields); ++i) {
    Descriptor_Struct_Field *field = dyn_array_get(struct_descriptor->struct_.fields, i);
    u32 size = descriptor_byte_size(field->descriptor);
    offset = u32_align(offset, size);
    offset += size;
  }

  u32 size = descriptor_byte_size(field_descriptor);
  offset = u32_align(offset, size);
  dyn_array_push(struct_descriptor->struct_.fields, (Descriptor_Struct_Field) {
    .name = field_name,
    .descriptor = field_descriptor,
    .offset = offset,
  });
}

static inline void
register_bitset_set(
  u64 *bitset,
  const Operand *operand
) {
  if (operand->type == Operand_Type_Register) {
    *bitset |= 1llu << operand->reg;
  } else {
    panic("TODO");
  }
}

static inline bool
register_bitset_get(
  u64 bitset,
  const Operand *operand
) {
  if (operand->type == Operand_Type_Register) {
     return !!(bitset & (1llu << operand->reg));
  } else {
    panic("TODO");
  }
  return false;
}

s64
operand_immediate_as_s64(
  Operand *operand
) {
  if (operand->type == Operand_Type_Immediate_8) return operand->imm8;
  if (operand->type == Operand_Type_Immediate_16) return operand->imm16;
  if (operand->type == Operand_Type_Immediate_32) return operand->imm32;
  if (operand->type == Operand_Type_Immediate_64) return operand->imm64;
  assert(!"Expected and immediate operand");
  return 0;
}

bool
operand_is_memory(
  Operand *operand
) {
  return (
    operand->type == Operand_Type_Memory_Indirect ||
    operand->type == Operand_Type_RIP_Relative ||
    operand->type == Operand_Type_Sib
  );
}

bool
operand_is_immediate(
  Operand *operand
) {
  if (operand->type == Operand_Type_Immediate_8) return true;
  if (operand->type == Operand_Type_Immediate_16) return true;
  if (operand->type == Operand_Type_Immediate_32) return true;
  if (operand->type == Operand_Type_Immediate_64) return true;
  return false;
}

static inline bool
operand_equal(
  const Operand *a,
  const Operand *b
) {
  if (a->type != b->type) return false;
  if (a->byte_size != b->byte_size) return false;
  switch(a->type) {
    case Operand_Type_Eflags: {
      return a->compare_type == b->compare_type;
    }
    case Operand_Type_Immediate_8: {
      return a->imm8 == b->imm8;
    }
    case Operand_Type_Immediate_16: {
      return a->imm16 == b->imm16;
    }
    case Operand_Type_Immediate_32: {
      return a->imm32 == b->imm32;
    }
    case Operand_Type_Immediate_64: {
      return a->imm64 == b->imm64;
    }
    case Operand_Type_Label_32: {
      return a->label32 == b->label32;
    }
    case Operand_Type_Xmm:
    case Operand_Type_Register: {
      return a->reg == b->reg;
    }
    case Operand_Type_Any: {
      return false; // Is this the semantics I want though?
    }
    case Operand_Type_None: {
      return true;
    }
    case Operand_Type_Sib: {
      return (
        a->sib.scale == b->sib.scale &&
        a->sib.index == b->sib.index &&
        a->sib.base == b->sib.base
      );
    }
    case Operand_Type_Memory_Indirect: {
      return (
        a->indirect.reg == b->indirect.reg &&
        a->indirect.displacement == b->indirect.displacement
      );
    }
    case Operand_Type_RIP_Relative_Import: {
      return (
        slice_equal(a->import.library_name, b->import.library_name) &&
        slice_equal(a->import.symbol_name, b->import.symbol_name)
      );
    }
    case Operand_Type_RIP_Relative: {
      return a->rip_offset_in_data == b->rip_offset_in_data;
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
  if (a->mnemonic != b->mnemonic) return false;
  for (u64 i = 0; i < countof(a->operands); ++i) {
    if (!operand_equal(&a->operands[i], &b->operands[i])) {
      return false;
    }
  }
  return true;
}

Value *
value_from_compare_internal(
  Compiler_Source_Location compiler_source_location,
  Compare_Type compare_type
) {
  Value *result = temp_allocate(Value);
  *result = (Value) {
    // TODO consider adding explicit boolean descriptor type
    .descriptor = &descriptor_s8,
    .operand = {
      .type = Operand_Type_Eflags,
      .byte_size = 1,
      .compare_type = compare_type,
    },
    .compiler_source_location = compiler_source_location,
  };
  return result;
}

#define value_from_compare(...) value_from_compare_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

Value *
value_global_internal(
  Compiler_Source_Location compiler_source_location,
  Program *program,
  Descriptor *descriptor
) {
  u32 byte_size = descriptor_byte_size(descriptor);
  u32 alignment = descriptor_alignment(descriptor);
  s8 *address = fixed_buffer_allocate_bytes(program->data_buffer, byte_size, alignment);
  s64 offset_in_data_section = address - program->data_buffer->memory;

  Value *result = temp_allocate(Value);
  *result = (Value) {
    .descriptor = descriptor,
    .operand = {
      .type = Operand_Type_RIP_Relative,
      .byte_size = byte_size,
      .rip_offset_in_data = (s64) offset_in_data_section,
    },
    .compiler_source_location = compiler_source_location,
  };
  return result;
}

Value *
value_any_internal(
  Compiler_Source_Location compiler_source_location
) {
  Value *result = temp_allocate(Value);
  *result = (Value) {
    .descriptor = &descriptor_any,
    .operand = {.type = Operand_Type_Any},
    .compiler_source_location = compiler_source_location,
  };
  return result;
}

#define value_any() value_any_internal(COMPILER_SOURCE_LOCATION)


Value *
value_from_f64_internal(
  Compiler_Source_Location compiler_source_location,
  Program *program,
  f32 float_value
) {
  Value *result = value_global_internal(compiler_source_location, program, &descriptor_f64);
  f64 *memory = rip_value_pointer(program, result);
  *memory = float_value;
  return result;
}

#define value_from_f64(...) value_from_f64_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

Value *
value_from_f32_internal(
  Compiler_Source_Location compiler_source_location,
  Program *program,
  f32 float_value
) {
  Value *result = value_global_internal(compiler_source_location, program, &descriptor_f32);
  f32 *memory = rip_value_pointer(program, result);
  *memory = float_value;
  return result;
}

#define value_from_f32(...) value_from_f32_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

Value *
value_from_s64_internal(
  Compiler_Source_Location compiler_source_location,
  s64 integer
) {
  Value *result = temp_allocate(Value);
  *result = (Value) {
    .descriptor = &descriptor_s64,
    .operand = imm64(integer),
    .compiler_source_location = compiler_source_location,
  };
  return result;
}
#define value_from_s64(...) value_from_s64_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

Value *
value_from_s32_internal(
  Compiler_Source_Location compiler_source_location,
  s32 integer
) {
  Value *result = temp_allocate(Value);
  *result = (const Value) {
    .descriptor = &descriptor_s32,
    .operand = imm32(integer),
    .compiler_source_location = compiler_source_location,
  };
  return result;
}

#define value_from_s32(...) value_from_s32_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

Value *
value_from_s16_internal(
  Compiler_Source_Location compiler_source_location,
  s16 integer
) {
  Value *result = temp_allocate(Value);
  *result = (const Value) {
    .descriptor = &descriptor_s16,
    .operand = imm16(integer),
    .compiler_source_location = compiler_source_location,
  };
  return result;
}

#define value_from_s16(...) value_from_s16_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

Value *
value_from_s8_internal(
  Compiler_Source_Location compiler_source_location,
  s8 integer
) {
  Value *result = temp_allocate(Value);
  *result = (const Value) {
    .descriptor = &descriptor_s8,
    .operand = imm8(integer),
    .compiler_source_location = compiler_source_location,
  };
  return result;
}

#define value_from_s8(...) value_from_s8_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

inline Value *
value_from_signed_immediate_internal(
  Compiler_Source_Location compiler_source_location,
  s64 value
) {
  if (s64_fits_into_s8(value)) {
    return value_from_s8_internal(compiler_source_location, (s8) value);
  }
  if (s64_fits_into_s16(value)) {
    return value_from_s16_internal(compiler_source_location, (s16) value);
  }
  if (s64_fits_into_s32(value)) {
    return value_from_s32_internal(compiler_source_location, (s32) value);
  }
  return value_from_s64_internal(compiler_source_location, value);
}
#define value_from_signed_immediate(...)\
  value_from_signed_immediate_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

Value *
value_byte_size_internal(
  Compiler_Source_Location compiler_source_location,
  Value *value
) {
  s32 byte_size = descriptor_byte_size(value->descriptor);
  return value_from_s32_internal(compiler_source_location, byte_size);
}
#define value_byte_size(...)\
  value_byte_size_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)


Value *
value_register_for_descriptor_internal(
  Compiler_Source_Location compiler_source_location,
  Register reg,
  Descriptor *descriptor
) {
  u32 byte_size = descriptor_byte_size(descriptor);
  assert(byte_size == 1 || byte_size == 2 || byte_size == 4 || byte_size == 8);

  Operand_Type operand_type =
    descriptor->type == Descriptor_Type_Float
    ? Operand_Type_Xmm
    : Operand_Type_Register;
  Value *result = temp_allocate(Value);
  *result = (const Value) {
    .descriptor = descriptor,
    .operand = {
      .type = operand_type,
      .reg = reg,
      .byte_size = byte_size,
    },
    .compiler_source_location = compiler_source_location,
  };
  return result;
}
#define value_register_for_descriptor(...)\
  value_register_for_descriptor_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

void *
rip_value_pointer(
  Program *program,
  Value *value
) {
  assert(value->operand.type == Operand_Type_RIP_Relative);
  return program->data_buffer->memory + value->operand.rip_offset_in_data;
}

Value *
value_global_c_string_from_slice_internal(
  Compiler_Source_Location compiler_source_location,
  Program *program,
  Slice slice
) {
  s32 length = (s32)slice.length + 1;
  Descriptor *descriptor = temp_allocate(Descriptor);
  *descriptor = (Descriptor) {
    .type = Descriptor_Type_Fixed_Size_Array,
    .array = {
      .item = &descriptor_s8,
      .length = length,
    },
  };

  Value *string_value = value_global_internal(compiler_source_location, program, descriptor);
  s8 *memory = rip_value_pointer(program, string_value);
  memcpy(memory, slice.bytes, slice.length);
  memory[length - 1] = 0;
  return string_value;
}
#define value_global_c_string_from_slice(...)\
  value_global_c_string_from_slice_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

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
  Program *program,
  Value *value
) {
  assert(value->operand.type == Operand_Type_Label_32);
  assert(program->jit_buffer);
  s8 *target =
    program->jit_buffer->memory + program->code_base_rva + value->operand.label32->target_rva;
  return (fn_type_opaque)target;
}

#define value_as_function(_value_, _type_) \
  ((_type_)helper_value_as_function(program_, _value_))


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
function_push_argument_internal(
  Compiler_Source_Location compiler_source_location,
  Descriptor_Function *function,
  Descriptor *arg_descriptor
) {
  u32 byte_size = descriptor_byte_size(arg_descriptor);
  assert(byte_size <= 8);
  switch (dyn_array_length(function->arguments)) {
    case 0: {
      Value *value = arg_descriptor->type == Descriptor_Type_Float
        ? value_register_for_descriptor_internal(compiler_source_location, Register_Xmm0, arg_descriptor)
        : value_register_for_descriptor_internal(compiler_source_location, Register_C, arg_descriptor);
      return *dyn_array_push(function->arguments, value);
    }
    case 1: {
      Value *value = arg_descriptor->type == Descriptor_Type_Float
        ? value_register_for_descriptor_internal(compiler_source_location, Register_Xmm1, arg_descriptor)
        : value_register_for_descriptor_internal(compiler_source_location, Register_D, arg_descriptor);
      return *dyn_array_push(function->arguments, value);
    }
    case 2: {
      Value *value = arg_descriptor->type == Descriptor_Type_Float
        ? value_register_for_descriptor_internal(compiler_source_location, Register_Xmm2, arg_descriptor)
        : value_register_for_descriptor_internal(compiler_source_location, Register_R8, arg_descriptor);
      return *dyn_array_push(function->arguments, value);
    }
    case 3: {
      Value *value = arg_descriptor->type == Descriptor_Type_Float
        ? value_register_for_descriptor_internal(compiler_source_location, Register_Xmm3, arg_descriptor)
        : value_register_for_descriptor_internal(compiler_source_location, Register_R9, arg_descriptor);
      return *dyn_array_push(function->arguments, value);
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
#define function_push_argument(...)\
  function_push_argument_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

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
    case Descriptor_Type_Float: {
      assert(!"TODO");
    }
    case Descriptor_Type_Any:
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

Program *
program_init(
  Program *program
) {
  *program = (Program) {
    .data_buffer = fixed_buffer_make(.allocator = allocator_system, .capacity = 128 * 1024),
    .import_libraries = dyn_array_make(Array_Import_Library, .capacity = 16),
    .functions = dyn_array_make(Array_Function_Builder, .capacity = 16),
    .errors = dyn_array_make(Array_Parse_Error, .capacity = 16),
    .global_scope = scope_make(0),
  };

  scope_define_value(program->global_scope, slice_literal("f64"), type_f64_value);
  scope_define_value(program->global_scope, slice_literal("f32"), type_f32_value);
  scope_define_value(program->global_scope, slice_literal("s64"), type_s64_value);
  scope_define_value(program->global_scope, slice_literal("s32"), type_s32_value);
  scope_define_value(program->global_scope, slice_literal("s16"), type_s16_value);
  scope_define_value(program->global_scope, slice_literal("s8"), type_s8_value);
  return program;
};

void
program_deinit(
  Program *program
) {
  for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
    Import_Library *library = dyn_array_get(program->import_libraries, i);
    dyn_array_destroy(library->symbols);
  }
  dyn_array_destroy(program->import_libraries);
}

EXCEPTION_DISPOSITION
program_test_exception_handler(
  EXCEPTION_RECORD *ExceptionRecord,
  u64 EstablisherFrame,
  CONTEXT *ContextRecord,
  DISPATCHER_CONTEXT *DispatcherContext
) {
  RUNTIME_FUNCTION *function = DispatcherContext->FunctionEntry;
  u64 absolute_function_begin_address = DispatcherContext->ImageBase + function->BeginAddress;
  u64 relative_instruction_byte_offset =
    DispatcherContext->ControlPc - absolute_function_begin_address;

  Function_Builder **builder_pointer = DispatcherContext->HandlerData;
  Function_Builder *builder = *builder_pointer;

  if (!builder->program->is_stack_unwinding_in_progress) {
    builder->program->is_stack_unwinding_in_progress = true;
    printf("Unhandled Exception: ");
    switch(ExceptionRecord->ExceptionCode) {
      case EXCEPTION_ACCESS_VIOLATION: {
        printf("Access Violation");
        break;
      }
      case EXCEPTION_ARRAY_BOUNDS_EXCEEDED: {
        printf("Hardware Array Bounds Check Failed");
        break;
      }
      case EXCEPTION_BREAKPOINT: {
        printf("User Breakpoint");
        break;
      }
      case EXCEPTION_DATATYPE_MISALIGNMENT: {
        printf("Misaligned Read / Write");
        break;
      }
      case EXCEPTION_FLT_DENORMAL_OPERAND: {
        printf("Denormal Float Value Result");
        break;
      }
      case EXCEPTION_FLT_DIVIDE_BY_ZERO: {
        printf("Float Divide By Zero");
        break;
      }
      case EXCEPTION_FLT_INEXACT_RESULT: {
        printf("Float Inexact Decimal Fraction");
        break;
      }
      case EXCEPTION_FLT_INVALID_OPERATION: {
        printf("Float Invalid Operation");
        break;
      }
      case EXCEPTION_FLT_OVERFLOW: {
        printf("Float Overflow");
        break;
      }
      case EXCEPTION_FLT_STACK_CHECK: {
        printf("Stack Overflow After Float Operation");
        break;
      }
      case EXCEPTION_FLT_UNDERFLOW: {
        printf("Float Underflow");
        break;
      }
      case EXCEPTION_ILLEGAL_INSTRUCTION: {
        printf("Illegal Machine Code Instruction");
        break;
      }
      case EXCEPTION_IN_PAGE_ERROR: {
        printf("Read Missing Memory Page");
        break;
      }
      case EXCEPTION_INT_DIVIDE_BY_ZERO: {
        printf("Integer Divide By Zero");
        break;
      }
      case EXCEPTION_INT_OVERFLOW: {
        printf("Integer Overflow");
        break;
      }
      case EXCEPTION_INVALID_DISPOSITION: {
        printf("Invalid Disposition From An Exception Handler");
        break;
      }
      case EXCEPTION_NONCONTINUABLE_EXCEPTION: {
        printf("Continue Execution After Noncontinuable Exception");
        break;
      }
      case EXCEPTION_PRIV_INSTRUCTION: {
        printf("Instruction Not Allowed In Current CPU Mode");
        break;
      }
      case EXCEPTION_SINGLE_STEP: {
        printf("Single Step Instruction");
        break;
      }
      case EXCEPTION_STACK_OVERFLOW: {
        printf("Stack Overflow");
        break;
      }
      default: {
        printf("Unknown");
        break;
      }
    }
    printf(".\n");
  }

  u64 current_offset = 0;
  for (u64 i = 0; i < dyn_array_length(builder->code_block.instructions); ++i) {
    Instruction *instruction = dyn_array_get(builder->code_block.instructions, i);
    // DispatcherContext->ControlPc provides IP *after* the instruction that caused the exception
    // so we add instruction byte size before comparing
    current_offset += instruction->encoded_byte_size;
    if (current_offset == relative_instruction_byte_offset) {
      const Source_Location *source_location = instruction->source_location;
      printf(
        "  at %.*s:(%llu:%llu)\n",
        u64_to_s32(source_location->filename.length),
        source_location->filename.bytes,
        source_location->line,
        source_location->column
      );
    }
  }

  (void)builder;
  return ExceptionContinueSearch;
}


Fixed_Buffer *
program_end(
  Program *program
) {
  if (dyn_array_is_initialized(program->import_libraries)) {
    for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
      Import_Library *lib = dyn_array_get(program->import_libraries, i);
      const char *library_name = slice_to_c_string(temp_allocator, lib->name);
      HINSTANCE dll_handle = LoadLibraryA(library_name);
      assert(dll_handle);

      for (u64 i = 0; i < dyn_array_length(lib->symbols); ++i) {
        Import_Symbol *symbol = dyn_array_get(lib->symbols, i);

        const char *symbol_name = slice_to_c_string(temp_allocator, symbol->name);
        fn_type_opaque fn_address = GetProcAddress(dll_handle, symbol_name);
        assert(fn_address);
        u64 *rip_target = fixed_buffer_append_u64(program->data_buffer, (u64)fn_address);
        symbol->offset_in_data = u64_to_s32((s8*)rip_target - program->data_buffer->memory);
      }
    }
  }

  u64 code_segment_size = estimate_max_code_size_in_bytes(program) + MAX_ESTIMATED_TRAMPOLINE_SIZE;
  u64 function_count = dyn_array_length(program->functions);
  u64 global_data_size = u64_align(program->data_buffer->occupied, 16);
  u64 unwind_info_size = u64_align(sizeof(UNWIND_INFO) * function_count, sizeof(DWORD));
  u64 data_segment_size = global_data_size + unwind_info_size;
  u64 program_size = data_segment_size + code_segment_size;

  // Making a contiguous buffer holding both data and memory to ensure
  Fixed_Buffer *result_buffer = fixed_buffer_make(
    .allocator = allocator_system,
    .capacity = program_size,
  );

  { // Copying and repointing the data segment into contiguous buffer
    fixed_buffer_append_slice(result_buffer, fixed_buffer_as_slice(program->data_buffer));
    // TODO rename to temp_data_buffer and turn it into a bucket buffer
    fixed_buffer_destroy(program->data_buffer);
    // Nobody should be trying to read or write to this temp buffer after compilation
    program->data_buffer = 0;
  }

  // Since we are writing to the same buffer both data segment and code segment,
  // and there is no weird file vs virtual address stuff going on like in PE32,
  // we can just use natural offsets and ignore the base RVA
  program->data_base_rva = 0;
  program->code_base_rva = 0;

  UNWIND_INFO *unwind_info_array = fixed_buffer_allocate_bytes(
    result_buffer, sizeof(UNWIND_INFO) * function_count, sizeof(DWORD)
  );

  RUNTIME_FUNCTION *fn_exception_info = allocator_allocate_array(
    allocator_system, RUNTIME_FUNCTION, function_count
  );

  s8 *code_memory = result_buffer->memory + result_buffer->occupied;
  u64 trampoline_address = (u64)program_test_exception_handler;

  u32 trampoline_virtual_address = make_trampoline(program, result_buffer, trampoline_address);

  for (u64 i = 0; i < function_count; ++i) {
    Function_Builder *builder = dyn_array_get(program->functions, i);
    UNWIND_INFO *unwind_info = &unwind_info_array[i];
    u32 unwind_data_rva = s64_to_u32((s8 *)unwind_info - result_buffer->memory);
    fn_encode(result_buffer, builder, &fn_exception_info[i], unwind_info, unwind_data_rva);
    {
      unwind_info->Flags |= UNW_FLAG_EHANDLER;
      u64 exception_handler_index = u64_align(unwind_info->CountOfCodes, 2);
      u32 *exception_handler_address = (u32 *)&unwind_info->UnwindCode[exception_handler_index];
      *exception_handler_address = trampoline_virtual_address;
      Function_Builder **exception_data = (void *)(exception_handler_address + 1);
      *exception_data = builder;
    }
  }

  // Making code executable
  VirtualProtect(code_memory, code_segment_size, PAGE_EXECUTE_READ, &(DWORD){0});

  if (!RtlAddFunctionTable(
    fn_exception_info, u64_to_u32(function_count), (s64) result_buffer->memory
  )) {
    panic("Could not add function table definition");
  }
  program->jit_buffer = result_buffer;
  return result_buffer;
}

void
program_push_error_from_slice(
  Program *program,
  Source_Location location,
  Slice message
) {
  dyn_array_push(program->errors, (Parse_Error) { message,  location });
}

void
program_push_error_from_bucket_buffer(
  Program *program,
  Source_Location location,
  Bucket_Buffer buffer
) {
  Fixed_Buffer *message_buffer = bucket_buffer_to_fixed_buffer(temp_allocator, buffer);
  Slice message = fixed_buffer_as_slice(message_buffer);
  program_push_error_from_slice(program, location, message);
  bucket_buffer_destroy(buffer);
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

Operand
import_symbol(
  Program *program,
  const Slice library_name,
  const Slice symbol_name
) {
  Import_Library *library = program_find_import_library(program, library_name);
  if (!library) {
    library = dyn_array_push(program->import_libraries, (Import_Library) {
      .name = library_name,
      .name_rva = 0xCCCCCCCC,
      .rva = 0xCCCCCCCC,
      .image_thunk_rva = 0xCCCCCCCC,
      .symbols = dyn_array_make(Array_Import_Symbol, .allocator = temp_allocator),
    });
  }

  Import_Symbol *symbol = import_library_find_symbol(library, symbol_name);

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
    .operand = import_symbol(
      program,
      slice_from_c_string(library_name),
      slice_from_c_string(symbol_name)
    ),
  };
  return result;
}

#define FUNCTION_PROLOG_EPILOG_MAX_INSTRUCTION_COUNT 16

u64
estimate_max_code_size_in_bytes(
  Program *program
) {
  u64 total_instruction_count = 0;
  for (u64 i = 0; i < dyn_array_length(program->functions); ++i) {
    Function_Builder *builder = dyn_array_get(program->functions, i);
    total_instruction_count += dyn_array_length(builder->code_block.instructions);
    total_instruction_count += FUNCTION_PROLOG_EPILOG_MAX_INSTRUCTION_COUNT;
  }
  // TODO this should be architecture-dependent
  const u64 max_bytes_per_instruction = 15;
  return total_instruction_count * max_bytes_per_instruction;
}

inline bool
same_value_type(
  Value *a,
  Value *b
) {
  return same_type(a->descriptor, b->descriptor);
}

bool
same_value_type_or_can_implicitly_move_cast(
  Value *target,
  Value *source
) {
  if (same_value_type(target, source)) return true;
  // Allow literal `0` to be cast to a pointer
  if (
    target->descriptor->type == Descriptor_Type_Pointer &&
    source->descriptor->type == Descriptor_Type_Integer &&
    operand_is_immediate(&source->operand) &&
    operand_immediate_as_s64(&source->operand) == 0
  ) {
    return true;
  }
  if (target->descriptor->type != source->descriptor->type) return false;
  if (target->descriptor->type == Descriptor_Type_Integer) {
    if (descriptor_byte_size(target->descriptor) > descriptor_byte_size(source->descriptor)) {
      return true;
    }
  }
  return false;
}









