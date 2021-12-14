#include "value.h"
#include "function.h"
#include "source.h"
#include "calling_convention.h"

static inline bool
mass_value_is_compile_time_known(
  const Value *value
) {
  if (!value) return false;
  if (value->descriptor != &descriptor_lazy_value) {
    if (value->storage.tag == Storage_Tag_Immediate) return true;
    if (value->storage.tag == Storage_Tag_Static) return true;
    if (value->storage.tag == Storage_Tag_None) return true;
  }
  return false;
}

static inline Slice
source_from_source_range(
  Compilation *compilation,
  const Source_Range *source_range
) {
  if (!source_range->file) return (Slice){0};
  Range_u64 offsets = {source_range->offsets.from, source_range->offsets.to};
  return slice_sub_range(source_range->file->text, offsets);
}

static void
source_range_print_start_position(
  Compilation *compilation,
  const Source_Range *source_range
) {
  if (!source_range->file) {
    printf(":(0:0)\n");
    return;
  }
  Source_Position from_position = {.line = 0, .column = 0};
  const Source_File *file = source_range->file;
  Slice source = file->text;
  u64 line_start_offset = 0;
  for (u64 i = 0; i < source.length; ++i) {
    if (source.bytes[i] != '\n') continue;
    // if the range starts at the \n it is considered part of the current line, not the next one
    if (i == source_range->offsets.from) break;
    from_position.line += 1;
    if (i > source_range->offsets.from) break;
    line_start_offset = i + 1;
  }
  from_position.column = source_range->offsets.from - line_start_offset;
  slice_print(file->path);
  printf(":(%" PRIu64 ":%" PRIu64 ")\n", from_position.line, from_position.column);
}
#define APPEND_SLICE(_SLICE_)\
  fixed_buffer_resizing_append_slice(&result, (_SLICE_))
#define APPEND_LITERAL(_STRING_)\
  APPEND_SLICE(slice_literal(_STRING_))

static void
mass_error_append_descriptor(
  Fixed_Buffer *result,
  const Descriptor *descriptor
) {
  if (descriptor->tag == Descriptor_Tag_Pointer_To) {
    APPEND_LITERAL("&");
    mass_error_append_descriptor(result, descriptor->Pointer_To.descriptor);
    return;
  }
  APPEND_SLICE(descriptor->name);
}

static void
mass_error_append_function_signature_string(
  Compilation *compilation,
  Fixed_Buffer *result,
  const Function_Info *info
) {
  APPEND_LITERAL("(");
  bool first = true;
  DYN_ARRAY_FOREACH(Function_Parameter, arg, info->parameters) {
    if (first) first = false;
    else APPEND_LITERAL(", ");
    APPEND_SLICE(arg->symbol->name);
    APPEND_LITERAL(" : ");
    mass_error_append_descriptor(result, arg->descriptor);
  }
  APPEND_LITERAL(")");
  if (info->flags & Function_Info_Flags_Compile_Time) {
    APPEND_LITERAL(" => ");
  } else {
    APPEND_LITERAL(" -> ");
  }
  switch(info->returns.tag) {
    case Function_Return_Tag_Inferred: {
      APPEND_LITERAL("_");
    } break;
    case Function_Return_Tag_Exact: {
      mass_error_append_descriptor(result, info->returns.Exact.descriptor);
    } break;
    case Function_Return_Tag_Generic: {
      Slice source = source_from_source_range(
        compilation, &info->returns.Generic.type_expression.source_range
      );
      APPEND_SLICE(source);
    } break;
  }
}

static Fixed_Buffer *
mass_error_to_string(
  Compilation *compilation,
  Mass_Error const* error
) {
  Fixed_Buffer *result = fixed_buffer_make(.allocator = allocator_system, .capacity = 4000);
  switch(error->tag) {
    case Mass_Error_Tag_Unimplemented: {
      APPEND_LITERAL("Unimplemented Feature: ");
      APPEND_SLICE(error->detailed_message);
    } break;
    case Mass_Error_Tag_User_Defined: {
      APPEND_SLICE(error->User_Defined.name);
      APPEND_LITERAL(": ");
      APPEND_SLICE(error->detailed_message);
    } break;
    case Mass_Error_Tag_Assignment_To_Constant: {
      APPEND_LITERAL("Trying to assign to a constant value");
    } break;
    case Mass_Error_Tag_Parse: {
      APPEND_LITERAL("Unable to parse the expression");
      if (error->detailed_message.length) {
        APPEND_LITERAL(": ");
        APPEND_SLICE(error->detailed_message);
      }
    } break;
    case Mass_Error_Tag_Unexpected_Token: {
      APPEND_LITERAL("Unexpected token '");
      APPEND_LITERAL("'");
      Slice source = source_from_source_range(compilation, &error->source_range);
      APPEND_SLICE(source);
      if (error->Unexpected_Token.expected.length) {
        APPEND_LITERAL(", expected '");
        APPEND_SLICE(error->Unexpected_Token.expected);
        APPEND_LITERAL("'");
      }
    } break;
    case Mass_Error_Tag_Undefined_Variable: {
      APPEND_LITERAL("Undefined variable '");
      APPEND_SLICE(error->Undefined_Variable.name);
      APPEND_LITERAL("'");
    } break;
    case Mass_Error_Tag_Redefinition: {
      // TODO report original location somehow
      APPEND_LITERAL("Redefinition of binding ");
      APPEND_SLICE(error->Redefinition.name);
    } break;
    case Mass_Error_Tag_Circular_Dependency: {
      APPEND_LITERAL("Circular dependency when resolving ");
      APPEND_SLICE(error->Circular_Dependency.name);
    } break;
    case Mass_Error_Tag_Unknown_Field: {
      APPEND_LITERAL("Field ");
      APPEND_SLICE(error->Unknown_Field.name);
      APPEND_LITERAL(" does not exist on type ");
      APPEND_SLICE(error->Unknown_Field.type->name);
    } break;
    case Mass_Error_Tag_Invalid_Identifier: {
      APPEND_LITERAL("Invalid identifier");
    } break;
    case Mass_Error_Tag_Dynamic_Library_Load: {
      APPEND_LITERAL("Unable to load a dynamic library ");
      APPEND_SLICE(error->Dynamic_Library_Load.library_name);
    } break;
    case Mass_Error_Tag_Dynamic_Library_Symbol_Not_Found: {
      APPEND_LITERAL("Unable to resolve a symbol ");
      APPEND_SLICE(error->Dynamic_Library_Symbol_Not_Found.symbol_name);
      APPEND_LITERAL(" from a dynamic library ");
      APPEND_SLICE(error->Dynamic_Library_Symbol_Not_Found.library_name);
    } break;
    case Mass_Error_Tag_File_Open: {
      APPEND_LITERAL("Can not open file ");
      APPEND_SLICE(error->File_Open.path);
    } break;
    case Mass_Error_Tag_File_Too_Large: {
      APPEND_LITERAL("File ");
      APPEND_SLICE(error->File_Too_Large.path);
      APPEND_LITERAL(" is larger than 4GB");
    } break;
    case Mass_Error_Tag_Expected_Static: {
      APPEND_LITERAL("Expected value to be static (compile-time known)");
    } break;
    case Mass_Error_Tag_Operator_Fixity_Conflict: {
      if (error->Operator_Fixity_Conflict.fixity == Operator_Fixity_Prefix) {
        APPEND_LITERAL("There is already a prefix operator ");
      } else {
        APPEND_LITERAL("There is already a infix or postfix operator ");
      }
      APPEND_SLICE(error->Operator_Fixity_Conflict.symbol);
      APPEND_LITERAL(" defined in this scope");
    } break;
    case Mass_Error_Tag_Non_Trailing_Default_Argument: {
      APPEND_LITERAL("An argument without a default value can not come after an argument that has one");
    } break;
    case Mass_Error_Tag_Type_Mismatch: {
      Mass_Error_Type_Mismatch const *mismatch = &error->Type_Mismatch;
      APPEND_LITERAL("Type mismatch: expected ");
      mass_error_append_descriptor(result, mismatch->expected);
      APPEND_LITERAL(", got ");
      mass_error_append_descriptor(result, mismatch->actual);
    } break;
    case Mass_Error_Tag_Integer_Range: {
      APPEND_LITERAL("Value does not fit into integer of type ");
      APPEND_SLICE(error->Integer_Range.descriptor->name);
    } break;
    case Mass_Error_Tag_Epoch_Mismatch: {
      APPEND_LITERAL("Trying to access a value from the wrong execution epoch ");
      Slice source = source_from_source_range(compilation, &error->source_range);
      APPEND_SLICE(source);
      APPEND_LITERAL(".\n");
      APPEND_LITERAL(
        "This happens when you access value from runtime in compile-time execution "
        "or a runtime value from a different stack frame than current function call."
      );
    } break;
    case Mass_Error_Tag_Undecidable_Overload: {
      Mass_Error_Undecidable_Overload const *overloads = &error->Undecidable_Overload;
      APPEND_LITERAL("Could not decide which overload is better: \n  ");
      mass_error_append_function_signature_string(compilation, result, overloads->a);
      APPEND_LITERAL("\n  ");
      mass_error_append_function_signature_string(compilation, result, overloads->b);
    } break;
    case Mass_Error_Tag_Non_Function_Overload: {
      APPEND_LITERAL("Trying to define a non-function overload ");
      Slice source = source_from_source_range(compilation, &error->source_range);
      APPEND_SLICE(source);
    } break;
    case Mass_Error_Tag_No_Matching_Overload: {
      // TODO provide better error message with argument types
      APPEND_LITERAL("Could not find matching overload for call ");
      Slice source = source_from_source_range(compilation, &error->source_range);
      APPEND_SLICE(source);
    } break;
    case Mass_Error_Tag_No_Runtime_Use: {
      if (error->detailed_message.length) {
        APPEND_SLICE(error->detailed_message);
      } else {
        APPEND_LITERAL("Value can't be used at runtime");
      }
      APPEND_LITERAL(":\n");
      Slice source = source_from_source_range(compilation, &error->source_range);
      APPEND_SLICE(source);
    } break;
    case Mass_Error_Tag_Recursive_Intrinsic_Use: {
      APPEND_LITERAL("Recursive calls to intrinsics are not allowed.\n");
      APPEND_LITERAL("Since an intrinsic is called during compilation of a fn body\n");
      APPEND_LITERAL("if it contains a direct or indirect call to itself\n");
      APPEND_LITERAL("as the body is not read - we can not execute it.");
    } break;
  }
  #undef APPEND_SLICE
  #undef APPEND_LITERAL
  return result;
}

static inline const void *
get_static_storage_with_bit_size(
  const Storage *storage,
  Bits bit_size
) {
  assert(storage->bit_size.as_u64 == bit_size.as_u64);
  assert(storage->tag == Storage_Tag_Static);
  return storage->Static.pointer;
}

static bool
same_function_signature(
  const Function_Info *a_info,
  const Function_Info *b_info
) {
  // TODO should the generics / inferred be handled here
  if (a_info->returns.tag != b_info->returns.tag) return false;
  if (a_info->returns.tag != Function_Return_Tag_Exact) return false;
  if (!same_type(a_info->returns.Exact.descriptor, b_info->returns.Exact.descriptor)) {
    return false;
  }

  if (dyn_array_length(a_info->parameters) != dyn_array_length(b_info->parameters)) {
    return false;
  }
  for (u64 i = 0; i < dyn_array_length(a_info->parameters); ++i) {
    Function_Parameter *a_arg = dyn_array_get(a_info->parameters, i);
    Function_Parameter *b_arg = dyn_array_get(b_info->parameters, i);
    if(!same_type(a_arg->descriptor, b_arg->descriptor)) return false;
  }
  return true;
}

static bool
same_type(
  const Descriptor *a,
  const Descriptor *b
) {
  if (descriptor_is_implicit_pointer(a)) {
    a = a->Pointer_To.descriptor;
  }
  if (descriptor_is_implicit_pointer(b)) {
    b = b->Pointer_To.descriptor;
  }
  if (a->tag != b->tag) return false;
  if (a->bit_size.as_u64 != b->bit_size.as_u64) return false;
  if (a->brand && b->brand && a->brand != b->brand) return false;
  switch(a->tag) {
    case Descriptor_Tag_Void:
    case Descriptor_Tag_Float: {
      return true;
    }
    case Descriptor_Tag_Integer: {
      return a->Integer.is_signed == b->Integer.is_signed;
    }
    case Descriptor_Tag_Pointer_To: {
      return same_type(a->Pointer_To.descriptor, b->Pointer_To.descriptor);
    }
    case Descriptor_Tag_Fixed_Size_Array: {
      return same_type(a->Fixed_Size_Array.item, b->Fixed_Size_Array.item) &&
        a->Fixed_Size_Array.length == b->Fixed_Size_Array.length;
    }
    case Descriptor_Tag_Struct:
    case Descriptor_Tag_Raw: {
      return a == b;
    }
    case Descriptor_Tag_Function_Instance: {
      return same_function_signature(a->Function_Instance.info, b->Function_Instance.info);
    }
    default: {
      assert(!"Unsupported descriptor type");
      return false;
    }
  }
}

static inline u64
descriptor_byte_size(
  const Descriptor *descriptor
) {
  u64 bit_size = descriptor->bit_size.as_u64;
  u64 byte_size = (bit_size + (CHAR_BIT - 1)) / CHAR_BIT;
  if (byte_size * CHAR_BIT != bit_size) {
    panic("TODO support non-byte aligned sizes");
  }
  return byte_size;
}

static inline u64
descriptor_byte_alignment(
  const Descriptor *descriptor
) {
  u64 bit_alignment = descriptor->bit_alignment.as_u64;
  u64 byte_alignment = (bit_alignment + (CHAR_BIT - 1)) / CHAR_BIT;
  if (byte_alignment * CHAR_BIT != bit_alignment) {
    panic("TODO support non-byte aligned sizes");
  }
  return byte_alignment;
}

static inline Label *
make_label(
  const Allocator *allocator,
  Program *program,
  Section *section,
  Slice name
) {
  Label *label = allocator_allocate(allocator, Label);
  *label = (Label) {
    .program = program,
    .section = section,
    .name = name,
  };
  return label;
}

static inline Storage
data_label32(
  Label *label,
  Bits bit_size
) {
  return (const Storage) {
    .tag = Storage_Tag_Memory,
    .bit_size = bit_size,
    .Memory.location = {
      .tag = Memory_Location_Tag_Instruction_Pointer_Relative,
      .Instruction_Pointer_Relative.label = label
    }
  };
}

static inline Storage
code_label32(
  Label *label
) {
  return (const Storage) {
    .tag = Storage_Tag_Memory,
    // FIXME this is set at 32 as otherwise current encoder is unhappy
    //       about the size mismatch. It should be zero instead.
    .bit_size = {32},
    .Memory.location = {
      .tag = Memory_Location_Tag_Instruction_Pointer_Relative,
      .Instruction_Pointer_Relative.label = label,
    }
  };
}

static inline Storage
storage_static_heap(
  const void *value,
  Bits bit_size
) {
  return (Storage){
    .tag = Storage_Tag_Static,
    .bit_size = bit_size,
    .Static.pointer = value,
  };
}

#define storage_static(_VALUE_)\
  storage_static_heap((_VALUE_), (Bits){sizeof(*(_VALUE_)) * CHAR_BIT})

static inline Storage
storage_immediate_with_bit_size(
  const void *source,
  Bits bit_size
) {
  Storage result = {
    .tag = Storage_Tag_Immediate,
    .bit_size = bit_size,
  };
  assert(bit_size.as_u64 <= sizeof(result.Immediate.bits) * CHAR_BIT);
  result.Immediate.bits = 0;
  assert(source);
  memcpy(&result.Immediate.bits, source, bit_size.as_u64 / CHAR_BIT);
  return result;
}

#define storage_immediate(_VALUE_)\
  storage_immediate_with_bit_size((_VALUE_), (Bits){sizeof(*(_VALUE_)) * CHAR_BIT})

#define DEFINE_IMM_X(_BIT_SIZE_)\
  static inline Storage\
  imm##_BIT_SIZE_(\
    u##_BIT_SIZE_ value\
  ) {\
    return storage_immediate_with_bit_size(&value, (Bits){_BIT_SIZE_});\
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
storage_stack(
  s32 offset,
  Bits bit_size,
  Stack_Area area
) {
  assert(bit_size.as_u64);
  return (Storage) {
    .tag = Storage_Tag_Memory,
    .bit_size = bit_size,
    .Memory.location = {
      .tag = Memory_Location_Tag_Stack,
      .Stack = {
        .area = area,
        .offset = offset
      },
    }
  };
}

static inline Storage
storage_with_offset_and_bit_size(
  const Storage *base,
  s32 diff,
  Bits bit_size
) {
  Storage result = *base;
  // Do not inherit flags as it causes issue when a struct or an array
  // is iterated over. In this case storage might be released multiple times.
  result.flags = 0;
  result.bit_size = bit_size;
  u64 byte_size = bit_size.as_u64 / 8;
  switch(base->tag) {
    default:
    case Storage_Tag_Eflags:
    case Storage_Tag_Xmm:
    case Storage_Tag_None: {
      panic("Internal Error: Unexpected storage type for structs");
    } break;
    case Storage_Tag_Immediate: {
      u64 offset_in_bits = s32_to_u64(diff) * 8;
      assert(offset_in_bits + bit_size.as_u64 <= base->bit_size.as_u64);
      result.Immediate.bits >>= offset_in_bits;
      return result;
    } break;
    case Storage_Tag_Register: {
      result.Register.packed = byte_size != 8;
      result.Register.offset_in_bits = s32_to_u16(diff * 8);
    } break;
    case Storage_Tag_Unpacked: {
      // TODO Consider making this generic and providing to users
      //      (instead of it being a side effect of System V ABI)
      if (diff < 0) panic("Can not index before an unpacked register");
      if (diff + byte_size > 16) panic("Out of bounds access on an unpacked struct");
      // This is the case for something like `struct { struct { u64 x; u64 y; } nested; } root`
      // where `root` is unpacked but the only field inside is `nested` which we also unpack.
      if (diff == 0 && byte_size == 16) {
        return result;
      }
      // Otherwise we expect requested slice to not cross an eight-byte boundary
      if (diff % 8 + byte_size > 8) panic("Unaligned unpacked struct field access");
      s32 start_index = diff / 8;
      Register reg = base->Unpacked.registers[start_index];
      result = (Storage){
        .tag = Storage_Tag_Register,
        .bit_size = bit_size,
        .Register = {
          .index = reg,
          .packed = byte_size != 8,
          .offset_in_bits = s32_to_u16((diff % 8) * 8),
        },
      };
    } break;
    case Storage_Tag_Static: {
      const s8 *pointer = get_static_storage_with_bit_size(base, base->bit_size);
      result = storage_static_heap(pointer + diff, bit_size);
    } break;
    case Storage_Tag_Memory: {
      switch(result.Memory.location.tag) {
        case Memory_Location_Tag_Instruction_Pointer_Relative: {
          result.Memory.location.Instruction_Pointer_Relative.offset += diff;
        } break;
        case Memory_Location_Tag_Indirect: {
          result.Memory.location.Indirect.offset += diff;
        } break;
        case Memory_Location_Tag_Stack: {
          result.Memory.location.Stack.offset += diff;
        } break;
      }
    } break;
  }
  return result;
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

static bool
storage_static_equal_internal(
  const Descriptor *a_descriptor,
  const void *a_memory,
  const Descriptor *b_descriptor,
  const void *b_memory
) {
  if (!same_type(a_descriptor, b_descriptor)) return false;
  u64 byte_size = descriptor_byte_size(a_descriptor);
  // TODO @Speed This should be a simple memcmp, but right now there is no good story
  //       for padding bytes in Mass, so safer to recurse.
  //return memcmp(a_memory, b_memory, byte_size) == 0;
  switch(a_descriptor->tag) {
    case Descriptor_Tag_Void: {
      return true;
    } break;
    // Opaques, references and pointers can be compared with memcmp
    case Descriptor_Tag_Float:
    case Descriptor_Tag_Integer:
    case Descriptor_Tag_Raw:
    case Descriptor_Tag_Pointer_To: {
      return memcmp(a_memory, b_memory, byte_size) == 0;
    } break;
    case Descriptor_Tag_Fixed_Size_Array: {
      // compare item by item
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
    } break;
    case Descriptor_Tag_Struct: {
      // compare field by field
      u64 a_field_count = dyn_array_length(a_descriptor->Struct.fields);
      u64 b_field_count = dyn_array_length(b_descriptor->Struct.fields);
      if (a_field_count != b_field_count) {
        return false;
      }
      for (u64 i = 0; i < a_field_count; ++i) {
        const Struct_Field *a_field = dyn_array_get(a_descriptor->Struct.fields, i);
        const Struct_Field *b_field = dyn_array_get(b_descriptor->Struct.fields, i);
        if (!storage_static_equal_internal(
          a_field->descriptor, (s8 *)a_memory + a_field->offset,
          b_field->descriptor, (s8 *)b_memory + b_field->offset
        )) {
          return false;
        }
      }
    } break;
    case Descriptor_Tag_Function_Instance: {
      panic("Unexpected static storage function");
    } break;
  }
  return true;
}

static inline const void *
storage_static_memory_with_bit_size(
  const Storage *storage,
  Bits bit_size
) {
  assert(storage->bit_size.as_u64 == bit_size.as_u64);
  if (storage->tag == Storage_Tag_Static) {
    return get_static_storage_with_bit_size(storage, bit_size);
  } else if (storage->tag == Storage_Tag_Immediate) {
    return &storage->Immediate.bits;
  } else {
    panic("Unexpected static storage tag");
  }
  return 0;
}

static inline const void *
storage_static_memory(
  const Storage *storage
) {
  return storage_static_memory_with_bit_size(storage, storage->bit_size);
}

static bool
storage_static_equal(
  const Descriptor *a_descriptor,
  const Storage *a_storage,
  const Descriptor *b_descriptor,
  const Storage *b_storage
) {
  if (!same_type(a_descriptor, b_descriptor)) return false;
  assert(a_storage->bit_size.as_u64 == b_storage->bit_size.as_u64);
  const void *a_memory = storage_static_memory_with_bit_size(a_storage, a_descriptor->bit_size);
  const void *b_memory = storage_static_memory_with_bit_size(b_storage, b_descriptor->bit_size);
  return storage_static_equal_internal(a_descriptor, a_memory, b_descriptor, b_memory);
}

static bool
storage_static_equal_values(
  const Value *a,
  const Value *b
) {
  return storage_static_equal(a->descriptor, &a->storage, b->descriptor, &b->storage);
}

static inline bool
storage_equal(
  const Storage *a,
  const Storage *b
) {
  if (a->tag != b->tag) return false;
  if (a->bit_size.as_u64 != b->bit_size.as_u64) return false;
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
          // TODO Should this do a compare based on offset?
          return a_location->Instruction_Pointer_Relative.label
            == b_location->Instruction_Pointer_Relative.label;
        }
        case Memory_Location_Tag_Indirect: {
          return (
            a_location->Indirect.base_register == b_location->Indirect.base_register &&
            a_location->Indirect.offset == b_location->Indirect.offset
          );
        }
        case Memory_Location_Tag_Stack: {
          return (
            a_location->Stack.area == b_location->Stack.area &&
            a_location->Stack.offset == b_location->Stack.offset
          );
        }
      }
      panic("Internal Error: Unexpected Memory_Location_Tag");
      return false;
    }
    case Storage_Tag_Unpacked: {
      return memcmp(a->Unpacked.registers, b->Unpacked.registers, sizeof(a->Unpacked.registers)) == 0;
    }
    case Storage_Tag_Immediate: {
      return memcmp(&a->Immediate.bits, &b->Immediate.bits, a->bit_size.as_u64 / 8) == 0;
    }
    case Storage_Tag_Xmm: {
      return a->Xmm.index == b->Xmm.index;
    }
    case Storage_Tag_Register: {
      return (
        a->Register.index == b->Register.index &&
        a->Register.offset_in_bits == b->Register.offset_in_bits
      );
    }
    case Storage_Tag_None: {
      return true;
    }
  }
  panic("Unknown operand type");
  return false;
}

static inline Value *
value_i64(
  Mass_Context *context,
  Slice digits,
  Number_Base base,
  Source_Range source_range
) {
  // @Volatile :TokenizerNumbers This code just assumes valid tokens
  static const u8 digit_decoder[128] = {
    ['0'] = 0, ['1'] = 1, ['2'] = 2, ['3'] = 3, ['4'] = 4,
    ['5'] = 5, ['6'] = 6, ['7'] = 7, ['8'] = 8, ['9'] = 9,
    ['a'] = 10, ['b'] = 11, ['c'] = 12, ['d'] = 13, ['e'] = 14, ['f'] = 15,
    ['A'] = 10, ['B'] = 11, ['C'] = 12, ['D'] = 13, ['E'] = 14, ['F'] = 15,
  };

  i64 literal;
  if (digits.length == 1) {
    char byte = digits.bytes[0];
    literal = (i64){ digit_decoder[byte] };
  } else {
    u64 bits = 0;
    for (const char *ch = digits.bytes; ch < digits.bytes + digits.length; ++ch) {
      assert((u8)*ch < countof(digit_decoder));
      if (*ch == '_') continue;
      bits *= base;
      bits += digit_decoder[*ch];
    }
    literal = (i64){ bits };
  }
  return value_make(context, &descriptor_i64, storage_immediate(&literal), source_range);
}

static inline Label *
allocate_section_memory(
  const Allocator *allocator,
  Program *program,
  Section *section,
  u64 byte_size,
  u64 alignment
) {
  virtual_memory_buffer_allocate_bytes(&section->buffer, byte_size, alignment);
  u64 offset_in_data_section = section->buffer.occupied - byte_size;

  Label *label = make_label(allocator, program, section, slice_literal("global"));
  label->offset_in_section = u64_to_u32(offset_in_data_section);
  label->resolved = true;

  return label;
}

static inline Storage
storage_indirect(
  Bits bit_size,
  Register reg
) {
  return (Storage){
    .tag = Storage_Tag_Memory,
    .bit_size = bit_size,
    .Memory.location = {
      .tag = Memory_Location_Tag_Indirect,
      .Indirect = {
        .base_register = reg
      },
    },
  };
}

static inline Storage
storage_eflags(
  Compare_Type compare_type
) {
  return (Storage){
    .tag = Storage_Tag_Eflags,
    .bit_size = {8},
    .Eflags = { .compare_type = compare_type }
  };
}

static inline Storage
storage_register(
  Register reg,
  Bits bit_size
) {
  assert(
    bit_size.as_u64 == 8 ||
    bit_size.as_u64 == 16 ||
    bit_size.as_u64 == 32 ||
    bit_size.as_u64 == 64
  );

  Storage result = {
    .tag = register_is_xmm(reg) ? Storage_Tag_Xmm : Storage_Tag_Register,
    .Register.index = reg,
    .bit_size = bit_size,
  };
  return result;
}

static inline Storage
storage_register_temp(
  Function_Builder *builder,
  Bits bit_size
) {
  Register reg = register_acquire_temp(builder);
  Storage storage = storage_register(reg, bit_size);
  storage.flags |= Storage_Flags_Temporary;
  return storage;
}

static inline void
storage_release_if_temporary(
  Function_Builder *builder,
  const Storage *storage
) {
  if (!(storage->flags & Storage_Flags_Temporary)) return;
  switch (storage->tag) {
    case Storage_Tag_Register: {
      register_release(builder, storage->Register.index);
      break;
    }
    case Storage_Tag_Xmm: {
      register_release(builder, storage->Xmm.index);
      break;
    }
    case Storage_Tag_Memory: {
      const Memory_Location *location = &storage->Memory.location;
      switch(location->tag) {
        case Memory_Location_Tag_Indirect: {
          Register reg = location->Indirect.base_register;
          if (reg == Register_SP) {
            panic("Unexpected temporary indirect memory location based on the stack pointer");
          }
          register_release(builder, reg);
          // TODO what about index register
          break;
        }
        case Memory_Location_Tag_Instruction_Pointer_Relative: {
          panic("Unexpected temporary instruction-relative storage");
          break;
        }
        case Memory_Location_Tag_Stack: {
          // TODO reuse stack memory
          break;
        }
      }
      break;
    }
    case Storage_Tag_Unpacked: {
      register_release(builder, storage->Unpacked.registers[0]);
      register_release(builder, storage->Unpacked.registers[1]);
      break;
    }
    case Storage_Tag_None:
    case Storage_Tag_Eflags:
    case Storage_Tag_Immediate:
    case Storage_Tag_Static: {
      panic("Unexpected temporary storage tag");
      break;
    }
  }
}

static inline void *
rip_value_pointer_from_label(
  const Label *label
) {
  assert(label->resolved);
  return (s8 *)label->section->buffer.memory + label->offset_in_section;
}


static inline void *
rip_value_pointer_from_storage(
  const Storage *storage
) {
  assert(storage_is_label(storage));
  const Label *label = storage->Memory.location.Instruction_Pointer_Relative.label;
  assert(label->resolved);
  u8 *base = rip_value_pointer_from_label(label);
  return base + storage->Memory.location.Instruction_Pointer_Relative.offset;
}

static inline Descriptor *
descriptor_array_of(
  const Allocator *allocator,
  const Descriptor *item_descriptor,
  u64 length
) {
  Descriptor *result = allocator_allocate(allocator, Descriptor);
  u64 item_bit_alignment = item_descriptor->bit_alignment.as_u64;
  u64 aligned_item_size = u64_align(item_descriptor->bit_size.as_u64, item_bit_alignment);
  *result = (Descriptor) {
    .tag = Descriptor_Tag_Fixed_Size_Array,
    .bit_size = {aligned_item_size * length},
    .bit_alignment = {item_bit_alignment},
    .Fixed_Size_Array = {
      .item = item_descriptor,
      .length = length,
    },
  };
  return result;
}

static inline Function_Return
function_return_exact(
  const Descriptor *descriptor,
  Source_Range source_range
) {
  assert(descriptor);
  return (Function_Return) {
    .tag = Function_Return_Tag_Exact,
    .source_range = source_range,
    .Exact = { .descriptor = descriptor },
  };
}

static inline Function_Return
function_return_generic(
  Value_View type_expression,
  Source_Range source_range
) {
  return (Function_Return) {
    .tag = Function_Return_Tag_Generic,
    .source_range = source_range,
    .Generic = { .type_expression = type_expression },
  };
}

static inline Function_Return
function_return_inferred(
  Source_Range source_range
) {
  return (Function_Return) {
    .tag = Function_Return_Tag_Inferred,
    .source_range = source_range,
  };
}

static inline void
function_info_init(
  Function_Info *info,
  Function_Return returns
) {
  *info = (Function_Info) {
    .parameters = (Array_Function_Parameter){&dyn_array_zero_items},
    .returns = returns,
  };
}

static inline Descriptor *
descriptor_function_instance(
  const Allocator *allocator,
  Slice name,
  const Function_Info *info,
  Function_Call_Setup call_setup
) {
  Descriptor *result = allocator_allocate(allocator, Descriptor);
  *result = (Descriptor) {
    .tag = Descriptor_Tag_Function_Instance,
    .name = name,
    .bit_size = {sizeof(void *) * CHAR_BIT},
    .bit_alignment = sizeof(void *) * CHAR_BIT,
    .Function_Instance = { .info = info, .call_setup = call_setup, },
  };
  return result;
}

static inline const Descriptor *
descriptor_pointer_to(
  Compilation *compilation,
  const Descriptor *descriptor
) {
  const Descriptor **maybe_cached_descriptor_pointer =
    hash_map_get(compilation->descriptor_pointer_to_cache_map, descriptor);
  if (maybe_cached_descriptor_pointer) {
    return *maybe_cached_descriptor_pointer;
  }
  Descriptor *result = allocator_allocate(compilation->allocator, Descriptor);
  *result = (const Descriptor) {
    .tag = Descriptor_Tag_Pointer_To,
    .bit_size = {sizeof(void *) * CHAR_BIT},
    .bit_alignment = sizeof(void *) * CHAR_BIT,
    .Pointer_To = {
      .is_implicit = false,
      .descriptor = descriptor,
    }
  };
  hash_map_set(compilation->descriptor_pointer_to_cache_map, descriptor, result);
  return result;
}

static Array_Value_Ptr
mass_fake_argument_array_from_parameters(
  const Allocator *allocator,
  Array_Function_Parameter parameters
) {
  u64 capacity = dyn_array_length(parameters);
  if (capacity == 0) {
    return (Array_Value_Ptr){&dyn_array_zero_items};
  }
  Array_Value_Ptr fake_args = dyn_array_make(Array_Value_Ptr, .allocator = allocator, .capacity = capacity);
  DYN_ARRAY_FOREACH(Function_Parameter, param, parameters) {
    assert(param->tag == Function_Parameter_Tag_Runtime);
    assert(param->descriptor);
    Value *fake_value = value_init(
      allocator_allocate(allocator, Value),
      param->descriptor, storage_none, param->source_range
    );
    dyn_array_push(fake_args, fake_value);
  }
  return fake_args;
}

static const Descriptor *
mass_deduce_function_return_type(
  Mass_Context *context,
  const Function_Literal *literal,
  Array_Function_Parameter parameters
) {
  Scope *body_scope = scope_make(context->allocator, literal->own_scope);
  Parser body_parser = {
    .flags = Parser_Flags_None,
    .scope = body_scope,
    .epoch = get_new_epoch(),
    .module = 0, // FIXME provide module here
  };

  // FIXME explain!!!!
  for (u64 i = 0; i < dyn_array_length(parameters); ++i) {
    const Function_Parameter *def_param = dyn_array_get(parameters, i);
    assert(def_param->descriptor);
    assert(def_param->symbol);
    Value *arg_value;
    const Descriptor *descriptor = def_param->descriptor;
    const Source_Range *source_range = &def_param->source_range;
    switch(def_param->tag) {
      case Function_Parameter_Tag_Generic:
      case Function_Parameter_Tag_Runtime: {
        // FIXME explain 0
        arg_value = mass_make_lazy_value(context, &body_parser, *source_range, 0, descriptor, 0);
      } break;
      case Function_Parameter_Tag_Exact_Static: {
        Storage storage = def_param->Exact_Static.storage;
        arg_value = value_make(context, descriptor, storage, *source_range);
      } break;
      default: {
        arg_value = 0;
        panic("UNREACHEABLE");
      } break;
    }
    arg_value->flags |= Value_Flags_Constant;
    scope_define_value(body_scope, body_parser.epoch, *source_range, def_param->symbol, arg_value);
  }
  assert(value_is_ast_block(literal->body));
  const Ast_Block *block = value_as_ast_block(literal->body);
  Value *lazy_value = token_parse_block(context, &body_parser, block, &literal->body->source_range);
  if (mass_has_error(context)) return 0;
  return value_or_lazy_value_descriptor(lazy_value);
}

static const Function_Info *
function_literal_info_for_args(
  Mass_Context *context,
  const Function_Literal *literal,
  Value_View args
) {
  if (!(literal->flags & Function_Literal_Flags_Generic)) {
    if (dyn_array_is_initialized(literal->specializations)) {
      assert(dyn_array_length(literal->specializations) == 1);
      return dyn_array_get(literal->specializations, 0)->info;
    }
    // fall through, let the specialization happen
  }

  // FIXME @ConstCast avoid this
  Function_Literal *mutable_literal = (Function_Literal *)literal;

  if (!dyn_array_is_initialized(mutable_literal->specializations)) {
    mutable_literal->specializations = dyn_array_make(
      Array_Function_Specialization,
      .capacity = 4,
      .allocator = context->allocator,
    );
  }

  DYN_ARRAY_FOREACH(Function_Specialization, specialization, mutable_literal->specializations) {
    if (dyn_array_length(specialization->descriptors) != args.length) continue;
    for (u64 i = 0; i < dyn_array_length(specialization->descriptors); ++i) {
      const Descriptor *cached_descriptor = *dyn_array_get(specialization->descriptors, i);
      const Descriptor *actual_descriptor = value_or_lazy_value_descriptor(value_view_get(&args, i));
      if (!same_type(cached_descriptor, actual_descriptor)) goto not_matched;
    }
    return specialization->info;
    not_matched:;
  }

  Array_Function_Parameter specialized_params = dyn_array_make(Array_Function_Parameter,
    .allocator = context->allocator,
    .capacity = dyn_array_length(literal->parameters),
  );

  Array_Const_Descriptor_Ptr cache_descriptors = dyn_array_make(
    Array_Const_Descriptor_Ptr,
    .capacity = args.length,
    .allocator = context->allocator,
  );

  for (u64 arg_index = 0; arg_index < dyn_array_length(literal->parameters); ++arg_index) {
    const Function_Parameter *param = dyn_array_get(literal->parameters, arg_index);
    Function_Parameter *specialized_param = dyn_array_push(specialized_params, *param);
    if (arg_index >= args.length) {
      if (!specialized_param->maybe_default_value) {
        panic("Calls to fns that don't have defaults to fill in missing args must be handled earlier");
      }
      assert(specialized_param->descriptor);
      continue;
    }
    Value *arg = value_view_get(&args, arg_index);
    const Descriptor *actual_descriptor = value_or_lazy_value_descriptor(arg);
    if(param->tag == Function_Parameter_Tag_Generic) {
      if (!(literal->flags & Function_Literal_Flags_Compile_Time)) {
        actual_descriptor = deduce_runtime_descriptor_for_value(context, arg, 0);
        // TODO cleanup memory?
        if (!actual_descriptor) return 0;
      }
      if (param->maybe_type_constraint) {
        actual_descriptor = param->maybe_type_constraint(actual_descriptor);
        if (!actual_descriptor) {
          // TODO cleanup memory?
          return 0;
        }
      }
      specialized_param->descriptor = actual_descriptor;
    }
    // In the presence of implicit casts it is unclear if this should use declared types or not.
    // On one hand with actual types we might generate identical copies, on the other hand
    // searching for a match becomes faster. Do not know what is better.
    dyn_array_push(cache_descriptors, actual_descriptor);
  }

  // FIXME this should be returned from `ensure_parameter_descriptors`
  Function_Info *specialized_info = allocator_allocate(context->allocator, Function_Info);
  function_info_init(specialized_info, literal->returns);
  if (literal->flags & Function_Literal_Flags_Intrinsic) {
    specialized_info->flags |= Function_Info_Flags_Intrinsic;
  }
  if (literal->flags & Function_Literal_Flags_Compile_Time) {
    specialized_info->flags |= Function_Info_Flags_Compile_Time;
  }
  specialized_info->parameters = specialized_params;

  ensure_parameter_descriptors(
    context, literal->own_scope, &specialized_info->parameters, &specialized_info->returns
  );

  if (specialized_info->returns.tag == Function_Return_Tag_Inferred) {
    if (!(literal->flags & Function_Literal_Flags_Intrinsic)) {
      // :OverloadLock :RecursiveInferredType
      // TODO This overload lock correctly catches recursive fns with inferred type,
      //      but the resulting error message is about an unmatched overload which is confusing.
      //      Perhaps a better option would be to propagate a reason for an overload.
      *literal->overload_lock_count += 1;
      const Descriptor *return_descriptor =
        mass_deduce_function_return_type(context, literal, specialized_params);
      if (mass_has_error(context)) return 0;
      specialized_info->returns = function_return_exact(return_descriptor, literal->returns.source_range);
      *literal->overload_lock_count -= 1;
    }
  }

  dyn_array_push(mutable_literal->specializations, (Function_Specialization) {
    .descriptors = cache_descriptors,
    .info = specialized_info,
  });

  if (mass_has_error(context)) return 0;

  return specialized_info;
}

static inline fn_type_opaque
value_as_function(
  Program *program,
  Value *value
) {
  assert(value->descriptor->tag == Descriptor_Tag_Function_Instance);
  if(mass_value_is_compile_time_known(value)) {
    void const * const *address_pointer = storage_static_memory_with_bit_size(&value->storage, (Bits){64});
    return (fn_type_opaque)*address_pointer;
  } else if (storage_is_label(&value->storage)) {
    Label *label = value->storage.Memory.location.Instruction_Pointer_Relative.label;
    assert(label->program == program);
    return (fn_type_opaque)rip_value_pointer_from_label(label);
  } else {
    panic("Could not find resolve runtime function for value");
    return 0;
  }
}

static const Os
host_os() {
  #if defined(_WIN32) && (defined(_M_AMD64) || defined(__x86_64__))
  return Os_Windows;
  #elif (defined(__MACH__) && defined(__x86_64__))
  return Os_Mac;
  #elif (defined(__linux__) && defined(__x86_64__))
  return Os_Linux;
  #else
  static_assert(false, "TODO add Os enum variant for this host system");
  #endif
}

static inline const Symbol *
mass_ensure_symbol_for_map(
  const Allocator *allocator,
  Symbol_Map *map,
  Slice name
) {
  u64 hash = Symbol_Map__hash(name);
  const Symbol *symbol = 0;
  if (!symbol) {
    Symbol **cache_entry = hash_map_get_by_hash(map, hash, name);
    if (cache_entry) {
      symbol = *cache_entry;
    }
  }
  if (!symbol) {
    Symbol *heap_symbol = allocator_allocate(allocator, Symbol);
    *heap_symbol = (Symbol){
      .name = name,
    };
    symbol = heap_symbol;
    hash_map_set_by_hash(map, hash, name, heap_symbol);
  }
  return symbol;
}

static inline const Symbol *
mass_ensure_symbol(
  Compilation *compilation,
  Slice name
) {
  return mass_ensure_symbol_for_map(compilation->allocator, compilation->symbol_cache_map, name);
}

static void
jit_init(
  Jit *jit,
  Program *program
) {
  *jit = (Jit) {
    .import_library_handles = hash_map_make(Jit_Import_Library_Handle_Map),
    .program = program,
  };
}

static void
jit_deinit(
  Jit *jit
) {
  if (jit->program) program_deinit(jit->program);
  if (jit->import_library_handles) hash_map_destroy(jit->import_library_handles);
}

static void
compilation_init(
  Compilation *compilation,
  Os target_os
) {
  *compilation = (Compilation) {
    .module_map = hash_map_make(Imported_Module_Map),
    .static_pointer_length_map = hash_map_make(Static_Pointer_Length_Map),
    .symbol_cache_map = hash_map_make(Symbol_Map, .initial_capacity = 256),
    .trampoline_map = hash_map_make(Trampoline_Map, .initial_capacity = 256),
    .prefix_operator_symbol_map = hash_map_make(Operator_Symbol_Map, .initial_capacity = 128),
    .infix_or_suffix_operator_symbol_map = hash_map_make(Operator_Symbol_Map, .initial_capacity = 128),
    .descriptor_pointer_to_cache_map = hash_map_make(Descriptor_Pointer_To_Cache_Map, .initial_capacity = 256),
    .jit = {0},
    .apply_operator = {
      .precedence = 20,
      .fixity = Operator_Fixity_Infix,
      .associativity = Operator_Associativity_Left,
      .tag = Operator_Tag_Alias,
      .Alias.handler = mass_handle_apply_operator,
    },
  };

  void *permanent_arena_address = 0;
  void *temp_arena_address = 0;
  #if !defined(NDEBUG)
  permanent_arena_address = (void *)0x10000000000;
  temp_arena_address = (void *)0x20000000000;
  #endif

  // Get 16 gigabytes of virtual permanent space
  virtual_memory_buffer_init_at_address(
    &compilation->allocation_buffer, 16llu * 1024 * 1024 * 1024, permanent_arena_address
  );
  compilation->allocation_buffer.commit_step_byte_size = 16 * 1024 * 1024;
  compilation->allocator = virtual_memory_buffer_allocator_make(&compilation->allocation_buffer);
  virtual_memory_buffer_enable_warmup(&compilation->allocation_buffer);

  // Get 1 gigabyte of temp space
  virtual_memory_buffer_init_at_address(
    &compilation->temp_buffer, 1llu * 1024 * 1024 * 1024, temp_arena_address
  );
  compilation->temp_buffer.commit_step_byte_size = 1024 * 1024;
  compilation->temp_allocator = virtual_memory_buffer_allocator_make(&compilation->temp_buffer);

  compilation->result = allocator_allocate(compilation->allocator, Mass_Result);

  compilation->runtime_program = allocator_allocate(compilation->allocator, Program);

  program_init(compilation->allocator, compilation->runtime_program, target_os);

  Program *jit_program = allocator_allocate(compilation->allocator, Program);
  program_init(compilation->allocator, jit_program, host_os());
  jit_init(&compilation->jit, jit_program);

  compilation->root_scope = scope_make(compilation->allocator, 0);
  module_compiler_init(compilation, &compilation->compiler_module);

  // Intern common symbols used during parsing
  compilation->common_symbols = (Common_Symbols) {
    .apply = mass_ensure_symbol(compilation, slice_literal("apply")),
    .fn = mass_ensure_symbol(compilation, slice_literal("fn")),
    .get = mass_ensure_symbol(compilation, slice_literal("get")),
    .intrinsic = mass_ensure_symbol(compilation, slice_literal("intrinsic")),
    .label = mass_ensure_symbol(compilation, slice_literal("label")),
    .macro = mass_ensure_symbol(compilation, slice_literal("macro")),
    .operator = mass_ensure_symbol(compilation, slice_literal("operator")),
    .placeholder = mass_ensure_symbol(compilation, slice_literal("placeholder")),
    .statement = mass_ensure_symbol(compilation, slice_literal("statement")),
    .syntax = mass_ensure_symbol(compilation, slice_literal("syntax")),
    .underscore = mass_ensure_symbol(compilation, slice_literal("underscore")),
    ._if = mass_ensure_symbol(compilation, slice_literal("if")),
    .then = mass_ensure_symbol(compilation, slice_literal("then")),
    ._while = mass_ensure_symbol(compilation, slice_literal("while")),
    ._else = mass_ensure_symbol(compilation, slice_literal("else")),
    ._ = mass_ensure_symbol(compilation, slice_literal("_")),
    .operator_arrow = mass_ensure_symbol(compilation, slice_literal("->")),
    .operator_at = mass_ensure_symbol(compilation, slice_literal("@")),
    .operator_colon = mass_ensure_symbol(compilation, slice_literal(":")),
    .operator_comma = mass_ensure_symbol(compilation, slice_literal(",")),
    .operator_dot = mass_ensure_symbol(compilation, slice_literal(".")),
    .operator_dot_star = mass_ensure_symbol(compilation, slice_literal(".*")),
    .operator_equal = mass_ensure_symbol(compilation, slice_literal("=")),
    .operator_fat_arrow = mass_ensure_symbol(compilation, slice_literal("=>")),
    .operator_space = mass_ensure_symbol(compilation, slice_literal(" ")),
    .operator_quote = mass_ensure_symbol(compilation, slice_literal("'")),
  };

  scope_define_builtins(compilation, compilation->root_scope);
}

static void
compilation_deinit(
  Compilation *compilation
) {
  hash_map_destroy(compilation->module_map);
  hash_map_destroy(compilation->static_pointer_length_map);
  hash_map_destroy(compilation->symbol_cache_map);
  hash_map_destroy(compilation->prefix_operator_symbol_map);
  hash_map_destroy(compilation->infix_or_suffix_operator_symbol_map);
  hash_map_destroy(compilation->trampoline_map);
  hash_map_destroy(compilation->descriptor_pointer_to_cache_map);
  program_deinit(compilation->runtime_program);
  jit_deinit(&compilation->jit);
  virtual_memory_buffer_deinit(&compilation->allocation_buffer);
  virtual_memory_buffer_deinit(&compilation->temp_buffer);
}

static inline bool
context_is_compile_time_eval(
  const Mass_Context *context
) {
  return context->compilation->jit.program == context->program;
};

Mass_Context
mass_context_from_compilation(
  Compilation *compilation
) {
  return (Mass_Context) {
    .allocator = compilation->allocator,
    .temp_allocator = compilation->temp_allocator,
    .program = compilation->runtime_program,
    .compilation = compilation,
    .result = compilation->result,
  };
}

typedef enum {
  Literal_Cast_Result_Success,
  Literal_Cast_Result_Target_Not_An_Integer,
  Literal_Cast_Result_Target_Too_Small,
  Literal_Cast_Result_Target_Too_Big,
} Literal_Cast_Result;

static Literal_Cast_Result
value_i64_cast_to(
  const Value *value,
  const Descriptor *target_descriptor,
  u64 *out_bits,
  u64 *out_bit_size
) {
  if (!descriptor_is_integer(target_descriptor)) {
    return Literal_Cast_Result_Target_Not_An_Integer;
  }

  u64 bit_size = target_descriptor->bit_size.as_u64;
  if (bit_size > 64) {
    return Literal_Cast_Result_Target_Too_Big;
  }

  u64 bits = value_as_i64(value)->bits;

  if (bit_size != 64) {
    if (descriptor_is_signed_integer(target_descriptor)) {
      u64 shifted = (s64)bits >> bit_size;
      if (shifted != 0 && ~shifted != 0) {
        return Literal_Cast_Result_Target_Too_Small;
      }
    } else {
      u64 shifted = bits >> bit_size;
      if (shifted != 0) {
        return Literal_Cast_Result_Target_Too_Small;
      }
    }
  }

  *out_bits = bits;
  *out_bit_size = bit_size;
  return Literal_Cast_Result_Success;
}

static bool
same_type_or_can_implicitly_move_cast(
  const Descriptor *target,
  const Descriptor *source
) {
  if (same_type(target, source)) return true;
  if (target->tag == Descriptor_Tag_Void) return true;
  if (target->brand && source->brand && target->brand != source->brand) return false;
  if (source->tag == Descriptor_Tag_Raw) {
    if (source->bit_size.as_u64 == target->bit_size.as_u64) {
      return true;
    }
  }

  if (target->tag != source->tag) return false;
  if (target->tag == Descriptor_Tag_Pointer_To) {
    if (
      source->Pointer_To.descriptor->tag == Descriptor_Tag_Fixed_Size_Array &&
      same_type(source->Pointer_To.descriptor->Fixed_Size_Array.item, target->Pointer_To.descriptor)
    ) {
      return true;
    }
    if (mass_descriptor_is_void(target->Pointer_To.descriptor)) {
      return true;
    }
  }
  if (target->tag == Descriptor_Tag_Struct) {
    assert(source->tag == Descriptor_Tag_Struct);
    if (dyn_array_length(source->Struct.fields) != dyn_array_length(target->Struct.fields)) {
      return false;
    }
    for (u64 i = 0; i < dyn_array_length(source->Struct.fields); ++i) {
      const Descriptor *source_field = dyn_array_get(source->Struct.fields, i)->descriptor;
      const Descriptor *target_field = dyn_array_get(target->Struct.fields, i)->descriptor;
      if (!same_type_or_can_implicitly_move_cast(target_field, source_field)) return false;
    }
    return true;
  }
  return false;
}
