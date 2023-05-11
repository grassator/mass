#include "value.h"
#include "function.h"
#include "source.h"
#include "calling_convention.h"

static inline bool
mass_value_is_static(
  const Value *value
) {
  if (!value) return false;
  if (value->tag != Value_Tag_Forced) return false;
  switch(value->Forced.storage.tag) {
    case Storage_Tag_Static:
    case Storage_Tag_Immediate: {
      return true;
    }
    case Storage_Tag_Register:
    case Storage_Tag_Xmm:
    case Storage_Tag_Disjoint:
    case Storage_Tag_Memory:
    case Storage_Tag_Eflags:
    default: {
      return false;
    }
  }
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

static void
mass_error_append_descriptor(
  Bucket_Buffer *result,
  const Descriptor *descriptor,
  u64 level
);

static void
mass_error_append_function_signature_string(
  Bucket_Buffer *result,
  const Function_Info *info
) {
  bucket_buffer_append(result, "(");
  for (u64 i = 0; i < dyn_array_length(info->parameters); ++i) {
    Resolved_Function_Parameter *arg = dyn_array_get(info->parameters, i);
    if (i != 0) bucket_buffer_append(result, ", ");
    bucket_buffer_append(result, arg->symbol->name);
    bucket_buffer_append(result, " : ");
    mass_error_append_descriptor(result, arg->descriptor, 1);
  }
  bucket_buffer_append(result, ")");
  const char *arrow = info->flags & Function_Info_Flags_Compile_Time ? " => " : " -> ";
  bucket_buffer_append(result, arrow);
  if (info->return_descriptor) {
    mass_error_append_descriptor(result, info->return_descriptor, 1);
  } else {
    bucket_buffer_append(result, "_");
  }
}

static void
mass_error_append_descriptor(
  Bucket_Buffer *result,
  const Descriptor *descriptor,
  u64 level
) {
  if (descriptor->brand) {
    bucket_buffer_append(result, descriptor->brand->name);
    return;
  }
  if (level > 3) {
    bucket_buffer_append(result, "..");
    return;
  }
  char print_buffer[32] = {0};
  switch(descriptor->tag) {
    case Descriptor_Tag_Void: {
      bucket_buffer_append(result, "()");
    } break;
    case Descriptor_Tag_Never: {
      bucket_buffer_append(result, "Never");
    } break;
    case Descriptor_Tag_Integer:
    case Descriptor_Tag_Float: {
      const char *prefix = descriptor->tag == Descriptor_Tag_Float ? "f" : descriptor->Integer.is_signed ? "s" : "u";
      bucket_buffer_append(result, prefix);
      u64 length = snprintf(print_buffer, countof(print_buffer), "%"PRIu64, descriptor->bit_size.as_u64);
      bucket_buffer_append(result, print_buffer);
    } break;
    case Descriptor_Tag_Raw: {
      bucket_buffer_append(result, "i");
      u64 length = snprintf(print_buffer, countof(print_buffer), "%"PRIu64, descriptor->bit_size.as_u64);
      bucket_buffer_append(result, print_buffer);
    } break;
    case Descriptor_Tag_Pointer_To: {
      bucket_buffer_append(result, "&");
      mass_error_append_descriptor(result, descriptor->Pointer_To.descriptor, level + 1);
    } break;
    case Descriptor_Tag_Fixed_Array: {
      mass_error_append_descriptor(result, descriptor->Fixed_Array.item, level + 1);
      u64 item_count = descriptor->Fixed_Array.length;
      u64 length = snprintf(print_buffer, countof(print_buffer), "%"PRIu64, item_count);
      bucket_buffer_append(result, "*");
      bucket_buffer_append(result, print_buffer);
    } break;
    case Descriptor_Tag_Struct: {
      bucket_buffer_append(result, "[");
      bool is_first = true;
      DYN_ARRAY_FOREACH(Struct_Field, it, descriptor->Struct.fields) {
        if (is_first) is_first = false; else bucket_buffer_append(result, ", ");
        if (it->name.length) {
          bucket_buffer_append(result, it->name);
          bucket_buffer_append(result, " : ");
        }
        mass_error_append_descriptor(result, it->descriptor, level + 1);
      }
      bucket_buffer_append(result, "]");
    } break;
    case Descriptor_Tag_Function_Instance: {
      mass_error_append_function_signature_string(result, descriptor->Function_Instance.info);
    } break;
  }
}

static Fixed_Buffer *
mass_error_to_string(
  Compilation *compilation,
  Mass_Error const* error
) {
  Bucket_Buffer *buffer = bucket_buffer_make();
  switch(error->tag) {
    case Mass_Error_Tag_Unimplemented: {
      bucket_buffer_append(buffer, "Unimplemented Feature: ");
      bucket_buffer_append(buffer, error->detailed_message);
    } break;
    case Mass_Error_Tag_Unreachable_Statement: {
      bucket_buffer_append(buffer, "Unreachable Statement");
      if (error->detailed_message.length) {
        bucket_buffer_append(buffer, ": ");
        bucket_buffer_append(buffer, error->detailed_message);
      }
    } break;
    case Mass_Error_Tag_User_Defined: {
      bucket_buffer_append(buffer, error->User_Defined.name);
      bucket_buffer_append(buffer, ": ");
      bucket_buffer_append(buffer, error->detailed_message);
    } break;
    case Mass_Error_Tag_Assignment_To_Constant: {
      bucket_buffer_append(buffer, "Trying to assign to a constant value");
    } break;
    case Mass_Error_Tag_Parse: {
      bucket_buffer_append(buffer, "Unable to parse the expression");
      if (error->detailed_message.length) {
        bucket_buffer_append(buffer, ": ");
        bucket_buffer_append(buffer, error->detailed_message);
      }
    } break;
    case Mass_Error_Tag_Tokenizer: {
      bucket_buffer_append(buffer, "Error while tokenizing");
      if (error->detailed_message.length) {
        bucket_buffer_append(buffer, ": ");
        bucket_buffer_append(buffer, error->detailed_message);
      }
    } break;
    case Mass_Error_Tag_Undefined_Variable: {
      bucket_buffer_append(buffer, "Undefined variable '");
      bucket_buffer_append(buffer, error->Undefined_Variable.name);
      bucket_buffer_append(buffer, "'");
    } break;
    case Mass_Error_Tag_Redefinition: {
      bucket_buffer_append(buffer, "Redefinition of binding ");
      bucket_buffer_append(buffer, error->Redefinition.name);
      bucket_buffer_append(buffer, "\n  previously defined here:\n  ");
      Slice other_source = source_from_source_range(compilation, &error->other_source_range);
      bucket_buffer_append(buffer, other_source);
    } break;
    case Mass_Error_Tag_Circular_Dependency: {
      bucket_buffer_append(buffer, "Circular dependency when resolving ");
      bucket_buffer_append(buffer, error->Circular_Dependency.name);
    } break;
    case Mass_Error_Tag_Unknown_Field: {
      bucket_buffer_append(buffer, "Field ");
      bucket_buffer_append(buffer, error->Unknown_Field.name);
      bucket_buffer_append(buffer, " does not exist on type ");
      mass_error_append_descriptor(buffer, error->Unknown_Field.type, 0);
    } break;
    case Mass_Error_Tag_Invalid_Identifier: {
      bucket_buffer_append(buffer, "Invalid identifier");
    } break;
    case Mass_Error_Tag_Dynamic_Library_Load: {
      bucket_buffer_append(buffer, "Unable to load a dynamic library ");
      bucket_buffer_append(buffer, error->Dynamic_Library_Load.library_name);
    } break;
    case Mass_Error_Tag_Dynamic_Library_Symbol_Not_Found: {
      bucket_buffer_append(buffer, "Unable to resolve a symbol ");
      bucket_buffer_append(buffer, error->Dynamic_Library_Symbol_Not_Found.symbol_name);
      bucket_buffer_append(buffer, " from a dynamic library ");
      bucket_buffer_append(buffer, error->Dynamic_Library_Symbol_Not_Found.library_name);
    } break;
    case Mass_Error_Tag_File_Open: {
      bucket_buffer_append(buffer, "Can not open file ");
      bucket_buffer_append(buffer, error->File_Open.path);
    } break;
    case Mass_Error_Tag_File_Too_Large: {
      bucket_buffer_append(buffer, "File ");
      bucket_buffer_append(buffer, error->File_Too_Large.path);
      bucket_buffer_append(buffer, " is larger than 4GB");
    } break;
    case Mass_Error_Tag_Expected_Static: {
      bucket_buffer_append(buffer, "Expected value to be static (compile-time known)");
    } break;
    case Mass_Error_Tag_Operator_Fixity_Conflict: {
      if (error->Operator_Fixity_Conflict.fixity == Operator_Fixity_Prefix) {
        bucket_buffer_append(buffer, "There is already a prefix operator ");
      } else {
        bucket_buffer_append(buffer, "There is already a infix or postfix operator ");
      }
      bucket_buffer_append(buffer, error->Operator_Fixity_Conflict.symbol);
      bucket_buffer_append(buffer, " defined in this scope");
    } break;
    case Mass_Error_Tag_Non_Trailing_Default_Argument: {
      bucket_buffer_append(buffer, "An argument without a default value can not come after an argument that has one");
    } break;
    case Mass_Error_Tag_Type_Mismatch: {
      Mass_Error_Type_Mismatch const *mismatch = &error->Type_Mismatch;
      if (error->detailed_message.length) {
        bucket_buffer_append(buffer, error->detailed_message);
      } else {
        bucket_buffer_append(buffer, "Type mismatch: expected ");
        mass_error_append_descriptor(buffer, mismatch->expected, 0);
        bucket_buffer_append(buffer, ", got ");
        mass_error_append_descriptor(buffer, mismatch->actual, 0);
      }
    } break;
    case Mass_Error_Tag_Integer_Range: {
      bucket_buffer_append(buffer, "Value does not fit into integer of type ");
      mass_error_append_descriptor(buffer, error->Integer_Range.descriptor, 0);
    } break;
    case Mass_Error_Tag_Epoch_Mismatch: {
      bucket_buffer_append(buffer, "Trying to access a value from the wrong execution epoch ");
      Slice source = source_from_source_range(compilation, &error->source_range);
      bucket_buffer_append(buffer, source);
      bucket_buffer_append(buffer, ".\n");
      bucket_buffer_append(buffer,
        "This happens when you access value from runtime in compile-time execution "
        "or a runtime value from a different stack frame than current function call."
      );
    } break;
    case Mass_Error_Tag_Undecidable_Overload: {
      Mass_Error_Undecidable_Overload const *overloads = &error->Undecidable_Overload;
      bucket_buffer_append(buffer, "Could not decide which overload is better: \n  ");
      for (u64 i = 0; i < dyn_array_length(overloads->matches); ++i) {
        if (i != 0) bucket_buffer_append(buffer, "\n  ");
        const Undecidable_Match *match = dyn_array_get(overloads->matches, i);
        mass_error_append_function_signature_string(buffer, match->info);
      }
    } break;
    case Mass_Error_Tag_Non_Function_Overload: {
      bucket_buffer_append(buffer, "Trying to define a non-function overload ");
      Slice source = source_from_source_range(compilation, &error->source_range);
      bucket_buffer_append(buffer, source);
    } break;
    case Mass_Error_Tag_No_Matching_Overload: {
      Mass_Error_No_Matching_Overload const *no_match = &error->No_Matching_Overload;
      bucket_buffer_append(buffer, "Could not find matching overload for call.\n  Arguments: ");
      for (u64 i = 0; i < dyn_array_length(no_match->arguments); ++i) {
        if (i != 0) bucket_buffer_append(buffer, ", ");
        const Resolved_Function_Parameter *arg = dyn_array_get(no_match->arguments, i);
        mass_error_append_descriptor(buffer, arg->descriptor, 0);
      }
      bucket_buffer_append(buffer, "\n  Source code: ");
      Slice source = source_from_source_range(compilation, &error->source_range);
      bucket_buffer_append(buffer, source);
    } break;
    case Mass_Error_Tag_No_Runtime_Use: {
      if (error->detailed_message.length) {
        bucket_buffer_append(buffer, error->detailed_message);
      } else {
        bucket_buffer_append(buffer, "Value can't be used at runtime");
      }
      bucket_buffer_append(buffer, ":\n");
      Slice source = source_from_source_range(compilation, &error->source_range);
      bucket_buffer_append(buffer, source);
    } break;
    case Mass_Error_Tag_Recursive_Intrinsic_Use: {
      bucket_buffer_append(buffer, "Recursive calls to intrinsics are not allowed.\n");
      bucket_buffer_append(buffer, "Since an intrinsic is called during compilation of a fn body\n");
      bucket_buffer_append(buffer, "if it contains a direct or indirect call to itself\n");
      bucket_buffer_append(buffer, "as the body is not read - we can not execute it.");
    } break;
  }
  Fixed_Buffer *result = bucket_buffer_to_fixed_buffer(allocator_system, buffer);
  bucket_buffer_destroy(buffer);
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
  if (!same_type(a_info->return_descriptor, b_info->return_descriptor)) {
    return false;
  }

  if (dyn_array_length(a_info->parameters) != dyn_array_length(b_info->parameters)) {
    return false;
  }
  for (u64 i = 0; i < dyn_array_length(a_info->parameters); ++i) {
    Resolved_Function_Parameter *a_arg = dyn_array_get(a_info->parameters, i);
    Resolved_Function_Parameter *b_arg = dyn_array_get(b_info->parameters, i);
    if(!same_type(a_arg->descriptor, b_arg->descriptor)) return false;
  }
  return true;
}

typedef enum {
  Brand_Comparison_Mode_Strict,
  Brand_Comparison_Mode_Ignore,
  Brand_Comparison_Mode_One_Unbranded,
} Brand_Comparison_Mode;

static bool
types_equal(
  const Descriptor *a,
  const Descriptor *b,
  Brand_Comparison_Mode brand_comparison_mode
) {
  if (a->brand != b->brand) {
    switch (brand_comparison_mode) {
      case Brand_Comparison_Mode_Strict: {
        return false;
      }
      case Brand_Comparison_Mode_Ignore: {
        // nothing to do
      } break;
      case Brand_Comparison_Mode_One_Unbranded: {
        if (a->brand && b->brand) return false;
      } break;
    }
  }
  if (a == b) return true;
  if (a->tag != b->tag) return false;
  if (a->bit_size.as_u64 != b->bit_size.as_u64) return false;
  if (a->bit_alignment.as_u64 != b->bit_alignment.as_u64) return false;

  switch(a->tag) {
    case Descriptor_Tag_Void:
    case Descriptor_Tag_Never:
    case Descriptor_Tag_Float: {
      return true;
    }
    case Descriptor_Tag_Integer: {
      return a->Integer.is_signed == b->Integer.is_signed;
    }
    case Descriptor_Tag_Pointer_To: {
      return types_equal(a->Pointer_To.descriptor, b->Pointer_To.descriptor, brand_comparison_mode);
    }
    case Descriptor_Tag_Fixed_Array: {
      return types_equal(a->Fixed_Array.item, b->Fixed_Array.item, brand_comparison_mode) &&
        a->Fixed_Array.length == b->Fixed_Array.length;
    }
    case Descriptor_Tag_Struct: {
      if (dyn_array_length(a->Struct.fields) != dyn_array_length(b->Struct.fields)) {
        return false;
      }
      for (u64 i = 0; i < dyn_array_length(a->Struct.fields); ++i) {
        const Descriptor *a_field = dyn_array_get(a->Struct.fields, i)->descriptor;
        const Descriptor *b_field = dyn_array_get(b->Struct.fields, i)->descriptor;
        if (!types_equal(a_field, b_field, brand_comparison_mode)) return false;
      }
      return true;
    }
    case Descriptor_Tag_Raw: {
      return true;
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

static inline bool
same_type(
  const Descriptor *a,
  const Descriptor *b
) {
  return types_equal(a, b, Brand_Comparison_Mode_Strict);
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
  if (bit_size.as_u64) {
    assert(source);
    memcpy(&result.Immediate.bits, source, bit_size.as_u64 / CHAR_BIT);
  }
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
  s32 offset,
  Bits bit_size
) {
  if (offset < 0) panic("Negative offsets are not supported");
  Storage result = *base;
  // Do not inherit flags as it causes issue when a struct or an array
  // is iterated over. In this case storage might be released multiple times.
  result.flags = 0;
  result.bit_size = bit_size;
  u64 offset_in_bits = s32_to_u64(offset) * 8;
  if (offset_in_bits + bit_size.as_u64 > base->bit_size.as_u64) {
    panic("Out of bounds access on a storage");
  }
  bool same_size_as_base = (bit_size.as_u64 == base->bit_size.as_u64);
  if (same_size_as_base) {
    assert(offset_in_bits == 0); // Implied by check for out of bounds above
    return result;
  }
  switch(base->tag) {
    default:
    case Storage_Tag_Eflags:
    case Storage_Tag_Xmm: {
      panic("Internal Error: Unexpected storage type for structs");
    } break;
    case Storage_Tag_Immediate: {
      result.Immediate.bits >>= offset_in_bits;
      return result;
    }
    case Storage_Tag_Register: {
      result.Register.packed = bit_size.as_u64 != 64;
      result.Register.offset_in_bits = u64_to_u16(offset_in_bits);
    } break;
    case Storage_Tag_Disjoint: {
      u64 bit_start = 0;
      for (u64 i = 0; i < dyn_array_length(base->Disjoint.pieces); ++i) {
        const Storage *piece = *dyn_array_get(base->Disjoint.pieces, i);
        u64 bit_end = bit_start + piece->bit_size.as_u64;
        bool starts_in_this_piece = offset_in_bits >= bit_start && offset_in_bits < bit_end;
        if (starts_in_this_piece) {
          u64 offset_in_bits_in_this_piece = offset_in_bits - bit_start;
          if (offset_in_bits_in_this_piece + bit_size.as_u64 > piece->bit_size.as_u64) {
            panic("Requested storage crosses a disjoint pieces boundary");
          }
          s32 nested_byte_offset = u64_to_s32(offset_in_bits_in_this_piece / 8);
          return storage_with_offset_and_bit_size(piece, nested_byte_offset, bit_size);
        }
        bit_start = bit_end;
      }
      panic("Could not find specified offset in the storage");
    } break;
    case Storage_Tag_Static: {
      const s8 *pointer = get_static_storage_with_bit_size(base, base->bit_size);
      result = storage_static_heap(pointer + offset, bit_size);
    } break;
    case Storage_Tag_Memory: {
      switch(result.Memory.location.tag) {
        case Memory_Location_Tag_Instruction_Pointer_Relative: {
          result.Memory.location.Instruction_Pointer_Relative.offset += offset;
        } break;
        case Memory_Location_Tag_Indirect: {
          result.Memory.location.Indirect.offset += offset;
        } break;
        case Memory_Location_Tag_Stack: {
          result.Memory.location.Stack.offset += offset;
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
    case Descriptor_Tag_Never:
    case Descriptor_Tag_Void: {
      return true;
    }
    // Opaques, references and pointers can be compared with memcmp
    case Descriptor_Tag_Float:
    case Descriptor_Tag_Integer:
    case Descriptor_Tag_Raw:
    case Descriptor_Tag_Pointer_To: {
      return memcmp(a_memory, b_memory, byte_size) == 0;
    }
    case Descriptor_Tag_Fixed_Array: {
      // compare item by item
      if (a_descriptor->Fixed_Array.length != b_descriptor->Fixed_Array.length) {
        return false;
      }
      for (u64 i = 0; i < a_descriptor->Fixed_Array.length; ++i) {
        const Descriptor *a_item = a_descriptor->Fixed_Array.item;
        const Descriptor *b_item = b_descriptor->Fixed_Array.item;
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
      u64 field_count = dyn_array_length(a_descriptor->Struct.fields);
      assert(field_count == dyn_array_length(b_descriptor->Struct.fields));

      for (u64 i = 0; i < field_count; ++i) {
        const Struct_Field *field = dyn_array_get(a_descriptor->Struct.fields, i);
        assert(same_type(field->descriptor, dyn_array_get(b_descriptor->Struct.fields, i)->descriptor));
        if (!storage_static_equal_internal(
          field->descriptor, (s8 *)a_memory + field->offset,
          field->descriptor, (s8 *)b_memory + field->offset
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
    case Storage_Tag_Disjoint: {
      if (dyn_array_length(a->Disjoint.pieces) != dyn_array_length(b->Disjoint.pieces)) return false;
      for (u64 i = 0; i < dyn_array_length(a->Disjoint.pieces); ++i) {
        const Storage *a_piece = *dyn_array_get(a->Disjoint.pieces, i);
        const Storage *b_piece = *dyn_array_get(b->Disjoint.pieces, i);
        if (!storage_equal(a_piece, b_piece)) return false;
      }
      return true;
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
  }
  panic("Unknown operand type");
  return false;
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
    case Storage_Tag_Disjoint: {
      for (u64 i = 0; i < dyn_array_length(storage->Disjoint.pieces); ++i) {
        Storage piece = **dyn_array_get(storage->Disjoint.pieces, i);
        // TODO This is a quite awkward.
        //      Maybe the flags should be propagated on storage creation?
        //      Also is it possible that some pieces are temp and some are not?
        piece.flags |= storage->flags & Storage_Flags_Temporary;
        storage_release_if_temporary(builder, &piece);
      }
      break;
    }
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
    .tag = Descriptor_Tag_Fixed_Array,
    .bit_size = {aligned_item_size * length},
    .bit_alignment = {item_bit_alignment},
    .Fixed_Array = {
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
  const Descriptor* return_descriptor
) {
  *info = (Function_Info) {
    .parameters = (Array_Resolved_Function_Parameter){&dyn_array_zero_items},
    .return_descriptor = return_descriptor,
  };
}

static inline Descriptor *
descriptor_function_instance(
  const Allocator *allocator,
  const Function_Info *info,
  Function_Call_Setup call_setup,
  const Program *program
) {
  Descriptor *result = allocator_allocate(allocator, Descriptor);
  *result = (Descriptor) {
    .tag = Descriptor_Tag_Function_Instance,
    .bit_size = {sizeof(void *) * CHAR_BIT},
    .bit_alignment = sizeof(void *) * CHAR_BIT,
    .Function_Instance = { .info = info, .call_setup = call_setup, .program = program, },
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
      .descriptor = descriptor,
    }
  };
  hash_map_set(compilation->descriptor_pointer_to_cache_map, descriptor, result);
  return result;
}

static Function_Info *
function_literal_info_for_parameters(
  Mass_Context *context,
  const Function_Literal *literal,
  Array_Resolved_Function_Parameter source_parameters
) {
  if (literal->header.generic_parameter_count == 0) {
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

  Array_Function_Parameter temp_specialized_parameters = dyn_array_make(
    Array_Function_Parameter,
    .allocator = context->temp_allocator,
    .capacity = dyn_array_length(literal->header.parameters),
  );

  for (u64 arg_index = 0; arg_index < dyn_array_length(literal->header.parameters); ++arg_index) {
    const Function_Parameter *param = dyn_array_get(literal->header.parameters, arg_index);
    Function_Parameter *specialized_param = dyn_array_push(temp_specialized_parameters, *param);

    if (arg_index >= dyn_array_length(source_parameters)) continue; // No specialization for default args
    if (param->tag != Function_Parameter_Tag_Generic) continue;

    const Resolved_Function_Parameter *source_param = dyn_array_get(source_parameters, arg_index);
    const Descriptor *actual_descriptor = source_param->descriptor;
    if (param->Generic.is_static) { // :StaticGenericToExactStatic
      if (source_param->tag != Resolved_Function_Parameter_Tag_Known) return 0;
      specialized_param->descriptor = actual_descriptor;
      specialized_param->tag = Function_Parameter_Tag_Exact_Static;
      specialized_param->Exact_Static.storage = source_param->Known.storage;
    } else {
      if (!(literal->header.flags & Function_Header_Flags_Compile_Time)) {
        if (source_param->tag == Resolved_Function_Parameter_Tag_Known) {
          Value fake_source_value;
          Storage storage = source_param->Known.storage;
          value_init(&fake_source_value, source_param->descriptor, storage, source_param->source_range);
          actual_descriptor = deduce_runtime_descriptor_for_value(context, &fake_source_value, 0);
          if (!actual_descriptor) return 0;
        }
      }
    }
    if (param->Generic.maybe_type_constraint) {
      if (!param->Generic.maybe_type_constraint((Type){actual_descriptor})) return 0;
    }
    specialized_param->descriptor = actual_descriptor;
  }

  DYN_ARRAY_FOREACH(Function_Specialization, specialization, literal->specializations) {
    if (dyn_array_length(specialization->parameters) != dyn_array_length(temp_specialized_parameters)) continue;
    for (u64 i = 0; i < dyn_array_length(specialization->parameters); ++i) {
      const Function_Parameter *cached = dyn_array_get(specialization->parameters, i);
      const Function_Parameter *current = dyn_array_get(temp_specialized_parameters, i);
      if (cached->tag != current->tag) {
        panic("Specialized parameter tag should not depend on source parameters");
      }
      switch(cached->tag) {
        case Function_Parameter_Tag_Runtime: {
          // Can be null but must be the same since we don't modify `Runtime` params above
          assert(cached->descriptor == current->descriptor);
        } break;
        case Function_Parameter_Tag_Generic: {
          if (!same_type(cached->descriptor, current->descriptor)) goto not_matched;
        } break;
        case Function_Parameter_Tag_Exact_Static: {
          const Function_Parameter *unspecialized = dyn_array_get(literal->header.parameters, i);
          // :StaticGenericToExactStatic
          // Only need to compare exact static args if they were created above, as hardcoded
          // `Exact_Static` args aren't supposed to change anyway
          if (unspecialized->tag == Function_Parameter_Tag_Generic) {
            assert(unspecialized->Generic.is_static);
            if (cached->descriptor != current->descriptor) goto not_matched;
            if (!storage_static_equal(
              cached->descriptor, &cached->Exact_Static.storage,
              current->descriptor, &current->Exact_Static.storage
            )) {
              goto not_matched;
            }
          } else {
            assert(cached->descriptor == current->descriptor);
          }
        } break;
      }
    }
    return specialization->info;
    not_matched:;
  }

  // The cache didn't match so we need a permanent copy of the parameters
  // for the key of the new cache entry
  Array_Function_Parameter specialized_parameters = dyn_array_make(
    Array_Function_Parameter,
    .allocator = context->allocator,
    .capacity = dyn_array_length(temp_specialized_parameters),
  );
  DYN_ARRAY_FOREACH(Function_Parameter, param, temp_specialized_parameters) {
    dyn_array_push(specialized_parameters, *param);
  }

  Function_Header specialized_header = literal->header;
  specialized_header.parameters = specialized_parameters;

  Function_Info *specialized_info = mass_allocate(context, Function_Info);
  mass_function_info_init_for_header_and_maybe_body(
    context, literal->own_scope, &specialized_header, literal->body, specialized_info
  );
  if (specialized_header.flags & Function_Header_Flags_Intrinsic) {
    specialized_info->flags |= Function_Info_Flags_Intrinsic;
  }
  if (specialized_header.flags & Function_Header_Flags_Compile_Time) {
    specialized_info->flags |= Function_Info_Flags_Compile_Time;
  }

  dyn_array_push(mutable_literal->specializations, (Function_Specialization) {
    .parameters = specialized_parameters,
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
  assert(value->tag == Value_Tag_Forced);
  const Storage *storage = &value->Forced.storage;
  if(mass_value_is_static(value)) {
    void const * const *address_pointer = storage_static_memory_with_bit_size(storage, (Bits){64});
    return (fn_type_opaque)*address_pointer;
  } else if (storage_is_label(storage)) {
    Label *label = storage->Memory.location.Instruction_Pointer_Relative.label;
    assert(label->program == program);
    return (fn_type_opaque)rip_value_pointer_from_label(label);
  } else {
    panic("Could not find resolve runtime function for value");
    return 0;
  }
}

static Os
host_os() {
  #if defined(_WIN32) && (defined(_M_AMD64) || defined(__x86_64__))
  return Os_Windows;
  #elif (defined(__MACH__) && defined(__x86_64__))
  return Os_Mac;
  #elif (defined(__linux__) && defined(__x86_64__))
  return Os_Linux;
  #else
  static_assert(false, "Host OS not supported");
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
  Symbol **cache_entry = hash_map_get_by_hash(map, hash, name);
  if (cache_entry) {
    symbol = *cache_entry;
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
    .intrinsic_proc_cache_map = hash_map_make(Intrinsic_Proc_Cache_Map, .initial_capacity = 128),
    .jit = {0},
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

  // Intern common symbols used during parsing
  compilation->common_symbols = (Common_Symbols) {
    .apply = mass_ensure_symbol(compilation, slice_literal("apply")),
    .fn = mass_ensure_symbol(compilation, slice_literal("fn")),
    .get = mass_ensure_symbol(compilation, slice_literal("get")),
    .intrinsic = mass_ensure_symbol(compilation, slice_literal("intrinsic")),
    .label = mass_ensure_symbol(compilation, slice_literal("label")),
    .operator = mass_ensure_symbol(compilation, slice_literal("operator")),
    .placeholder = mass_ensure_symbol(compilation, slice_literal("placeholder")),
    .postfix_block = mass_ensure_symbol(compilation, slice_literal("postfix_block")),
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
    .operator_colon_equal = mass_ensure_symbol(compilation, slice_literal(":=")),
    .operator_double_colon = mass_ensure_symbol(compilation, slice_literal("::")),
    .operator_comma = mass_ensure_symbol(compilation, slice_literal(",")),
    .operator_dot = mass_ensure_symbol(compilation, slice_literal(".")),
    .operator_dot_star = mass_ensure_symbol(compilation, slice_literal(".*")),
    .operator_equal = mass_ensure_symbol(compilation, slice_literal("=")),
    .operator_fat_arrow = mass_ensure_symbol(compilation, slice_literal("=>")),
    .operator_space = mass_ensure_symbol(compilation, slice_literal(" ")),
    .operator_tilde = mass_ensure_symbol(compilation, slice_literal("~")),
  };

  mass_compilation_init_scopes(compilation);
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
  hash_map_destroy(compilation->intrinsic_proc_cache_map);
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
}

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
  switch(target_descriptor->tag) {
    case Descriptor_Tag_Raw:
    case Descriptor_Tag_Integer: {
      // All good
    } break;
    case Descriptor_Tag_Float: {
      panic("TODO");
    } break;
    case Descriptor_Tag_Void:
    case Descriptor_Tag_Never:
    case Descriptor_Tag_Struct:
    case Descriptor_Tag_Pointer_To:
    case Descriptor_Tag_Fixed_Array:
    case Descriptor_Tag_Function_Instance: {
      return Literal_Cast_Result_Target_Not_An_Integer;
    }
  }

  u64 bit_size = target_descriptor->bit_size.as_u64;
  if (bit_size > 64) {
    return Literal_Cast_Result_Target_Too_Big;
  }

  u64 bits = const_value_as_i64(value)->bits;

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
