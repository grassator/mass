#ifndef CALLING_CONVENTION_H
#define CALLING_CONVENTION_H

#include "types.h"
#include "value.h"

static void
mass_x86_64_call_encode_proc(
  Function_Builder *builder,
  Storage address_storage,
  const Source_Range *source_range,
  const Scope *scope
);

static Function_Call_Setup
calling_convention_x86_64_windows_call_setup_proc(
  const Allocator *allocator,
  const Function_Info *function
);

static const Calling_Convention calling_convention_x86_64_windows = {
  .call_setup_proc = calling_convention_x86_64_windows_call_setup_proc,
  .register_volatile_bitset = (
    // Arguments
    (1llu << Register_C) | (1llu << Register_D) | (1llu << Register_R8) | (1llu << Register_R9) |
    // Return
    (1llu << Register_A) |
    // Other
    (1llu << Register_R10) | (1llu << Register_R11)
  ),
};

static Function_Call_Setup
calling_convention_x86_64_system_v_call_setup_proc(
  const Allocator *allocator,
  const Function_Info *function
);

static const Calling_Convention calling_convention_x86_64_system_v = {
  .call_setup_proc = calling_convention_x86_64_system_v_call_setup_proc,
  .register_volatile_bitset = (
    // Arguments
    (1llu << Register_DI) | (1llu << Register_SI) | (1llu << Register_D) |
    (1llu << Register_C) | (1llu << Register_R8) | (1llu << Register_R9) |
    // Varargs / Return
    (1llu << Register_A) | (1llu << Register_D) | // 'D' is used both for args and return
    // Other
    (1llu << Register_R10) | (1llu << Register_R11)
  ),
};

#endif // CALLING_CONVENTION_H

static void
calling_convention_x86_64_common_end_proc(
  Program *program,
  Function_Builder *builder
);

#ifdef CALLING_CONVENTION_IMPLEMENTATION

static void
call_setup_fill_parameter_register_bitset(
  Function_Call_Setup *setup
) {
  DYN_ARRAY_FOREACH(Function_Call_Parameter, param, setup->parameters) {
    Storage storage = param->storage;
    u64 target_arg_register_bitset = register_bitset_from_storage(&storage);
    if(setup->parameter_registers_bitset.bits & target_arg_register_bitset) {
      panic("Found overlapping register usage in call setup");
    }
    setup->parameter_registers_bitset.bits |= target_arg_register_bitset;
  }
}

static s32
calling_convention_x86_64_adjust_stack_offset(
  Stack_Area area,
  s32 stack_offset,
  s32 stack_reserve,
  s32 argument_stack_base
) {
  switch(area) {
    case Stack_Area_Local: {
      assert(stack_offset < 0);
      return stack_reserve + stack_offset;
    }
    case Stack_Area_Received_Argument: {
      assert(stack_offset >= 0);
      return argument_stack_base + stack_offset;
    }
    case Stack_Area_Call_Target_Argument: {
      assert(stack_offset >= 0);
      return stack_offset;
    }
  }
  panic("Unexpected stack area tag");
  return 0;
}

static const s32 X86_64_REGISTER_SIZE = 8;

static s32
calling_convention_x86_64_push_size(
  Function_Builder *builder
) {
  s32 push_size = 0;
  // :RegisterPushPop
  // pushes change the stack pointer so we need to account for that
  for (s32 reg_index = Register_R15; reg_index >= Register_A; --reg_index) {
    if (register_bitset_get(builder->register_used_bitset.bits, reg_index)) {
      if (!register_bitset_get(builder->register_volatile_bitset.bits, reg_index)) {
        push_size += X86_64_REGISTER_SIZE;
      }
    }
  }
  return push_size;
}

static void
calling_convention_x86_64_common_end_proc(
  Program *program,
  Function_Builder *builder
) {
  // Here is how stack looks at this point
  //   > return address
  //   > some number of pushes
  //   > locals

  // first we make all of them 8-byte aligned - return address and pushes are
  // naturally register-sized and locals are aligned here:
  builder->stack_reserve += builder->max_call_parameters_stack_size;
  builder->stack_reserve = s32_align(builder->stack_reserve, X86_64_REGISTER_SIZE);

  s32 push_size = calling_convention_x86_64_push_size(builder);

  // their sum must then be 16-byte aligned as per ABI
  s32 return_address_size = X86_64_REGISTER_SIZE;
  // :StackLayout
  s32 argument_stack_base = builder->stack_reserve + push_size + return_address_size;
  if (argument_stack_base % 16) {
    argument_stack_base += X86_64_REGISTER_SIZE;
    builder->stack_reserve += X86_64_REGISTER_SIZE;
  }

  // Adjust stack locations
  Instruction *previous = 0;
  for (Instruction_Bucket *bucket = builder->code_block.first_bucket; bucket; bucket = bucket->next) {
    for (u64 i = 0; i < bucket->length; ++i) {
      Instruction *instruction = &bucket->items[i];
      switch(instruction->tag) {
        case Instruction_Tag_Label:
        case Instruction_Tag_Label_Patch:
        case Instruction_Tag_Location: {
          // Nothing to do
        } break;
        case Instruction_Tag_Bytes: {
          // Handled below
        } break;
        // :StackPatch
        case Instruction_Tag_Stack_Patch: {
          assert(previous->tag == Instruction_Tag_Bytes);
          Instruction_Stack_Patch *patch = &instruction->Stack_Patch;
          u8 *mod_r_m = previous->Bytes.memory + patch->mod_r_m_offset_in_previous_instruction;
          u8 mod_r_m_byte_size = 1;
          u8 sib_byte_size = 1;
          void *displacement32 = (mod_r_m + mod_r_m_byte_size + sib_byte_size);
          s32 original_stack_offset;
          memcpy(&original_stack_offset, displacement32, sizeof(original_stack_offset));
          s32 stack_offset = calling_convention_x86_64_adjust_stack_offset(
            patch->stack_area, original_stack_offset, builder->stack_reserve, argument_stack_base
          );
          memcpy(displacement32, &stack_offset, sizeof(stack_offset));
          // :OversizedStackOffsets
          // Patch the instruction to have a smaller size displacement if it fits
          if (s32_fits_into_s8(stack_offset)) {
            // overwrite MOD part with MOD_8
            *mod_r_m &= 0b00111111;
            *mod_r_m |= MOD_Displacement_s8 << 6;
            // Move the remaining bytes 3 positions to the left
            u8 *remainder_bytes = (u8 *)displacement32 + sizeof(s32);
            u8 *instruction_end = previous->Bytes.memory + previous->Bytes.length;
            s64 remainder_length = instruction_end - remainder_bytes;
            memmove((s8 *)displacement32 + 1, remainder_bytes, remainder_length);
            previous->Bytes.length -= sizeof(s32) - sizeof(s8);
          }
        } break;
      }
      previous = instruction;
    }
  }
}

static void
x86_64_system_v_adjust_classification_if_no_register_available(
  System_V_Register_State *registers,
  System_V_Classification *classification
) {
  if (classification->class == SYSTEM_V_ARGUMENT_CLASS_INTEGER) {
    if (registers->general.index + classification->eightbyte_count > registers->general.count) {
      classification->class = SYSTEM_V_ARGUMENT_CLASS_MEMORY;
    }
  }
  if (classification->class == SYSTEM_V_ARGUMENT_CLASS_SSE) {
    if (registers->vector.index + classification->eightbyte_count > registers->vector.count) {
      classification->class = SYSTEM_V_ARGUMENT_CLASS_MEMORY;
    }
  }
}

static Function_Call_Parameter
x86_64_system_v_parameter_for_classification(
  const Allocator *allocator,
  System_V_Register_State *registers,
  const System_V_Classification *classification,
  u64 *stack_offset
) {
  u64 byte_size = descriptor_byte_size(classification->descriptor);
  Storage storage = imm0;
  switch(classification->class) {
    case SYSTEM_V_ARGUMENT_CLASS_NO_CLASS: {
      goto absolute;
    } break;
    case SYSTEM_V_ARGUMENT_CLASS_INTEGER: {
      System_V_Registers *gpr = &registers->general;
      assert (gpr->index + classification->eightbyte_count <= gpr->count);
      if (classification->eightbyte_count == 1) {
        Register reg = gpr->items[gpr->index++];
        storage = (Storage) {
          .tag = Storage_Tag_Register,
          .flags = Storage_Flags_None,
          .bit_size = classification->descriptor->bit_size,
          .Register.index = reg
        };
        switch(classification->descriptor->bit_size.as_u64) {
          case 64:
          case 32:
          case 16:
          case 8: {
            storage.Register.packed = false;
          } break;
          default: {
            storage.Register.packed = true;
            storage.Register.offset_in_bits = 0;
          } break;
        }
      } else if (classification->eightbyte_count == 2) {
        Storage *allocated_storages = allocator_allocate_array(allocator, Storage, 2);
        allocated_storages[0] = storage_register(gpr->items[gpr->index++], (Bits){64});
        allocated_storages[1] = storage_register(gpr->items[gpr->index++], (Bits){64});
        Array_Storage_Ptr pieces = dyn_array_make(Array_Storage_Ptr, .capacity = 2, .allocator = allocator);
        dyn_array_push(pieces, &allocated_storages[0]);
        dyn_array_push(pieces, &allocated_storages[1]);
        storage = (Storage) {
          .tag = Storage_Tag_Disjoint,
          .bit_size = classification->descriptor->bit_size,
          .Disjoint = { .pieces = pieces },
        };
      } else {
        panic("Unexpected eightbyte_count for an INTEGER class argument");
      }
      goto absolute;
    } break;
    case SYSTEM_V_ARGUMENT_CLASS_SSE: {
      assert (registers->vector.index + classification->eightbyte_count <= registers->vector.count);
      if (classification->eightbyte_count == 1) {
        Register reg = registers->vector.items[registers->vector.index++];
        storage = storage_register(reg, classification->descriptor->bit_size);
      } else {
        panic("TODO support packed vector values");
      }
      goto absolute;
    } break;
    case SYSTEM_V_ARGUMENT_CLASS_MEMORY: {
      u64 alignment = descriptor_byte_alignment(classification->descriptor);
      *stack_offset = u64_align(*stack_offset, u64_max(8, alignment));
      Bits bit_size = classification->descriptor->bit_size;
      Function_Call_Parameter result = {
        .flags = Function_Call_Parameter_Flags_None,
        .descriptor = classification->descriptor,
        .storage = storage_stack(u64_to_s32(*stack_offset), bit_size, Stack_Area_Call_Target_Argument),
      };
      *stack_offset += byte_size;
      return result;
    } break;
    case SYSTEM_V_ARGUMENT_CLASS_SSEUP:
    case SYSTEM_V_ARGUMENT_CLASS_X87:
    case SYSTEM_V_ARGUMENT_CLASS_X87UP:
    case SYSTEM_V_ARGUMENT_CLASS_COMPLEX_X87: {
      panic("TODO");
    } break;
    default: {
      panic("Unpexected SYSTEM_V class");
    } break;
  }

  absolute:
  return (Function_Call_Parameter){
    .descriptor = classification->descriptor,
    .storage = storage,
  };
}

// TODO verify this implementation against GCC
// https://github.com/gcc-mirror/gcc/blob/master/gcc/config/i386/i386.c#L2080

typedef enum {
  System_V_Aggregate_Iterator_Tag_Struct,
  System_V_Aggregate_Iterator_Tag_Array,
} System_V_Aggregate_Iterator_Tag;

typedef struct {
  System_V_Aggregate_Iterator_Tag tag;
  const Descriptor *aggregate;
  const Descriptor *item;
  u64 offset;
  u64 next_index;
} System_V_Aggregate_Iterator;

static inline u64
system_v_item_iterator_count(
  const System_V_Aggregate_Iterator *it
) {
  switch(it->tag) {
    case System_V_Aggregate_Iterator_Tag_Struct: {
      assert(it->aggregate->tag == Descriptor_Tag_Struct);
      return dyn_array_length(it->aggregate->Struct.fields);
    }
    case System_V_Aggregate_Iterator_Tag_Array: {
      assert(it->aggregate->tag == Descriptor_Tag_Fixed_Array);
      return it->aggregate->Fixed_Array.length;
    }
  }
  panic("UNEXPECTED System_V_Aggregate_Iterator_Tag");
  return 0;
}

static inline bool
system_v_item_iterator_next(
  System_V_Aggregate_Iterator *it
) {
  u64 item_count = system_v_item_iterator_count(it);
  if (it->next_index >= item_count) return false;
  switch(it->tag) {
    case System_V_Aggregate_Iterator_Tag_Struct: {
      assert(it->aggregate->tag == Descriptor_Tag_Struct);
      const Struct_Field *field = dyn_array_get(it->aggregate->Struct.fields, it->next_index);
      it->item = field->descriptor;
      it->offset = field->offset;
    } break;
    case System_V_Aggregate_Iterator_Tag_Array: {
      assert(it->aggregate->tag == Descriptor_Tag_Fixed_Array);
      it->item = it->aggregate->Fixed_Array.item;
      it->offset = descriptor_byte_size(it->item) * it->next_index;
    } break;
    default: {
      panic("UNEXPECTED System_V_Aggregate_Iterator_Tag");
      return false;
    }
  }
  it->next_index += 1;
  return true;
}

static inline bool
x86_64_system_v_has_unaligned(
  System_V_Aggregate_Iterator it
) {
  while(system_v_item_iterator_next(&it)) {
    if (it.offset % descriptor_byte_alignment(it.item) != 0) return true;
  }
  return false;
}

static void
x86_64_system_v_classify_field_recursively(
  System_V_Eightbyte_Array *eightbyte_array,
  System_V_Aggregate_Iterator *it,
  u64 parent_offset
);

static System_V_Classification
x86_64_system_v_classify(
  const Descriptor *descriptor
) {
  u64 byte_size = descriptor_byte_size(descriptor);
  u64 eightbyte = 8;

  System_V_Aggregate_Iterator it;
  switch(descriptor->tag) {
    case Descriptor_Tag_Never:
    case Descriptor_Tag_Void: {
      return (System_V_Classification){ .class = SYSTEM_V_ARGUMENT_CLASS_NO_CLASS, .descriptor = descriptor };
    } break;
    case Descriptor_Tag_Function_Instance:
    case Descriptor_Tag_Pointer_To:
    case Descriptor_Tag_Raw:
    case Descriptor_Tag_Integer: {
      if (descriptor->bit_size.as_u64 == 0) {
        return (System_V_Classification){ .class = SYSTEM_V_ARGUMENT_CLASS_NO_CLASS, .descriptor = descriptor };
      }
      if (byte_size <= eightbyte) {
        SYSTEM_V_ARGUMENT_CLASS class = SYSTEM_V_ARGUMENT_CLASS_INTEGER;
        return (System_V_Classification){
          .class = class,
          .descriptor = descriptor,
          .eightbyte_count = 1,
        };
      } else {
        return (System_V_Classification){ .class = SYSTEM_V_ARGUMENT_CLASS_MEMORY, .descriptor = descriptor };
      }
    } break;
    case Descriptor_Tag_Float: {
      assert(byte_size <= eightbyte);
      return (System_V_Classification){ .class = SYSTEM_V_ARGUMENT_CLASS_SSE, .descriptor = descriptor };
    } break;
    case Descriptor_Tag_Struct: {
      it = (System_V_Aggregate_Iterator) {
        .tag = System_V_Aggregate_Iterator_Tag_Struct,
        .aggregate = descriptor,
      };
    } break;
    case Descriptor_Tag_Fixed_Array: {
      it = (System_V_Aggregate_Iterator) {
        .tag = System_V_Aggregate_Iterator_Tag_Array,
        .aggregate = descriptor,
      };
    } break;
    default: {
      panic("Unexpected descriptor tag");
      return (System_V_Classification){0};
    } break;
  }

  // 1. If the size of an object is larger than eight eightbytes,
  // or it contains unaligned fields, it has class MEMORY
  if (byte_size > 8 * eightbyte || x86_64_system_v_has_unaligned(it)) {
    return (System_V_Classification){ .class = SYSTEM_V_ARGUMENT_CLASS_MEMORY, .descriptor = descriptor };
  }
  // 2. If a C++ object is non-trivial for the purpose of calls, as specified in the
  // C++ ABI 13, it is passed by invisible reference (the object is replaced in the
  // parameter list by a pointer that has class INTEGER)
  bool is_c_plus_plus_non_trivial = false; // TODO allow to specify / detect this
  if (is_c_plus_plus_non_trivial) {
    panic("TODO propagate somehow to the caller that this in an implicit reference");
    return (System_V_Classification){ .class = SYSTEM_V_ARGUMENT_CLASS_INTEGER, .descriptor = descriptor };
  }

  // 3. If the size of the aggregate exceeds a single eightbyte, each is classified
  // separately. Each eightbyte gets initialized to class NO_CLASS.
  System_V_Eightbyte_Array eightbyte_array = {
    .classes = {0},
    .count = (byte_size + eightbyte - 1) / (eightbyte),
  };
  for (u32 i = 0; i < eightbyte_array.count; ++i) {
    eightbyte_array.classes[i] = SYSTEM_V_ARGUMENT_CLASS_NO_CLASS;
  }

  // 4. Each field of an object is classified recursively so that always two fields are considered.
  // The resulting class is calculated according to the classes of the fields in the eightbyte:
  x86_64_system_v_classify_field_recursively(&eightbyte_array, &it, 0);

  SYSTEM_V_ARGUMENT_CLASS struct_class = SYSTEM_V_ARGUMENT_CLASS_NO_CLASS;

  // 5. Then a post merger cleanup is done:
  for (u32 i = 0; i < eightbyte_array.count; ++i) {
    bool is_first = i == 0;
    SYSTEM_V_ARGUMENT_CLASS *class = &eightbyte_array.classes[i];
    SYSTEM_V_ARGUMENT_CLASS preceeded_by_class =
      is_first ? SYSTEM_V_ARGUMENT_CLASS_NO_CLASS : eightbyte_array.classes[i - 1];

    // 5(a) If one of the classes is MEMORY, the whole argument is passed in memory.
    if (*class == SYSTEM_V_ARGUMENT_CLASS_MEMORY) {
      struct_class = SYSTEM_V_ARGUMENT_CLASS_MEMORY;
      break;
    }
    // 5(b) If X87UP is not preceded by X87, the whole argument is passed in memory.
    if (*class == SYSTEM_V_ARGUMENT_CLASS_X87UP && preceeded_by_class != SYSTEM_V_ARGUMENT_CLASS_X87) {
      struct_class = SYSTEM_V_ARGUMENT_CLASS_MEMORY;
      break;
    }
    // 5(c) If the size of the aggregate exceeds two eightbytes and the first eightbyte
    // isn't SSE or any other eightbyte isn’t SSEUP, the whole argument is passed in memory.
    if (byte_size > 2 * eightbyte) {
      if (is_first) {
        if (*class != SYSTEM_V_ARGUMENT_CLASS_SSE) {
          struct_class = SYSTEM_V_ARGUMENT_CLASS_MEMORY;
          break;
        }
      } else {
        if (*class != SYSTEM_V_ARGUMENT_CLASS_SSEUP) {
          struct_class = SYSTEM_V_ARGUMENT_CLASS_MEMORY;
          break;
        }
      }
    }
    // 5(d) If SSEUP is not preceded by SSE or SSEUP, it is converted to SSE.
    if (
      *class == SYSTEM_V_ARGUMENT_CLASS_SSEUP &&
      !(preceeded_by_class == SYSTEM_V_ARGUMENT_CLASS_SSE || preceeded_by_class == SYSTEM_V_ARGUMENT_CLASS_SSEUP)
    ) {
      *class = SYSTEM_V_ARGUMENT_CLASS_SSE;
    }
  }

  // FIXME we should return the whole array instead  of doing this
  if (struct_class == SYSTEM_V_ARGUMENT_CLASS_NO_CLASS) {
    struct_class = eightbyte_array.classes[0];
  }
  System_V_Classification classification = {
    .descriptor = descriptor,
    .eightbyte_count = eightbyte_array.count,
    .class = struct_class,
  };

  return classification;
}

static void
x86_64_system_v_classify_field_recursively(
  System_V_Eightbyte_Array *eightbyte_array,
  System_V_Aggregate_Iterator *it,
  u64 parent_offset
) {
  u32 eightbyte = 8;
  while(system_v_item_iterator_next(it)) {
    u32 item_byte_size = u64_to_u32(descriptor_byte_size(it->item));
    u64 field_offset_in_root_aggregate = parent_offset + it->offset;

    switch(it->item->tag) {
      case Descriptor_Tag_Void:
      case Descriptor_Tag_Never:
      case Descriptor_Tag_Function_Instance:
      case Descriptor_Tag_Pointer_To:
      case Descriptor_Tag_Raw:
      case Descriptor_Tag_Float:
      case Descriptor_Tag_Integer: {
        u64 start_eightbyte_index = field_offset_in_root_aggregate / eightbyte;
        u64 end_eightbyte_index = (field_offset_in_root_aggregate + item_byte_size - 1) / eightbyte;

        // System V x86_64 ABI assumes that no type is larger than 8 bytes. Even SSE/AVX types
        // are split into 8-byte chunks for the purposes of this algorithm.
        // Also unaligned field are disallowed in step 1. of the aggregate classification.
        // These two properies combined mean that a non-aggregate field must
        // never cross an 8-byte boundary.
        assert(start_eightbyte_index == end_eightbyte_index);

        // We should not get an index that is larger than root aggregate size in 8-bytes
        assert(start_eightbyte_index < eightbyte_array->count);

        SYSTEM_V_ARGUMENT_CLASS *eightbyte_class = &eightbyte_array->classes[start_eightbyte_index];

        SYSTEM_V_ARGUMENT_CLASS field_class;
        if (item_byte_size == 0) {
          field_class = SYSTEM_V_ARGUMENT_CLASS_NO_CLASS;
        } else {
          if (item_byte_size <= eightbyte) {
            field_class = descriptor_is_float(it->item) ? SYSTEM_V_ARGUMENT_CLASS_SSE : SYSTEM_V_ARGUMENT_CLASS_INTEGER;
          } else {
            field_class = SYSTEM_V_ARGUMENT_CLASS_MEMORY;
          }
        }

        // 4. Each field of an object is classified recursively so that always two fields are considered.
        // The resulting class is calculated according to the classes of the fields in the eightbyte:

        // 4(a) If both classes are equal, this is the resulting class.
        if (*eightbyte_class == field_class) {
          *eightbyte_class = field_class;
        } else
        // 4(b) If one of the classes is NO_CLASS, the resulting class is the other class.
        if (field_class == SYSTEM_V_ARGUMENT_CLASS_NO_CLASS) {
          eightbyte_class = eightbyte_class;
        } else if (eightbyte_class == SYSTEM_V_ARGUMENT_CLASS_NO_CLASS) {
          *eightbyte_class = field_class;
        } else
        // 4(c) If one of the classes is MEMORY, the result is the MEMORY class.
        if (field_class == SYSTEM_V_ARGUMENT_CLASS_MEMORY) {
          *eightbyte_class = SYSTEM_V_ARGUMENT_CLASS_MEMORY;
        } else
        // 4(d) If one of the classes is INTEGER, the result is the INTEGER class.
        if (*eightbyte_class == SYSTEM_V_ARGUMENT_CLASS_INTEGER || field_class == SYSTEM_V_ARGUMENT_CLASS_INTEGER) {
          *eightbyte_class = SYSTEM_V_ARGUMENT_CLASS_INTEGER;
        } else
        // 4(e) If one of the classes is X87, X87UP, COMPLEX_X87 class, MEMORY is used as class.
        if (
          *eightbyte_class == SYSTEM_V_ARGUMENT_CLASS_X87 ||
          *eightbyte_class == SYSTEM_V_ARGUMENT_CLASS_X87UP ||
          *eightbyte_class == SYSTEM_V_ARGUMENT_CLASS_COMPLEX_X87 ||
          field_class == SYSTEM_V_ARGUMENT_CLASS_X87 ||
          field_class == SYSTEM_V_ARGUMENT_CLASS_X87UP ||
          field_class == SYSTEM_V_ARGUMENT_CLASS_COMPLEX_X87
        ) {
          *eightbyte_class = SYSTEM_V_ARGUMENT_CLASS_MEMORY;
        }
        // 4(f) Otherwise class SSE is used.
        else {
          *eightbyte_class = SYSTEM_V_ARGUMENT_CLASS_SSE;
        }

        break;
      }
      case Descriptor_Tag_Struct: {
        System_V_Aggregate_Iterator nested_it = {
          .tag = System_V_Aggregate_Iterator_Tag_Struct,
          .aggregate = it->item,
        };
        x86_64_system_v_classify_field_recursively(
          eightbyte_array, &nested_it, field_offset_in_root_aggregate
        );
        break;
      }
      case Descriptor_Tag_Fixed_Array: {
        System_V_Aggregate_Iterator nested_it = {
          .tag = System_V_Aggregate_Iterator_Tag_Array,
          .aggregate = it->item,
        };
        x86_64_system_v_classify_field_recursively(
          eightbyte_array, &nested_it, field_offset_in_root_aggregate
        );
        break;
      }
      default: {
        panic("Unexpected descriptor tag");
        break;
      }
    }
  }
}

static Function_Call_Setup
calling_convention_x86_64_system_v_call_setup_proc(
  const Allocator *allocator,
  const Function_Info *function
) {
  Function_Call_Setup result = {
    .call_encode_proc = mass_x86_64_call_encode_proc,
    .calling_convention = &calling_convention_x86_64_system_v,
  };
  bool is_indirect_return = false;
  const Descriptor *return_descriptor = function->return_descriptor;
  if (!return_descriptor->bit_size.as_u64) {
    result.callee_return = imm0;
    result.caller_return = imm0;
  } else {
    static const Register general_registers[] = { Register_A, Register_D };
    static const Register vector_registers[] = { Register_Xmm0, Register_Xmm1 };

    System_V_Register_State registers = {
      .general = {
        .items = general_registers,
        .count = countof(general_registers),
        .index = 0,
      },
      .vector = {
        .items = vector_registers,
        .count = countof(vector_registers),
        .index = 0,
      },
    };

    System_V_Classification classification = x86_64_system_v_classify(return_descriptor);
    if (classification.class == SYSTEM_V_ARGUMENT_CLASS_MEMORY) {
      is_indirect_return = true;
      Bits bit_size = return_descriptor->bit_size;
      result.caller_return = storage_indirect(bit_size, Register_A);
      result.callee_return = storage_indirect(bit_size, Register_DI);
    } else {
      u64 stack_offset = 0;
      Function_Call_Parameter item = x86_64_system_v_parameter_for_classification(
        allocator, &registers, &classification, &stack_offset
      );

      result.callee_return = item.storage;
      result.caller_return = item.storage;
    }
  }

  static const Register general_registers[] = {
    Register_DI, Register_SI, Register_D, Register_C, Register_R8, Register_R9
  };
  static const Register vector_registers[] = {
    Register_Xmm0, Register_Xmm1, Register_Xmm2, Register_Xmm3,
    Register_Xmm4, Register_Xmm5, Register_Xmm6, Register_Xmm7,
  };

  System_V_Register_State registers = {
    .general = {
      .items = general_registers,
      .count = countof(general_registers),
      .index = is_indirect_return ? 1 : 0,
    },
    .vector = {
      .items = vector_registers,
      .count = countof(vector_registers),
      .index = 0,
    },
  };

  result.parameters = dyn_array_make(
    Array_Function_Call_Parameter,
    .allocator = allocator,
    .capacity = dyn_array_length(function->parameters) + 1,
  );

  u64 stack_offset = 0;
  for(u64 param_index = 0; param_index < dyn_array_length(function->parameters); ++param_index) {
    Resolved_Function_Parameter *param = dyn_array_get(function->parameters, param_index);
    if (param->tag == Resolved_Function_Parameter_Tag_Known) continue;
    System_V_Classification classification =
      x86_64_system_v_classify(param->descriptor);
    x86_64_system_v_adjust_classification_if_no_register_available(&registers, &classification);

    Function_Call_Parameter call_param = x86_64_system_v_parameter_for_classification(
      allocator, &registers, &classification, &stack_offset
    );
    call_param.original_index = param_index; //:ParameterOriginalIndex

    dyn_array_push(result.parameters, call_param);
  }
  result.parameters_stack_size = u64_to_u32(u64_align(stack_offset, 8));

  if (is_indirect_return) {
    // :IndirectReturnArgument
    dyn_array_push(result.parameters, (Function_Call_Parameter) {
      .flags = Function_Call_Parameter_Flags_Uninitialized,
      .descriptor = return_descriptor,
      .storage = storage_indirect(return_descriptor->bit_size, Register_DI),
    });
  }
  call_setup_fill_parameter_register_bitset(&result);

  return result;
}

static Function_Call_Setup
calling_convention_x86_64_windows_call_setup_proc(
  const Allocator *allocator,
  const Function_Info *function
) {
  Function_Call_Setup result = {
    .call_encode_proc = mass_x86_64_call_encode_proc,
    .calling_convention = &calling_convention_x86_64_windows,
  };
  const Descriptor *return_descriptor = function->return_descriptor;
  bool is_indirect_return = false;
  if (!return_descriptor->bit_size.as_u64) {
    result.callee_return = imm0;
    result.caller_return = imm0;
  } else {
    Bits bit_size = return_descriptor->bit_size;
    if (descriptor_is_float(return_descriptor)) {
      Storage common_storage = storage_register(Register_Xmm0, bit_size);
      result.callee_return = common_storage;
      result.caller_return = common_storage;
    } else {
      switch(bit_size.as_u64) {
        case 64:
        case 32:
        case 16:
        case 8: {
          Storage common_storage = storage_register(Register_A, bit_size);
          result.callee_return = common_storage;
          result.caller_return = common_storage;
        } break;
        default: {
          is_indirect_return = true;
          result.caller_return = storage_indirect(return_descriptor->bit_size, Register_A);
          result.callee_return = storage_indirect(return_descriptor->bit_size, Register_C);
        } break;
      }
    }
  }

  static const Register general_registers[] = {Register_C, Register_D, Register_R8, Register_R9};
  static const Register float_registers[] = {Register_Xmm0, Register_Xmm1, Register_Xmm2, Register_Xmm3};
  assert(countof(general_registers) == countof(float_registers));

  result.parameters = dyn_array_make(
    Array_Function_Call_Parameter,
    .allocator = allocator,
    .capacity = dyn_array_length(function->parameters) + 1,
  );

  u64 argument_index = is_indirect_return ? 1 : 0;

  for(u64 param_index = 0; param_index < dyn_array_length(function->parameters); ++param_index) {
    Resolved_Function_Parameter *param = dyn_array_get(function->parameters, param_index);
    if (param->tag == Resolved_Function_Parameter_Tag_Known) continue;
    Function_Call_Parameter item = {
      .flags = Function_Call_Parameter_Flags_None,
      .descriptor = param->descriptor,
      .original_index = param_index, //:ParameterOriginalIndex
    };

    Bits bit_size = item.descriptor->bit_size;
    switch(bit_size.as_u64) {
      case 64:
      case 32:
      case 16:
      case 8: {
        // passed by value
      } break;
      default: {
        item.flags |= Function_Call_Parameter_Flags_Implicit_Pointer;
        bit_size = (Bits){64};
      } break;
    }

    if (argument_index < countof(general_registers)) {
      Register reg = descriptor_is_float(item.descriptor)
        ? float_registers[argument_index]
        : general_registers[argument_index];

      item.storage = storage_register(reg, bit_size);
    } else {
      s32 offset = u64_to_s32(argument_index * 8);
      item.storage = storage_stack(offset, bit_size, Stack_Area_Call_Target_Argument);
    }
    dyn_array_push(result.parameters, item);
    argument_index += 1;
  }

  if (is_indirect_return) {
    // :IndirectReturnArgument
    dyn_array_push(result.parameters, (Function_Call_Parameter) {
      .flags = Function_Call_Parameter_Flags_Uninitialized,
      .descriptor = return_descriptor,
      .storage = storage_indirect(return_descriptor->bit_size, Register_C),
    });
  }

  // In this calling convention a home area for at least 4 arguments is always reserved
  result.parameters_stack_size = u64_to_u32(u64_max(4, dyn_array_length(function->parameters)) * 8);

  call_setup_fill_parameter_register_bitset(&result);

  return result;
}

#endif

