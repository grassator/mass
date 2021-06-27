#ifndef CALLING_CONVENTION_H
#define CALLING_CONVENTION_H

#include "types.h"
#include "value.h"

static void
calling_convention_x86_64_windows_body_end_proc(
  Program *program,
  Function_Builder *builder
);

static Memory_Layout
calling_convention_x86_64_windows_arguments_layout_proc(
  const Allocator *allocator,
  const Function_Info *function
);

static Value *
calling_convention_x86_64_windows_return_proc(
  const Allocator *allocator,
  const Function_Info *function,
  Function_Argument_Mode mode
);

static const Calling_Convention calling_convention_x86_64_windows = {
  .body_end_proc = calling_convention_x86_64_windows_body_end_proc,
  .arguments_layout_proc = calling_convention_x86_64_windows_arguments_layout_proc,
  .return_proc = calling_convention_x86_64_windows_return_proc,
  .register_volatile_bitset = (
    // Arguments
    (1llu << Register_C) | (1llu << Register_D) | (1llu << Register_R8) | (1llu << Register_R9) |
    // Return
    (1llu << Register_A) |
    // Other
    (1llu << Register_R10) | (1llu << Register_R11)
  ),
};

static void
calling_convention_x86_64_system_v_body_end_proc(
  Program *program,
  Function_Builder *builder
);

static Memory_Layout
calling_convention_x86_64_system_v_arguments_layout_proc(
  const Allocator *allocator,
  const Function_Info *function
);

static Value *
calling_convention_x86_64_system_v_return_proc(
  const Allocator *allocator,
  const Function_Info *function,
  Function_Argument_Mode mode
);

static const Calling_Convention calling_convention_x86_64_system_v = {
  .body_end_proc = calling_convention_x86_64_system_v_body_end_proc,
  .arguments_layout_proc = calling_convention_x86_64_system_v_arguments_layout_proc,
  .return_proc = calling_convention_x86_64_system_v_return_proc,
  .register_volatile_bitset = (
    // Arguments
    (1llu << Register_DI) | (1llu << Register_SI) | (1llu << Register_D) |
    (1llu << Register_C) | (1llu << Register_R8) | (1llu << Register_R9) |
    // Varargs / Return
    (1llu << Register_A) |
    // Other
    (1llu << Register_R10) | (1llu << Register_R11)
  ),
};

#endif // CALLING_CONVENTION_H

#ifdef CALLING_CONVENTION_IMPLEMENTATION

static void
calling_convention_x86_64_common_end_proc(
  Program *program,
  Function_Builder *builder
) {
  const s32 register_size = 8;

  s32 push_size = 0;
  // :RegisterPushPop
  // pushes change the stack pointer so we need to account for that
  for (s32 reg_index = Register_R15; reg_index >= Register_A; --reg_index) {
    if (register_bitset_get(builder->used_register_bitset, reg_index)) {
      if (!register_bitset_get(builder->register_volatile_bitset, reg_index)) {
        push_size += register_size;
      }
    }
  }

  builder->stack_reserve += builder->max_call_parameters_stack_size;

  // Here is how stack looks at this point
  //   > return address
  //   > some number of pushes
  //   > locals
  // first we make all of them 8-byte aligned - return address and pushes are
  // naturally register-sized and locals are aligned here:
  builder->stack_reserve = s32_align(builder->stack_reserve, register_size);
  // their sum must then be 16-byte aligned as per ABI
  s32 return_address_size = register_size;
  s32 argument_stack_base = builder->stack_reserve + push_size + return_address_size;
  if (argument_stack_base % 16) {
    argument_stack_base += register_size;
    builder->stack_reserve += register_size;
  }

  // Adjust stack locations
  DYN_ARRAY_FOREACH (Instruction, instruction, builder->code_block.instructions) {
    for (u8 storage_index = 0; storage_index < countof(instruction->Assembly.operands); ++storage_index) {
      Storage *storage = &instruction->Assembly.operands[storage_index];
      if (storage->tag != Storage_Tag_Memory) continue;
      Memory_Location *location = &storage->Memory.location;
      switch(location->tag) {
        case Memory_Location_Tag_Stack: {
          Memory_Location_Stack stack = location->Stack;
          *storage = storage_indirect(storage->byte_size, Register_SP);
          switch(stack.area) {
            case Stack_Area_Local: {
              assert(stack.offset < 0);
              storage->Memory.location.Indirect.offset = builder->stack_reserve + stack.offset;
              break;
            }
            case Stack_Area_Received_Argument: {
              assert(stack.offset >= 0);
              storage->Memory.location.Indirect.offset = argument_stack_base + stack.offset;
              break;
            }
            case Stack_Area_Call_Target_Argument: {
              assert(stack.offset >= 0);
              storage->Memory.location.Indirect.offset = stack.offset;
              break;
            }
          }
          break;
        }
        case Memory_Location_Tag_Instruction_Pointer_Relative:
        case Memory_Location_Tag_Indirect: {
          // Nothing to do
          break;
        }
      }
    }
  }
}

typedef enum {
  SYSTEM_V_NO_CLASS,
  SYSTEM_V_INTEGER,
  SYSTEM_V_SSE,
  SYSTEM_V_SSEUP,
  SYSTEM_V_X87,
  SYSTEM_V_X87UP,
  SYSTEM_V_COMPLEX_X87,
  SYSTEM_V_MEMORY,
} SYSTEM_V_ARGUMENT_CLASS;

typedef struct {
  Storage storage;
  SYSTEM_V_ARGUMENT_CLASS class;
} System_V_Class_Entry;
typedef dyn_array_type(System_V_Class_Entry) Array_System_V_Class_Entry;

typedef struct {
  const Register *items;
  u32 count;
  u32 index;
} System_V_Registers;

typedef struct {
  Memory_Layout memory_layout;
  System_V_Registers general_purpose_registers;
  System_V_Registers vector_registers;
  u64 stack_offset;
  u64 offset_in_classified_eightbyte;
} System_V_Classification_State;

static SYSTEM_V_ARGUMENT_CLASS
x86_64_system_v_memory_layout_item_for_class(
  System_V_Classification_State *state,
  SYSTEM_V_ARGUMENT_CLASS class,
  Slice name,
  const Descriptor *descriptor,
  Memory_Layout_Item *out_result
) {
  u64 byte_size = descriptor_byte_size(descriptor);
  switch(class) {
    case SYSTEM_V_NO_CLASS: {
      *out_result = (Memory_Layout_Item) {
        .tag = Memory_Layout_Item_Tag_Absolute,
        .name = name,
        .descriptor = descriptor,
        .Absolute = {.storage = storage_none},
      };
      return SYSTEM_V_NO_CLASS;
    }
    case SYSTEM_V_INTEGER: {
      if (state->general_purpose_registers.index < state->general_purpose_registers.count) {
        Register reg = state->general_purpose_registers.items[state->general_purpose_registers.index++];
        *out_result = (Memory_Layout_Item) {
          .tag = Memory_Layout_Item_Tag_Absolute,
          .name = name,
          .descriptor = descriptor,
          .Absolute = {.storage = storage_register(reg, byte_size)},
        };
        return SYSTEM_V_INTEGER;
      } else {
        goto relative;
      }
      break;
    }
    case SYSTEM_V_SSE: {
      if (state->vector_registers.index < state->vector_registers.count) {
        Register reg = state->vector_registers.items[state->vector_registers.index++];
        *out_result = (Memory_Layout_Item) {
          .tag = Memory_Layout_Item_Tag_Absolute,
          .name = name,
          .descriptor = descriptor,
          .Absolute = {.storage = storage_register(reg, byte_size)},
        };
        return SYSTEM_V_SSE;
      } else {
        goto relative;
      }
      break;
    }
    case SYSTEM_V_MEMORY: {
      goto relative;
    }
    case SYSTEM_V_SSEUP:
    case SYSTEM_V_X87:
    case SYSTEM_V_X87UP:
    case SYSTEM_V_COMPLEX_X87: {
      panic("TODO");
      break;
    }
  }

  relative: {
    *out_result = (Memory_Layout_Item) {
      .tag = Memory_Layout_Item_Tag_Base_Relative,
      .flags = Memory_Layout_Item_Flags_Implicit_Pointer,
      .name = name,
      .descriptor = descriptor,
      .Base_Relative = {.offset = state->stack_offset},
    };
    state->stack_offset += u64_align(byte_size, 8);
    return SYSTEM_V_MEMORY;
  }
}

static inline SYSTEM_V_ARGUMENT_CLASS
x86_64_system_v_push_memory_layout_item_for_class(
  System_V_Classification_State *state,
  SYSTEM_V_ARGUMENT_CLASS class,
  Slice name,
  const Descriptor *descriptor
) {
  Memory_Layout_Item item;
  class = x86_64_system_v_memory_layout_item_for_class(state, class, name, descriptor, &item);
  dyn_array_push(state->memory_layout.items, item);
  return class;
}

static SYSTEM_V_ARGUMENT_CLASS
x86_64_system_v_classify(
  const Allocator *allocator,
  System_V_Classification_State *state,
  Slice name,
  const Descriptor *descriptor
) {
  u64 byte_size = descriptor_byte_size(descriptor);
  if (byte_size == 0) {
    return SYSTEM_V_NO_CLASS;
  }
  u64 eightbyte = 8;
  switch(descriptor->tag) {
    case Descriptor_Tag_Function_Instance:
    case Descriptor_Tag_Pointer_To:
    case Descriptor_Tag_Opaque: {
      if (byte_size <= eightbyte) {
        SYSTEM_V_ARGUMENT_CLASS class =
          descriptor_is_float(descriptor) ? SYSTEM_V_SSE : SYSTEM_V_INTEGER;
        return x86_64_system_v_push_memory_layout_item_for_class(state, class, name, descriptor);
      } else {
        return SYSTEM_V_MEMORY;
      }
      break;
    }
    case Descriptor_Tag_Struct: {
      bool has_unaligned = false; // FIXME calculate this
      // 1. If the size of an object is larger than eight eightbytes,
      // or it contains unaligned fields, it has class MEMORY
      if (byte_size > 8 * eightbyte || has_unaligned) {
        return x86_64_system_v_push_memory_layout_item_for_class(state, SYSTEM_V_MEMORY, name, descriptor);
      }
      // 2. If a C++ object is non-trivial for the purpose of calls, as specified in the
      // C++ ABI 13, it is passed by invisible reference (the object is replaced in the
      // parameter list by a pointer that has class INTEGER)
      bool is_c_plus_plus_non_trivial = false; // TODO allow to specify / detect this
      if (is_c_plus_plus_non_trivial) {
        return x86_64_system_v_push_memory_layout_item_for_class(state, SYSTEM_V_INTEGER, name, descriptor);
      }

      // 3. If the size of the aggregate exceeds a single eightbyte, each is classified
      // separately. Each eightbyte gets initialized to class NO_CLASS.
      SYSTEM_V_ARGUMENT_CLASS eightbyte_class = SYSTEM_V_NO_CLASS;

      // @Hack We need to somehow say that the struct has no Storage in Mass terms so
      //       if struct fields are unpacked to registers, SYSTEM_V_NO_CLASS is used
      //       for the struct as a whole and then its fields have register storages.
      //       This might cause problems down the line.
      SYSTEM_V_ARGUMENT_CLASS struct_class = SYSTEM_V_NO_CLASS;

      System_V_Classification_State saved_state = *state;
      state->memory_layout = (Memory_Layout) {
        .items = dyn_array_make(
          Array_Memory_Layout_Item,
          .allocator = allocator,
          .capacity = dyn_array_length(descriptor->Struct.memory_layout.items),
        ),
      };

      u64 last_offset = 0;

      // 4. Each field of an object is classified recursively so that always two fields are
      // considered. The resulting class is calculated according to the classes of the
      // fields in the eightbyte:
      DYN_ARRAY_FOREACH(Memory_Layout_Item, item, descriptor->Struct.memory_layout.items) {
        const Descriptor *item_descriptor = item->descriptor;
        assert(item->tag == Memory_Layout_Item_Tag_Base_Relative);
        u64 offset = item->Base_Relative.offset;

        // FIXME this algorithm is designed to operate on C-style aggregates
        //       which unlike the Memory_Layout can not specify arbitrary offsets
        //       and overlaps. So we might have to convert to that representation
        //       or do some really fancy tracking of overlaps (unions).
        if (offset < last_offset) {
          panic("FIXME support unions");
        } else {
          last_offset = offset;
        }

        if (offset - state->offset_in_classified_eightbyte >= eightbyte) {
          eightbyte_class = SYSTEM_V_NO_CLASS;
          state->offset_in_classified_eightbyte = offset;
          assert(state->offset_in_classified_eightbyte % 8 == 0);
        }
        SYSTEM_V_ARGUMENT_CLASS field_class =
          x86_64_system_v_classify(allocator, state, item->name, item_descriptor);
        // 4(a) If both classes are equal, this is the resulting class.
        if (eightbyte_class == field_class) {
          continue;
        } else
        // 4(b) If one of the classes is NO_CLASS, the resulting class is the other class.
        if (field_class == SYSTEM_V_NO_CLASS) {
          continue;
        } else if (eightbyte_class == SYSTEM_V_NO_CLASS) {
          eightbyte_class = field_class;
        } else
        // 4(c) If one of the classes is MEMORY, the result is the MEMORY class.
        if (field_class == SYSTEM_V_MEMORY) {
          eightbyte_class = SYSTEM_V_MEMORY;
        } else
        // 4(d) If one of the classes is INTEGER, the result is the INTEGER class.
        if (eightbyte_class == SYSTEM_V_INTEGER || field_class == SYSTEM_V_INTEGER) {
          eightbyte_class = SYSTEM_V_INTEGER;
        } else
        // 4(e) If one of the classes is X87, X87UP, COMPLEX_X87 class, MEMORY is used as class.
        if (
          eightbyte_class == SYSTEM_V_X87 ||
          eightbyte_class == SYSTEM_V_X87UP ||
          eightbyte_class == SYSTEM_V_COMPLEX_X87 ||
          field_class == SYSTEM_V_X87 ||
          field_class == SYSTEM_V_X87UP ||
          field_class == SYSTEM_V_COMPLEX_X87
        ) {
          eightbyte_class = SYSTEM_V_MEMORY;
        }
        // 4(f) Otherwise class SSE is used.
        else {
          eightbyte_class = SYSTEM_V_SSE;
        }
        if (eightbyte_class == SYSTEM_V_MEMORY) {
          struct_class = SYSTEM_V_MEMORY;
          break;
        }
      }

      // FIXME 5. Then a post merger cleanup is done:

      if (struct_class == SYSTEM_V_MEMORY) {
        dyn_array_destroy(state->memory_layout.items);
        // Restore both the memory layout and the available registers info
        *state = saved_state;
      } else {
        // :StructIds
        // Because we have to allocate a distinct Descriptor here
        // structs are not longer comparable by pointer.
        Descriptor *updated_descriptor = allocator_allocate(allocator, Descriptor);
        *updated_descriptor = *descriptor;
        updated_descriptor->Struct.memory_layout = state->memory_layout;
        descriptor = updated_descriptor;

        // Restore the memory layout, keep available registers info
        state->memory_layout = saved_state.memory_layout;

        // This is required to correctly propagate up the recursive call that a field
        // that is itself a struct is packed inside a single register.
        if (byte_size <= eightbyte) {
          assert(struct_class == SYSTEM_V_NO_CLASS);
          struct_class = eightbyte;
        }
      }
      return x86_64_system_v_push_memory_layout_item_for_class(state, struct_class, name, descriptor);
    }
    case Descriptor_Tag_Fixed_Size_Array: {
      panic("TODO implement array classification");
      break;
    }
  }
  panic("Unexpected descriptor tag");
  return SYSTEM_V_NO_CLASS;
}

static void
calling_convention_x86_64_system_v_body_end_proc(
  Program *program,
  Function_Builder *builder
) {

  // FIXME avoid doing this
  Value *return_value = calling_convention_x86_64_system_v_return_proc(
    allocator_default, builder->function, Function_Argument_Mode_Body
  );

  bool is_indirect_return = (
    return_value->storage.tag == Storage_Tag_Memory &&
    return_value->storage.Memory.location.tag == Memory_Location_Tag_Indirect &&
    return_value->storage.Memory.location.Indirect.base_register == Register_DI
  );
  allocator_deallocate(allocator_default, return_value, sizeof(return_value));

  // :ReturnTypeLargerThanRegister
  if(is_indirect_return) {
    push_instruction(&builder->code_block.instructions, builder->return_value->source_range,
      (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {rax, rdi}}});
  }
  calling_convention_x86_64_common_end_proc(program, builder);
}

static Value *
calling_convention_x86_64_system_v_return_proc(
  const Allocator *allocator,
  const Function_Info *function,
  Function_Argument_Mode mode
) {
  if (function->returns.descriptor == &descriptor_void) {
    return value_init(
      allocator_allocate(allocator, Value),
      &descriptor_void,
      storage_none,
      function->returns.source_range
    );
  }
  static const Register general_registers[] = { Register_A, Register_D };
  static const Register vector_registers[] = { Register_Xmm0, Register_Xmm1 };

  System_V_Classification_State state = {
    .general_purpose_registers = {
      .items = general_registers,
      .count = countof(general_registers),
      .index = 0,
    },
    .vector_registers = {
      .items = vector_registers,
      .count = countof(vector_registers),
      .index = 0,
    },
    .memory_layout = {
      .items = dyn_array_make(
        Array_Memory_Layout_Item,
        .allocator = allocator_default,
        .capacity = 1,
      ),
    },
  };

  SYSTEM_V_ARGUMENT_CLASS class = x86_64_system_v_classify(
    allocator_default, &state, function->returns.name, function->returns.descriptor
  );
  Storage return_storage;
  const Descriptor *return_descriptor;
  if (class == SYSTEM_V_MEMORY) {
    Register base_register = mode == Function_Argument_Mode_Call ? Register_A : Register_DI;
    return_descriptor = function->returns.descriptor;
    return_storage = storage_indirect(descriptor_byte_size(return_descriptor), base_register);
  } else {
    assert(dyn_array_length(state.memory_layout.items) == 1);
    Memory_Layout_Item *item = dyn_array_get(state.memory_layout.items, 0);
    assert(item->tag == Memory_Layout_Item_Tag_Absolute);
    return_descriptor = item->descriptor;
    return_storage = item->Absolute.storage;
    dyn_array_destroy(state.memory_layout.items);
  }

  return value_init(
    allocator_allocate(allocator, Value),
    return_descriptor,
    return_storage,
    function->returns.source_range
  );
}

static Memory_Layout
calling_convention_x86_64_system_v_arguments_layout_proc(
  const Allocator *allocator,
  const Function_Info *function
) {
  static const Register general_registers[] = {
    Register_DI, Register_SI, Register_D, Register_C, Register_R8, Register_R9
  };
  static const Register vector_registers[] = {
    Register_Xmm0, Register_Xmm1, Register_Xmm2, Register_Xmm3,
    Register_Xmm4, Register_Xmm5, Register_Xmm6, Register_Xmm7,
  };

  // FIXME avoid doing this
  Value *return_value = calling_convention_x86_64_system_v_return_proc(
    allocator, function, Function_Argument_Mode_Body
  );

  bool is_indirect_return = (
    return_value->storage.tag == Storage_Tag_Memory &&
    return_value->storage.Memory.location.tag == Memory_Location_Tag_Indirect &&
    return_value->storage.Memory.location.Indirect.base_register == Register_DI
  );

  System_V_Classification_State state = {
    .general_purpose_registers = {
      .items = general_registers,
      .count = countof(general_registers),
      .index = is_indirect_return ? 1 : 0,
    },
    .vector_registers = {
      .items = vector_registers,
      .count = countof(vector_registers),
      .index = 0,
    },
    .memory_layout = {
      .items = dyn_array_make(
        Array_Memory_Layout_Item,
        .allocator = allocator,
        .capacity = dyn_array_length(function->arguments) + 1,
      ),
    },
  };

  DYN_ARRAY_FOREACH(Function_Argument, arg, function->arguments) {
    x86_64_system_v_classify(allocator, &state, arg->name, arg->descriptor);
  }

  if (is_indirect_return) {
    dyn_array_push(state.memory_layout.items, (Memory_Layout_Item) {
      .tag = Memory_Layout_Item_Tag_Absolute,
      .flags = Memory_Layout_Item_Flags_Uninitialized | Memory_Layout_Item_Flags_Implicit_Pointer,
      .name = {0}, // Defining return value name happens separately
      .descriptor = function->returns.descriptor,
      .source_range = function->returns.source_range,
      .Absolute = { .storage = return_value->storage, },
    });
  }

  return state.memory_layout;
}

static void
calling_convention_x86_64_windows_body_end_proc(
  Program *program,
  Function_Builder *builder
) {
  // :ReturnTypeLargerThanRegister
  if(descriptor_byte_size(builder->function->returns.descriptor) > 8) {
    push_instruction(&builder->code_block.instructions, builder->return_value->source_range,
      (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {rax, rcx}}});
  }
  calling_convention_x86_64_common_end_proc(program, builder);
}

static Value *
calling_convention_x86_64_windows_return_proc(
  const Allocator *allocator,
  const Function_Info *function,
  Function_Argument_Mode mode
) {
  const Descriptor *descriptor = function->returns.descriptor;
  if (descriptor == &descriptor_void) {
    return &void_value;
  }
  Storage storage;
  // TODO handle 16 byte non-float return values in XMM0
  if (descriptor_is_float(descriptor)) {
    storage = storage_register_for_descriptor(Register_Xmm0, descriptor);
  } else {
    u64 byte_size = descriptor_byte_size(descriptor);
    if (byte_size <= 8) {
      storage = storage_register_for_descriptor(Register_A, descriptor);
    } else {
      // :ReturnTypeLargerThanRegister
      // Inside the function large returns are pointed to by RCX,
      // but this pointer is also returned in A
      Register base_register = Register_A;
      if (mode == Function_Argument_Mode_Body) {
        base_register = Register_C;
      }
      storage = storage_indirect(byte_size, base_register);
    }
  }
  return value_init(
    allocator_allocate(allocator, Value), descriptor, storage, COMPILER_SOURCE_RANGE
  );
}

static Memory_Layout
calling_convention_x86_64_windows_arguments_layout_proc(
  const Allocator *allocator,
  const Function_Info *function
) {
  static const Register general_registers[] = {Register_C, Register_D, Register_R8, Register_R9};
  static const Register float_registers[] = {Register_Xmm0, Register_Xmm1, Register_Xmm2, Register_Xmm3};
  assert(countof(general_registers) == countof(float_registers));

  Memory_Layout layout = {
    .items = dyn_array_make(
      Array_Memory_Layout_Item,
      .allocator = allocator,
      .capacity = dyn_array_length(function->arguments) + 1,
    ),
  };

  // :ReturnTypeLargerThanRegister
  // If return type is larger than register, the pointer to stack location
  // where it needs to be written to is passed as the first argument
  // shifting registers for actual arguments by one
  u64 return_byte_size = descriptor_byte_size(function->returns.descriptor);
  bool is_return_larger_than_register = return_byte_size > 8;
  u64 index = is_return_larger_than_register ? 1 : 0;

  DYN_ARRAY_FOREACH(Function_Argument, arg, function->arguments) {
    Memory_Layout_Item item = {
      .flags = Memory_Layout_Item_Flags_None,
      .name = arg->name,
      .descriptor = arg->descriptor,
      .source_range = arg->source_range,
    };

    u64 byte_size = descriptor_byte_size(arg->descriptor);
    bool is_large_argument = byte_size > 8;
    Storage arg_storage;
    if (index < countof(general_registers)) {
      Register reg = descriptor_is_float(arg->descriptor)
        ? float_registers[index]
        : general_registers[index];
      if (is_large_argument) {
        // Large arguments are passed "by reference", i.e. their memory location in the register
        arg_storage = storage_indirect(byte_size, reg);
        item.flags |= Memory_Layout_Item_Flags_Implicit_Pointer;
      } else {
        arg_storage = storage_register_for_descriptor(reg, arg->descriptor);
      }
      item.tag = Memory_Layout_Item_Tag_Absolute;
      item.Absolute.storage = arg_storage;
    } else {
      item.tag = Memory_Layout_Item_Tag_Base_Relative;
      item.Base_Relative.offset = index * 8;
    }
    dyn_array_push(layout.items, item);
    index += 1;
  }

  if (is_return_larger_than_register) {
    dyn_array_push(layout.items, (Memory_Layout_Item) {
      .tag = Memory_Layout_Item_Tag_Absolute,
      .flags = Memory_Layout_Item_Flags_Uninitialized | Memory_Layout_Item_Flags_Implicit_Pointer,
      .name = {0}, // Defining return value name happens separately
      .descriptor = function->returns.descriptor,
      .source_range = function->returns.source_range,
      .Absolute = { .storage = storage_indirect(return_byte_size, Register_C), },
    });
  }

  return layout;
}
#endif

