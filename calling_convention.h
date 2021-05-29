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

static Storage
calling_convention_x86_64_windows_return_storage_proc(
  const Function_Info *function,
  Function_Argument_Mode mode
);

static const Calling_Convention calling_convention_x86_64_windows = {
  .body_end_proc = calling_convention_x86_64_windows_body_end_proc,
  .arguments_layout_proc = calling_convention_x86_64_windows_arguments_layout_proc,
  .return_storage_proc = calling_convention_x86_64_windows_return_storage_proc,
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

static Storage
calling_convention_x86_64_system_v_return_storage_proc(
  const Function_Info *function,
  Function_Argument_Mode mode
);

static const Calling_Convention calling_convention_x86_64_system_v = {
  .body_end_proc = calling_convention_x86_64_system_v_body_end_proc,
  .arguments_layout_proc = calling_convention_x86_64_system_v_arguments_layout_proc,
  .return_storage_proc = calling_convention_x86_64_system_v_return_storage_proc,
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
  s32 return_address_size = 8;
  builder->stack_reserve += builder->max_call_parameters_stack_size;
  builder->stack_reserve = s32_align(builder->stack_reserve, 16) + return_address_size;

  s32 argument_stack_base = builder->stack_reserve + return_address_size;
  // :RegisterPushPop
  // pushes change the stack pointer so we need to account for that
  for (s32 reg_index = Register_R15; reg_index >= Register_A; --reg_index) {
    if (register_bitset_get(builder->used_register_bitset, reg_index)) {
      if (!register_bitset_get(builder->register_volatile_bitset, reg_index)) {
        argument_stack_base += 8;
      }
    }
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
  Array_System_V_Class_Entry class_entry_stack;
  System_V_Registers general_purpose_registers;
  System_V_Registers vector_registers;
  u64 stack_offset;
} System_V_Classification_State;

static Storage
x86_64_system_v_storage_for_class(
  System_V_Classification_State *state,
  SYSTEM_V_ARGUMENT_CLASS class
) {
  switch(class) {
    case SYSTEM_V_NO_CLASS: {
      return storage_none;
    }
    case SYSTEM_V_INTEGER: {
      if (state->general_purpose_registers.index < state->general_purpose_registers.count) {
        Register reg = state->general_purpose_registers.items[state->general_purpose_registers.index++];
        return storage_register(reg, 8);
      } else {
        Storage result = storage_stack_argument(u64_to_s32(state->stack_offset), 8);
        state->stack_offset += 8;
        return result;
      }
    }
    case SYSTEM_V_SSE: {
      if (state->vector_registers.index < state->vector_registers.count) {
        Register reg = state->vector_registers.items[state->vector_registers.index++];
        return storage_register(reg, 8);
      } else {
        Storage result = storage_stack_argument(u64_to_s32(state->stack_offset), 8);
        state->stack_offset += 8;
        return result;
      }
      break;
    }
    case SYSTEM_V_MEMORY: {
      Storage result = storage_stack_argument(u64_to_s32(state->stack_offset), 8);
      state->stack_offset += 8;
      return result;
    }
    case SYSTEM_V_SSEUP:
    case SYSTEM_V_X87:
    case SYSTEM_V_X87UP:
    case SYSTEM_V_COMPLEX_X87: {
      panic("TODO");
      break;
    }
  }
  return storage_none;
}

static SYSTEM_V_ARGUMENT_CLASS
x86_64_system_v_classify(
  System_V_Classification_State *state,
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
        if (descriptor_is_float(descriptor)) {
          return SYSTEM_V_SSE;
        } else {
          return SYSTEM_V_INTEGER;
        }
      }
      panic("TODO support large opaque descriptors");
      break;
    }
    case Descriptor_Tag_Struct: {
      bool has_unaligned = false; // FIXME calculate this
      // 1. If the size of an object is larger than eight eightbytes,
      // or it contains unaligned fields, it has class MEMORY
      if (byte_size > 8 * eightbyte || has_unaligned) {
        return SYSTEM_V_MEMORY;
      }
      // 2. If a C++ object is non-trivial for the purpose of calls, as specified in the
      // C++ ABI 13, it is passed by invisible reference (the object is replaced in the
      // parameter list by a pointer that has class INTEGER)
      bool is_c_plus_plus_non_trivial = false; // TODO allow to specify this
      if (is_c_plus_plus_non_trivial) {
        return SYSTEM_V_INTEGER;
      }

      // 3. If the size of the aggregate exceeds a single eightbyte, each is classified
      // separately. Each eightbyte gets initialized to class NO_CLASS.
      SYSTEM_V_ARGUMENT_CLASS result = SYSTEM_V_NO_CLASS;
      u64 current_eightbyte_offset = 0;

      const Memory_Layout *layout = &descriptor->Struct.memory_layout;

      DYN_ARRAY_FOREACH(Memory_Layout_Item, item, layout->items) {
        const Descriptor *item_descriptor = item->descriptor;
        assert(item->tag == Memory_Layout_Item_Tag_Base_Relative);
        u64 offset = item->Base_Relative.offset;
        if (offset - current_eightbyte_offset >= eightbyte) {
          dyn_array_push(state->class_entry_stack, (System_V_Class_Entry) {
            .storage = x86_64_system_v_storage_for_class(state, result),
            .class = result,
          });
          result = SYSTEM_V_NO_CLASS;
          current_eightbyte_offset = offset;
          assert(current_eightbyte_offset % 8 == 0);
        }
        SYSTEM_V_ARGUMENT_CLASS current = x86_64_system_v_classify(state, item_descriptor);
        if (result == current) {
          continue;
        } else if (current == SYSTEM_V_NO_CLASS) {
          continue;
        } else if (result == SYSTEM_V_NO_CLASS) {
          result = current;
        } else if (result == SYSTEM_V_MEMORY || current == SYSTEM_V_MEMORY) {
          result = SYSTEM_V_MEMORY;
        } else if (result == SYSTEM_V_INTEGER || current == SYSTEM_V_INTEGER) {
          result = SYSTEM_V_INTEGER;
        } else if (
          result == SYSTEM_V_X87 || result == SYSTEM_V_X87UP || result == SYSTEM_V_COMPLEX_X87 ||
          current == SYSTEM_V_X87 || current == SYSTEM_V_X87UP || current == SYSTEM_V_COMPLEX_X87
        ) {
          result = SYSTEM_V_MEMORY;
        } else {
          result = SYSTEM_V_SSE;
        }
      }
      return result;
      // TODO implement post-merger cleanup
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
  // :ReturnTypeLargerThanRegister
  if(descriptor_byte_size(builder->function->returns.descriptor) > 8) {
    push_instruction(&builder->code_block.instructions, builder->return_value->source_range,
      (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {rax, rdi}}});
  }
  calling_convention_x86_64_common_end_proc(program, builder);
}

static Storage
calling_convention_x86_64_system_v_return_storage_proc(
  const Function_Info *function,
  Function_Argument_Mode mode
) {
  const Descriptor *descriptor = function->returns.descriptor;
  if (descriptor == &descriptor_void) {
    return storage_none;
  }
  // TODO handle 16 byte non-float return values in XMM0
  if (descriptor_is_float(descriptor)) {
    return storage_register_for_descriptor(Register_Xmm0, descriptor);
  }
  u64 byte_size = descriptor_byte_size(descriptor);
  if (byte_size <= 8) {
    return storage_register_for_descriptor(Register_A, descriptor);
  }
  // :ReturnTypeLargerThanRegister
  // Inside the function large returns are pointed to by RDI,
  // but this pointer is also returned in A
  Register base_register = Register_A;
  if (mode == Function_Argument_Mode_Body) {
    base_register = Register_DI;
  }
  return storage_indirect(byte_size, base_register);
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

  // :ReturnTypeLargerThanRegister
  // If return type is larger than register, the pointer to stack location
  // where it needs to be written to is passed as the first argument
  // shifting registers for actual arguments by one
  u64 return_byte_size = descriptor_byte_size(function->returns.descriptor);
  bool is_return_larger_than_register = return_byte_size > 8;

  System_V_Classification_State state = {
    .general_purpose_registers = {
      .items = general_registers,
      .count = countof(general_registers),
      .index = is_return_larger_than_register ? 1 : 0,
    },
    .vector_registers = {
      .items = vector_registers,
      .count = countof(vector_registers),
      .index = 0,
    },
    .class_entry_stack = dyn_array_make(Array_System_V_Class_Entry),
  };

  Memory_Layout layout = {
    .base = storage_stack_argument(0, 1),
    .items = dyn_array_make(
      Array_Memory_Layout_Item,
      .allocator = allocator,
      .capacity = dyn_array_length(function->arguments) + 1,
    ),
  };

  DYN_ARRAY_FOREACH(Function_Argument, arg, function->arguments) {
    dyn_array_clear(state.class_entry_stack);
    SYSTEM_V_ARGUMENT_CLASS class = x86_64_system_v_classify(&state, arg->descriptor);
    dyn_array_push(state.class_entry_stack, (System_V_Class_Entry) {
      .storage = x86_64_system_v_storage_for_class(&state, class),
      .class = class,
    });
    DYN_ARRAY_FOREACH(System_V_Class_Entry, entry, state.class_entry_stack) {
      Memory_Layout_Item item = {
        .flags = Memory_Layout_Item_Flags_None,
        .name = arg->name,
        .descriptor = arg->descriptor,
        .source_range = arg->source_range,
      };

      Storage arg_storage = dyn_array_get(state.class_entry_stack, 0)->storage;
      arg_storage.byte_size = descriptor_byte_size(arg->descriptor); // FIXME this is wrong
      if (arg_storage.tag == Storage_Tag_Memory) {
        assert(arg_storage.Memory.location.tag == Memory_Location_Tag_Stack);
        item.tag = Memory_Layout_Item_Tag_Base_Relative;
        item.Base_Relative.offset = arg_storage.Memory.location.Stack.offset;
      } else {
        item.tag = Memory_Layout_Item_Tag_Absolute;
        item.Absolute.storage = arg_storage;
      }
      dyn_array_push(layout.items, item);
    }
  }

  if (is_return_larger_than_register) {
    dyn_array_push(layout.items, (Memory_Layout_Item) {
      .tag = Memory_Layout_Item_Tag_Absolute,
      .flags = Memory_Layout_Item_Flags_Uninitialized,
      .name = {0}, // Defining return value name happens separately
      .descriptor = function->returns.descriptor,
      .source_range = function->returns.source_range,
      .Absolute = { .storage = storage_indirect(return_byte_size, Register_DI), },
    });
  }
  dyn_array_destroy(state.class_entry_stack);

  return layout;
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

static Storage
calling_convention_x86_64_windows_return_storage_proc(
  const Function_Info *function,
  Function_Argument_Mode mode
) {
  const Descriptor *descriptor = function->returns.descriptor;
  if (descriptor == &descriptor_void) {
    return storage_none;
  }
  // TODO handle 16 byte non-float return values in XMM0
  if (descriptor_is_float(descriptor)) {
    return storage_register_for_descriptor(Register_Xmm0, descriptor);
  }
  u64 byte_size = descriptor_byte_size(descriptor);
  if (byte_size <= 8) {
    return storage_register_for_descriptor(Register_A, descriptor);
  }
  // :ReturnTypeLargerThanRegister
  // Inside the function large returns are pointed to by RCX,
  // but this pointer is also returned in A
  Register base_register = Register_A;
  if (mode == Function_Argument_Mode_Body) {
    base_register = Register_C;
  }
  return storage_indirect(byte_size, base_register);
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
    .base = storage_stack_argument(0, 1),
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
      .flags = Memory_Layout_Item_Flags_Uninitialized,
      .name = {0}, // Defining return value name happens separately
      .descriptor = function->returns.descriptor,
      .source_range = function->returns.source_range,
      .Absolute = { .storage = storage_indirect(return_byte_size, Register_C), },
    });
  }

  return layout;
}
#endif

