#ifndef CALLING_CONVENTION_H
#define CALLING_CONVENTION_H

#include "types.h"

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
  static const Register float_registers[] = {
    Register_Xmm0, Register_Xmm1, Register_Xmm2, Register_Xmm3,
    Register_Xmm4, Register_Xmm5, /* Register_Xmm6, Register_Xmm7, */
  };
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
    Memory_Layout_Item_Flags flags = Memory_Layout_Item_Flags_None;

    Memory_Layout_Item item = {
      .flags = flags,
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
      .Absolute = { .storage = storage_indirect(return_byte_size, Register_DI), },
    });
  }

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
    Memory_Layout_Item_Flags flags = Memory_Layout_Item_Flags_None;

    Memory_Layout_Item item = {
      .flags = flags,
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

