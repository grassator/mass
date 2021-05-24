#include "function.h"

static Storage
reserve_stack_storage(
  Function_Builder *builder,
  u64 raw_byte_size
) {
  s32 byte_size = u64_to_s32(raw_byte_size);
  builder->stack_reserve = s32_align(builder->stack_reserve, byte_size);
  builder->stack_reserve += byte_size;
  return stack(-builder->stack_reserve, byte_size);
}

// :StackDisplacementEncoding
// There are three types of values that can be present on the stack in the current setup
// 1) Parameters to the current function. In Win32 ABI that is every parameter past 4th.
//    these values are temporarily stored in the Storage as positive integers with values
//    larger than max_call_parameters_stack_size. Offsets of these are adjusted in
//    fn_adjust_stack_displacement.
// 2) Temporary values that are used for stack arguments for function to other functions for
//    the same cases as above.
// 3) Locals. They need to physically located in memory after the 2), but untill we know
//    all the function calls within the body we do not know the max_call_parameters_stack_size
//    so during initial encoding they are stored as negative numbers and then adjusted in
//    fn_adjust_stack_displacement.
Value *
reserve_stack_internal(
  Compiler_Source_Location compiler_source_location,
  Execution_Context *context,
  Function_Builder *builder,
  const Descriptor *descriptor,
  Source_Range source_range
) {
  Storage storage = reserve_stack_storage(builder, descriptor_byte_size(descriptor));
  return value_make_internal(
    compiler_source_location, context, descriptor, storage, source_range
  );
}

#define reserve_stack(...)\
  reserve_stack_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

static inline Register
register_acquire(
  Function_Builder *builder,
  Register reg_index
) {
  assert(!register_bitset_get(builder->register_occupied_bitset, reg_index));
  register_bitset_set(&builder->used_register_bitset, reg_index);
  register_bitset_set(&builder->register_occupied_bitset, reg_index);
  return reg_index;
}

static Register
register_find_available(
  Function_Builder *builder
) {
  // FIXME We are skipping Register_A here as it is hardcoded in quite a few places still
  static const Register temp_registers[] = {
    Register_C, Register_B, Register_D, Register_R8, Register_R9, Register_R10,
    Register_R11, Register_R12, Register_R13, Register_R14, Register_R15,
  };
  for (u32 i = 0; i < countof(temp_registers); ++i) {
    Register reg_index = temp_registers[i];
    if (!register_bitset_get(builder->register_occupied_bitset, reg_index)) {
      return reg_index;
    }
  }
  // FIXME
  panic("Could not acquire a temp register");
  return -1;
}

static inline Register
register_acquire_temp(
  Function_Builder *builder
) {
  return register_acquire(builder, register_find_available(builder));
}

static inline void
ensure_register_released(
  Function_Builder *builder,
  Register reg_index
) {
  register_bitset_unset(&builder->register_occupied_bitset, reg_index);
  builder->register_occupied_values[reg_index] = 0;
}

void
register_release(
  Function_Builder *builder,
  Register reg_index
) {
  assert(register_bitset_get(builder->register_occupied_bitset, reg_index));
  ensure_register_released(builder, reg_index);
}


typedef struct {
  const Source_Range *source_range;
  Register index;
  Register saved_index;
  bool saved;
} Maybe_Saved_Register;

Maybe_Saved_Register
register_acquire_maybe_save_if_already_acquired(
  Allocator *allocator,
  Function_Builder *builder,
  const Source_Range *source_range,
  Register reg_index
) {
  Maybe_Saved_Register result = {
    .saved = false,
    .source_range = source_range,
    .index = reg_index,
  };
  if (!register_bitset_get(builder->register_occupied_bitset, reg_index)) {
    register_acquire(builder, reg_index);
    return result;
  }

  result.saved_index = register_acquire_temp(builder);
  result.saved = true;

  push_instruction(
    &builder->code_block.instructions, *source_range,
    (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {
      storage_register_for_descriptor(result.saved_index, &descriptor_s64),
      storage_register_for_descriptor(reg_index, &descriptor_s64)
    }}}
  );

  return result;
}

void
register_release_maybe_restore(
  Function_Builder *builder,
  const Maybe_Saved_Register *maybe_saved_register
) {
  if (maybe_saved_register->saved) {
    push_instruction(
      &builder->code_block.instructions, *maybe_saved_register->source_range,
      (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {
        storage_register_for_descriptor(maybe_saved_register->index, &descriptor_s64),
        storage_register_for_descriptor(maybe_saved_register->saved_index, &descriptor_s64),
      }}}
    );
    register_release(builder, maybe_saved_register->saved_index);
  } else {
    register_release(builder, maybe_saved_register->index);
  }
}

void
move_value(
  Allocator *allocator,
  Function_Builder *builder,
  const Source_Range *source_range,
  const Storage *target,
  const Storage *source
) {
  Array_Instruction *instructions = &builder->code_block.instructions;
  if (target == source) return;
  if (storage_equal(target, source)) return;

  if (target->tag == Storage_Tag_Eflags) {
    panic("Internal Error: Trying to move into Eflags");
  }

  u64 target_size = target->byte_size;
  u64 source_size = source->byte_size;

  if (target->tag == Storage_Tag_Xmm || source->tag == Storage_Tag_Xmm) {
    assert(target_size == source_size);
    if (target_size == 4) {
      push_instruction(instructions, *source_range,
        (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {movss, {*target, *source, 0}}});
    } else if (target_size == 8) {
      push_instruction(instructions, *source_range,
        (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {movsd, {*target, *source, 0}}});
    } else {
      panic("Internal Error: XMM operand of unexpected size");
    }
    return;
  }

  if (source->tag == Storage_Tag_Eflags) {
    assert(storage_is_register_or_memory(target));
    Storage temp = *target;
    if (target->byte_size != 1) {
      temp = (Storage) {
        .tag = Storage_Tag_Register,
        .byte_size = 1,
        .Register.index = register_acquire_temp(builder),
      };
    }
    switch(source->Eflags.compare_type) {
      case Compare_Type_Equal: {
        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {sete, {temp, *source}}});
        break;
      }
      case Compare_Type_Not_Equal: {
        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {setne, {temp, *source}}});
        break;
      }

      case Compare_Type_Unsigned_Below: {
        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {setb, {temp, *source}}});
        break;
      }
      case Compare_Type_Unsigned_Below_Equal: {
        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {setbe, {temp, *source}}});
        break;
      }
      case Compare_Type_Unsigned_Above: {
        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {seta, {temp, *source}}});
        break;
      }
      case Compare_Type_Unsigned_Above_Equal: {
        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {setae, {temp, *source}}});
        break;
      }

      case Compare_Type_Signed_Less: {
        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {setl, {temp, *source}}});
        break;
      }
      case Compare_Type_Signed_Less_Equal: {
        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {setle, {temp, *source}}});
        break;
      }
      case Compare_Type_Signed_Greater: {
        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {setg, {temp, *source}}});
        break;
      }
      case Compare_Type_Signed_Greater_Equal: {
        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {setge, {temp, *source}}});
        break;
      }
      default: {
        assert(!"Unsupported comparison");
      }
    }
    if (!storage_equal(&temp, target)) {
      assert(temp.tag == Storage_Tag_Register);
      Storage resized_temp = temp;
      resized_temp.byte_size = target->byte_size;
      push_instruction(instructions, *source_range,
        (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {movsx, {resized_temp, temp}}});
      push_instruction(instructions, *source_range,
        (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {*target, resized_temp}}});
      register_release(builder, temp.Register.index);
    }
    return;
  }

  if (source->tag == Storage_Tag_Static) {
    if (source->byte_size > 8) {
      // TODO use XMM or 64 bit registers where appropriate
      // TODO support packed structs
      static const u64 chunk_size = 4;
      assert(source->byte_size % chunk_size == 0);
      // TODO can there be something else?
      assert(target->tag == Storage_Tag_Memory);
      Memory_Location location = target->Memory.location;
      // TODO support other ones
      assert(location.tag == Memory_Location_Tag_Indirect);
      assert(source->byte_size == target->byte_size);
      for (u64 offset = 0; offset < source->byte_size; offset += chunk_size) {
        Storage adjusted_target = *target;
        adjusted_target.byte_size = chunk_size;
        adjusted_target.Memory.location.Indirect.offset += offset;
        assert(source->Static.memory.tag == Static_Memory_Tag_Heap);
        void *memory = (s8 *)source->Static.memory.Heap.pointer + offset;
        Storage adjusted_source = storage_static_internal(memory, chunk_size);
        push_instruction(
          instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {adjusted_target, adjusted_source}}}
        );
      }
      return;
    }
    s64 immediate = storage_static_value_up_to_s64(source);
    if (immediate == 0 && target->tag == Storage_Tag_Register) {
      // This messes up flags register so comparisons need to be aware of this optimization
      push_instruction(instructions, *source_range,
        (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {xor, {*target, *target}}});
      return;
    }
    Storage adjusted_source;
    switch(target_size) {
      case 1: {
        adjusted_source = imm8(s64_to_s8(immediate));
        break;
      }
      case 2: {
        adjusted_source = imm16(s64_to_s16(immediate));
        break;
      }
      case 4: {
        adjusted_source = imm32(s64_to_s32(immediate));
        break;
      }
      case 8: {
        // FIXME This does sign extension so will be broken for unsigned
        if (s64_fits_into_s32(immediate)) {
          adjusted_source = imm32(s64_to_s32(immediate));
        } else {
          adjusted_source = imm64(immediate);
        }
        break;
      }
      default: {
        panic("Unexpected integer size");
        adjusted_source = (Storage){0};
        break;
      }
    }
    // Because of 15 byte instruction limit on x86 there is no way to move 64bit immediate
    // to a memory location. In which case we do a move through a temp register
    bool is_64bit_immediate = adjusted_source.byte_size == 8;
    if (is_64bit_immediate && target->tag != Storage_Tag_Register) {
      Storage temp = {
        .tag = Storage_Tag_Register,
        .byte_size = adjusted_source.byte_size,
        .Register.index = register_acquire_temp(builder),
      };
      push_instruction(instructions, *source_range,
        (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {temp, adjusted_source}}});
      push_instruction(instructions, *source_range,
        (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {*target, temp}}});
      register_release(builder, temp.Register.index);
    } else {
      push_instruction(instructions, *source_range,
        (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {*target, adjusted_source}}});
    }
    return;
  }

  // TODO figure out more type checking

  if (target_size != source_size) {
    if (source_size < target_size) {
      // TODO deal with unsigned numbers
      if (target->tag == Storage_Tag_Register) {
        if (source_size == 4) {
          // TODO check whether this correctly sign extends
          Storage adjusted_target = {
            .tag = Storage_Tag_Register,
            .Register = target->Register,
            .byte_size = 4,
          };
          push_instruction(instructions, *source_range,
            (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {adjusted_target, *source}}});
        } else {
          push_instruction(instructions, *source_range,
            (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {movsx, {*target, *source}}});
        }
      } else {
        Storage temp = {
          .tag = Storage_Tag_Register,
          .byte_size = target->byte_size,
          .Register.index = register_acquire_temp(builder),
        };
        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {movsx, {temp, *source}}});
        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {*target, temp}}});
        register_release(builder, temp.Register.index);
      }
      return;
    } else {
      print_operand(target);
      printf(" ");
      print_operand(source);
      printf("\nat ");
      source_range_print_start_position(source_range);
      assert(!"Mismatched operand size when moving");
    }
  }

  if (target->tag == Storage_Tag_Memory && source->tag == Storage_Tag_Memory) {
    if (target_size >= 16) {
      // TODO probably can use larger chunks for copying but need to check alignment
      Storage temp_rsi = storage_register_for_descriptor(register_acquire_temp(builder), &descriptor_s64);
      Storage temp_rdi = storage_register_for_descriptor(register_acquire_temp(builder), &descriptor_s64);
      Storage temp_rcx = storage_register_for_descriptor(register_acquire_temp(builder), &descriptor_s64);
      {
        Storage reg_rsi = storage_register_for_descriptor(Register_SI, &descriptor_s64);
        Storage reg_rdi = storage_register_for_descriptor(Register_DI, &descriptor_s64);
        Storage reg_rcx = storage_register_for_descriptor(Register_C, &descriptor_s64);
        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {temp_rsi, reg_rsi}}});
        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {temp_rdi, reg_rdi}}});
        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {temp_rcx, reg_rcx}}});

        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {lea, {reg_rsi, *source}}});
        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {lea, {reg_rdi, *target}}});
        Storage size_operand = imm64(target_size);
        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {reg_rcx, size_operand}}});
        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {rep_movsb}});

        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {reg_rsi, temp_rsi}}});
        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {reg_rdi, temp_rdi}}});
        push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {reg_rcx, temp_rcx}}});
      }
      register_release(builder, temp_rsi.Register.index);
      register_release(builder, temp_rdi.Register.index);
      register_release(builder, temp_rcx.Register.index);
    } else {
      Storage temp = {
        .tag = Storage_Tag_Register,
        .byte_size = target->byte_size,
        .Register.index = register_acquire_temp(builder),
      };
      move_value(allocator, builder, source_range, &temp, source);
      move_value(allocator, builder, source_range, target, &temp);
      register_release(builder, temp.Register.index);
    }
    return;
  }

  push_instruction(instructions, *source_range,
    (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {*target, *source}}});
}

// FIXME Get rid of this function
void
move_to_result_from_temp(
  Allocator *allocator,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *target,
  Value *temp_source
) {
  if (target->storage.tag == Storage_Tag_Any) {
    target->storage = temp_source->storage;
    return;
  }
  // Sometimes it is convenient to use result as temp in which case there
  // is no need for a move or a register release
  if (!storage_equal(&target->storage, &temp_source->storage)) {
    move_value(allocator, builder, source_range, &target->storage, &temp_source->storage);
    if (temp_source->storage.tag == Storage_Tag_Register) {
      register_release(builder, temp_source->storage.Register.index);
    }
  }
}

// :StackDisplacementEncoding
s64
fn_adjust_stack_displacement(
  const Function_Builder *builder,
  s64 displacement
) {
  // Negative diplacement is used to encode local variables
  if (displacement < 0) {
    displacement += builder->stack_reserve;
  } else
  // Positive values larger than max_call_parameters_stack_size
  // are for arguments to this function on the stack
  if (displacement >= u32_to_s64(builder->max_call_parameters_stack_size)) {
    // Return address will be pushed on the stack by the caller
    // and we need to account for that
    s32 return_address_size = 8;
    displacement += builder->stack_reserve + return_address_size;
  }
  return displacement;
}

void
fn_normalize_instruction_operands(
  Program *program,
  const Function_Builder *builder,
  Instruction *instruction
) {
  // :StorageNormalization
  // Normalizing operands to simplify future handling in the encoder
  for (u8 storage_index = 0; storage_index < countof(instruction->Assembly.operands); ++storage_index) {
    Storage *storage = &instruction->Assembly.operands[storage_index];
    if (
      storage->tag == Storage_Tag_Memory &&
      storage->Memory.location.tag == Memory_Location_Tag_Indirect &&
      storage->Memory.location.Indirect.base_register == Register_SP
    ) {
      storage->Memory.location.Indirect.offset =
        fn_adjust_stack_displacement(builder, storage->Memory.location.Indirect.offset);
    }
  }
}

// TODO generalize for all jumps to jumps
void
fn_maybe_remove_unnecessary_jump_from_return_statement_at_the_end_of_function(
  Function_Builder *builder
) {
  Instruction *last_instruction = dyn_array_last(builder->code_block.instructions);
  if (!last_instruction) return;
  if (last_instruction->tag != Instruction_Tag_Assembly) return;
  if (last_instruction->Assembly.mnemonic != jmp) return;
  Storage storage = last_instruction->Assembly.operands[0];
  if (!storage_is_label(&storage)) return;
  if (storage.Memory.location.Instruction_Pointer_Relative.label_index.value
    != builder->code_block.end_label.value) return;
  dyn_array_pop(builder->code_block.instructions);
}

void
fn_end(
  Program *program,
  Function_Builder *builder
) {
  assert(!builder->frozen);

  u8 alignment = 0x8;
  builder->stack_reserve += builder->max_call_parameters_stack_size;
  builder->stack_reserve = s32_align(builder->stack_reserve, 16) + alignment;

  for (u64 i = 0; i < dyn_array_length(builder->code_block.instructions); ++i) {
    Instruction *instruction = dyn_array_get(builder->code_block.instructions, i);
    fn_normalize_instruction_operands(program, builder, instruction);
  }
  fn_maybe_remove_unnecessary_jump_from_return_statement_at_the_end_of_function(builder);

  builder->frozen = true;
}

u32
make_trampoline(
  Program *program,
  Virtual_Memory_Buffer *buffer,
  s64 address
) {
  u32 result = u64_to_u32(buffer->occupied);
  encode_instruction_with_compiler_location(
    program, buffer,
    &(Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {rax, imm64(address)}}}
  );
  encode_instruction_with_compiler_location(
    program, buffer,
    &(Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {jmp, {rax}}}
  );
  return result;
}

typedef struct {
  u8 register_index;
  u8 offset_in_prolog;
} Function_Pushed_Register;

void
fn_encode(
  Program *program,
  Virtual_Memory_Buffer *buffer,
  const Function_Builder *builder,
  Function_Layout *out_layout
) {
  // FIXME move to the callers and turn into an assert
  if (builder->function->flags & Descriptor_Function_Flags_Macro) {
    // We should not encode macro functions. And we might not even to be able to anyway
    // as some of them have Any arguments or returns
    return;
  }
  // Macro functions do not have own stack and do not need freezing
  assert(builder->frozen);

  *out_layout = (Function_Layout) {
    .stack_reserve = builder->stack_reserve,
  };

  Label_Index label_index = builder->code_block.start_label;

  Label *label = program_get_label(program, label_index);

  // Calls to `fn_encode` do not do anything if we already encoded
  if (label->resolved) return;

  s64 code_base_rva = label->section->base_rva;
  out_layout->begin_rva = u64_to_u32(code_base_rva + buffer->occupied);
  Storage stack_size_operand = imm_auto_8_or_32(out_layout->stack_reserve);
  encode_instruction_with_compiler_location(
    program, buffer, &(Instruction) {
      .tag = Instruction_Tag_Label,
      .Label.index = label_index
    }
  );

  // :RegisterPushPop
  // :Win32UnwindCodes Must match what happens in the unwind code generation
  // Push non-volatile registers (in reverse order)
  u8 push_index = 0;
  for (s32 reg_index = Register_R15; reg_index >= Register_A; --reg_index) {
    if (register_bitset_get(builder->used_register_bitset, reg_index)) {
      if (!register_bitset_get(builder->register_volatile_bitset, reg_index)) {
        out_layout->volatile_register_push_offsets[push_index++] =
          u64_to_u8(code_base_rva + buffer->occupied - out_layout->begin_rva);
        Storage to_save = storage_register_for_descriptor(reg_index, &descriptor_s64);
        encode_instruction_with_compiler_location(
          program, buffer,
          &(Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {push, {to_save}}}
        );
      }
    }
  }

  encode_instruction_with_compiler_location(
    program, buffer,
    &(Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {sub, {rsp, stack_size_operand}}}
  );
  out_layout->stack_allocation_offset_in_prolog =
    u64_to_u8(code_base_rva + buffer->occupied -out_layout->begin_rva);
  out_layout->size_of_prolog =
    u64_to_u8(code_base_rva + buffer->occupied - out_layout->begin_rva);

  for (u64 i = 0; i < dyn_array_length(builder->code_block.instructions); ++i) {
    Instruction *instruction = dyn_array_get(builder->code_block.instructions, i);
    encode_instruction(program, buffer, instruction);
  }

  encode_instruction_with_compiler_location(
    program, buffer, &(Instruction) {
      .tag = Instruction_Tag_Label, .Label.index = builder->code_block.end_label
    }
  );

  // :ReturnTypeLargerThanRegister
  if(descriptor_byte_size(builder->function->returns.descriptor) > 8) {
    // FIXME :RegisterAllocation
    //       make sure that return value is always available in RCX at this point
    encode_instruction_with_compiler_location(
      program, buffer,
      &(Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {rax, rcx}}}
    );
  }

  encode_instruction_with_compiler_location(
    program, buffer,
    &(Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {add, {rsp, stack_size_operand}}}
  );

  // :RegisterPushPop
  // Pop non-volatile registers (in original order)
  for (Register reg_index = 0; reg_index <= Register_R15; ++reg_index) {
    if (register_bitset_get(builder->used_register_bitset, reg_index)) {
      if (!register_bitset_get(builder->register_volatile_bitset, reg_index)) {
        Storage to_save = storage_register_for_descriptor(reg_index, &descriptor_s64);
        encode_instruction_with_compiler_location(
          program, buffer, &(Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {pop, {to_save}}}
        );
      }
    }
  }

  encode_instruction_with_compiler_location(program, buffer,
    &(Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {ret, {0}}});
  out_layout->end_rva = u64_to_u32(code_base_rva + buffer->occupied);

  encode_instruction_with_compiler_location(program, buffer,
    &(Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {int3, {0}}});
}

Value *
function_return_value_for_descriptor(
  Execution_Context *context,
  const Descriptor *descriptor,
  Function_Argument_Mode mode,
  Source_Range source_range
) {
  if (descriptor == &descriptor_void) {
    return &void_value;
  }
  // TODO handle 16 byte non-float return values in XMM0
  if (descriptor_is_float(descriptor)) {
    Storage storage = storage_register_for_descriptor(Register_Xmm0, descriptor);
    return value_make(context, descriptor, storage, source_range);
  }
  u64 byte_size = descriptor_byte_size(descriptor);
  if (byte_size <= 8) {
    Storage storage = storage_register_for_descriptor(Register_A, descriptor);
    return value_make(context, descriptor, storage, source_range);
  }
  // :ReturnTypeLargerThanRegister
  // Inside the function large returns are pointed to by RCX,
  // but this pointer is also returned in A
  Register base_register = Register_A;
  if (mode == Function_Argument_Mode_Body) {
    base_register = Register_C;
  }
  Storage storage = {
    .tag = Storage_Tag_Memory,
    .byte_size = byte_size,
    .Memory.location = {
      .tag = Memory_Location_Tag_Indirect,
      .Indirect = {
        .base_register = base_register,
      }
    }
  };
  return value_make(context, descriptor, storage, source_range);
}

Label_Index
make_if(
  Execution_Context *context,
  Array_Instruction *instructions,
  const Source_Range *source_range,
  Value *value
) {
  Program *program = context->program;
  bool is_always_true = false;
  Label_Index label = make_label(program, &program->memory.code, slice_literal("if"));
  if(value->storage.tag == Storage_Tag_Static) {
    s64 imm = storage_static_value_up_to_s64(&value->storage);
    if (imm == 0) return label;
    is_always_true = true;
  }

  if (!is_always_true) {
    if (value->storage.tag == Storage_Tag_Eflags) {
      switch(value->storage.Eflags.compare_type) {
        case Compare_Type_Equal: {
          push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {jne, {code_label32(label), value->storage, 0}}});
          break;
        }
        case Compare_Type_Not_Equal: {
          push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {je, {code_label32(label), value->storage, 0}}});
          break;
        }

        case Compare_Type_Unsigned_Below: {
          push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {jae, {code_label32(label), value->storage, 0}}});
          break;
        }
        case Compare_Type_Unsigned_Below_Equal: {
          push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {ja, {code_label32(label), value->storage, 0}}});
          break;
        }
        case Compare_Type_Unsigned_Above: {
          push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {jbe, {code_label32(label), value->storage, 0}}});
          break;
        }
        case Compare_Type_Unsigned_Above_Equal: {
          push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {jb, {code_label32(label), value->storage, 0}}});
          break;
        }

        case Compare_Type_Signed_Less: {
          push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {jge, {code_label32(label), value->storage, 0}}});
          break;
        }
        case Compare_Type_Signed_Less_Equal: {
          push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {jg, {code_label32(label), value->storage, 0}}});
          break;
        }
        case Compare_Type_Signed_Greater: {
          push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {jle, {code_label32(label), value->storage, 0}}});
          break;
        }
        case Compare_Type_Signed_Greater_Equal: {
          push_instruction(instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {jl, {code_label32(label), value->storage, 0}}});
          break;
        }
        default: {
          assert(!"Unsupported comparison");
        }
      }
    } else {
      Storage test_temp = value->storage;
      if (test_temp.tag == Storage_Tag_Register) {
        push_instruction(
          instructions, *source_range,
          (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {x64_test, {test_temp, test_temp, 0}}}
        );
      } else {
        u64 byte_size = descriptor_byte_size(value->descriptor);
        if (byte_size == 4 || byte_size == 8) {
          push_instruction(
            instructions, *source_range,
            (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {cmp, {value->storage, imm32(0), 0}}}
          );
        } else if (byte_size == 1) {
          push_instruction(
            instructions, *source_range,
            (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {cmp, {value->storage, imm8(0), 0}}}
          );
        } else {
          assert(!"Unsupported value inside `if`");
        }
      }
      Value *eflags = value_from_compare(context, Compare_Type_Equal, *source_range);
      push_instruction(instructions, *source_range,
        (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {jz, {code_label32(label), eflags->storage, 0}}});
    }
  }
  return label;
}

Value *
maybe_constant_fold_internal(
  Execution_Context *context,
  Function_Builder *builder,
  s64 constant_result,
  const Expected_Result *expected_result,
  const Source_Range *source_range
) {
  const Descriptor *descriptor = expected_result_descriptor(expected_result);
  if (!descriptor) descriptor = &descriptor_s64;
  Storage imm_storage;
  switch(descriptor_byte_size(descriptor)) {
    case 1: imm_storage = imm8(s64_to_s8(constant_result)); break;
    case 2: imm_storage = imm16(s64_to_s16(constant_result)); break;
    case 4: imm_storage = imm32(s64_to_s32(constant_result)); break;
    case 8: imm_storage = imm64(s64_to_s64(constant_result)); break;
    default: imm_storage = (Storage){0}; panic("Unexpected operand size"); break;
  }
  Value *imm_value = value_make(context, descriptor, imm_storage, *source_range);
  return expected_result_ensure_value_or_temp(context, builder, expected_result, imm_value);
}

static void
load_address(
  Execution_Context *context,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *result_value,
  Storage source
) {
  // FIXME enable this check when the reference args are more explicit in the call code
  //assert(result_value->descriptor->tag == Descriptor_Tag_Pointer_To);
  assert(source.tag == Storage_Tag_Memory);

  Value *temp_register = result_value->storage.tag == Storage_Tag_Register
    ? result_value
    : value_register_for_descriptor(
        context, register_acquire_temp(builder), result_value->descriptor, *source_range
    );

  // TODO rethink operand sizing
  // We need to manually adjust the size here because even if we loading one byte
  // the right side is treated as an opaque address and does not participate in
  // instruction encoding.
  source.byte_size = descriptor_byte_size(result_value->descriptor);

  push_instruction(
    &builder->code_block.instructions, *source_range,
    (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {lea, {temp_register->storage, source, 0}}}
  );

  move_to_result_from_temp(
    context->allocator, builder, source_range, result_value, temp_register
  );
}

static Storage
ensure_compiled_function_body(
  Execution_Context *context,
  Value *fn_value
) {
  if (fn_value->descriptor->tag == Descriptor_Tag_Function_Instance) {
    return fn_value->storage;
  }
  const Descriptor *descriptor = fn_value->descriptor;
  assert(descriptor == &descriptor_function_literal);
  // TODO figure out how to avoid the const cast here
  Function_Literal *literal = (Function_Literal *)storage_static_as_c_type(&fn_value->storage, Function_Literal);
  const Function_Info *function = literal->info;

  assert(!(function->flags & Descriptor_Function_Flags_Macro));
  Storage *label_storage = context_is_compile_time_eval(context)
    ? &literal->compile_time_storage
    : &literal->runtime_storage;

  if (label_storage->tag != Storage_Tag_None) return *label_storage;

  if (value_is_external_symbol(literal->body)) {
    const External_Symbol *symbol = storage_static_as_c_type(&literal->body->storage, External_Symbol);
    *label_storage = import_symbol(context, symbol->library_name, symbol->symbol_name);
    return *label_storage;
  }

  Program *program = context->program;

  Slice fn_name = fn_value->descriptor->name.length
    ? fn_value->descriptor->name
    : slice_literal("anonymous_function");

  Label_Index call_label = make_label(program, &program->memory.code, fn_name);
  // It is important to cache the label here for recursive calls
  *label_storage = code_label32(call_label);

  Function_Builder *builder = &(Function_Builder){
    .function = function,
    .register_volatile_bitset = program->platform_info.register_volatile_bitset,
    .code_block = {
      .start_label = call_label,
      // FIXME use fn_value->descriptor->name
      .end_label = make_label(program, &program->memory.code, slice_literal("fn end")),
      .instructions = dyn_array_make(Array_Instruction, .allocator = context->allocator),
    },
  };

  Execution_Context body_context = *context;
  Scope *body_scope = scope_make(context->allocator, function->scope);
  body_context.flags &= ~Execution_Context_Flags_Global;
  body_context.scope = body_scope;
  body_context.epoch = get_new_epoch();

  {
    Memory_Layout arguments_layout = function_arguments_memory_layout(
      allocator_default, function, Function_Argument_Mode_Body
    );

    DYN_ARRAY_FOREACH(Memory_Layout_Item, item, arguments_layout.items) {
      assert(item->tag == Memory_Layout_Item_Tag_Absolute);
      Value *arg_value = value_make(
        &body_context, item->descriptor, item->Absolute.storage, item->source_range
      );
      scope_define_value(body_scope, arg_value->source_range, item->name, arg_value);
      Register arg_reg = Register_SP;
      if (arg_value->storage.tag == Storage_Tag_Register) {
        arg_reg = arg_value->storage.Register.index;
      } else if(arg_value->storage.tag == Storage_Tag_Memory) {
        assert(arg_value->storage.Memory.location.tag == Memory_Location_Tag_Indirect);
        arg_reg = arg_value->storage.Memory.location.Indirect.base_register;
      } else {
        panic("Unexpected storage tag for an argument");
      }
      if (arg_reg != Register_SP) {
        register_bitset_set(&builder->register_occupied_bitset, arg_reg);
        builder->register_occupied_values[arg_reg] = arg_value;
      }
    }
    dyn_array_destroy(arguments_layout.items);
  }

  const Function_Return *returns = &function->returns;
  Value *return_value = function_return_value_for_descriptor(
    &body_context, returns->descriptor, Function_Argument_Mode_Body, returns->source_range
  );

  scope_define_value(body_scope, return_value->source_range, MASS_RETURN_VALUE_NAME, return_value);

  Value *return_label_value = value_make(
    context, &descriptor_void, code_label32(builder->code_block.end_label), returns->source_range
  );
  scope_define_value(body_scope, returns->source_range, MASS_RETURN_LABEL_NAME, return_label_value);

  // :ReturnTypeLargerThanRegister
  // Make sure we don't stomp the address of a larger-than-register
  // return value during the execution of the function
  if (
    return_value->storage.tag == Storage_Tag_Memory &&
    return_value->storage.Memory.location.tag == Memory_Location_Tag_Indirect
  ) {
    Register return_reg = return_value->storage.Memory.location.Indirect.base_register;
    register_bitset_set(&builder->register_occupied_bitset, return_reg);
    builder->register_occupied_values[return_reg] = return_value;
  }

  // Return value can be named in which case it should be accessible in the fn body
  if (function->returns.name.length) {
    scope_define_value(body_scope, return_value->source_range, function->returns.name, return_value);
  }
  Value *parse_result = token_parse_block_no_scope(&body_context, literal->body);
  MASS_ON_ERROR(*context->result) return (Storage){0};

  value_force_exact(&body_context, builder, return_value, parse_result);

  fn_end(program, builder);

  // Only push the builder at the end to avoid problems in nested JIT compiles
  dyn_array_push(program->functions, *builder);

  return code_label32(builder->code_block.start_label);
}


s64
calculate_arguments_match_score(
  const Function_Info *descriptor,
  Array_Value_Ptr arguments
) {
  enum {
    // TODO consider relationship between casts and literal types
    Score_Exact_Literal = 1000 * 1000 * 1000,
    Score_Exact_Type = 1000 * 1000,
    Score_Exact_Default = 1000,
    Score_Cast = 1,
  };
  assert(dyn_array_length(arguments) < 1000);
  s64 score = 0;
  for (u64 arg_index = 0; arg_index < dyn_array_length(descriptor->arguments); ++arg_index) {
    Function_Argument *target_arg = dyn_array_get(descriptor->arguments, arg_index);
    Value *source_arg = 0;
    const Descriptor *source_descriptor;
    if (arg_index >= dyn_array_length(arguments)) {
      if (!target_arg->maybe_default_expression.length) return -1;
      source_descriptor = target_arg->descriptor;
    } else {
      source_arg = *dyn_array_get(arguments, arg_index);
      source_descriptor = value_or_lazy_value_descriptor(source_arg);
    }
    if (same_type(target_arg->descriptor, source_descriptor)) {
      score += Score_Exact_Type;
    } else if (
      (source_arg && same_value_type_or_can_implicitly_move_cast(target_arg->descriptor, source_arg)) ||
      same_type_or_can_implicitly_move_cast(target_arg->descriptor, source_descriptor)
    ) {
      score += Score_Cast;
    } else {
      return -1;
    }
  }
  return score;
}

void
program_init_startup_code(
  Execution_Context *context
) {
  Program *program = context->program;
  Function_Info *fn_info = allocator_allocate(context->allocator, Function_Info);
  function_info_init(fn_info, 0 /* scope */);
  Descriptor *descriptor =
    descriptor_function_instance(context->allocator, slice_literal("__startup"), fn_info);
  Label_Index fn_label = make_label(program, &program->memory.code, slice_literal("__startup"));
  Storage storage = code_label32(fn_label);

  // FIXME Create a special source range for internal values
  Source_Range source_range = {0};
  Value *function = value_make(context, descriptor, storage, source_range);

  Function_Builder builder = (Function_Builder){
    .function = fn_info,
    .frozen = true,
    .code_block = {
      .start_label = fn_label,
      .end_label = make_label(program, &program->memory.code, slice_literal("__startup end")),
      .instructions = dyn_array_make(Array_Instruction, .allocator = context->allocator),
    },
  };

  // Resolve relocations
  Descriptor *void_pointer = descriptor_pointer_to(context->allocator, &descriptor_void);
  Storage register_a = storage_register_for_descriptor(Register_A, void_pointer);
  u64 relocation_count = dyn_array_length(program->relocations);
  for (u64 i = 0; i < relocation_count; ++i) {
    Relocation *relocation = dyn_array_get(program->relocations, i);
    push_instruction(
      &builder.code_block.instructions, source_range,
      (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {lea, {register_a, relocation->address_of, 0}}}
    );
    push_instruction(
      &builder.code_block.instructions, source_range,
      (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {mov, {relocation->patch_at, register_a, 0}}}
    );
  }


  for (u64 i = 0; i < dyn_array_length(context->program->startup_functions); ++i) {
    Value *fn = *dyn_array_get(context->program->startup_functions, i);
    Storage label_storage = ensure_compiled_function_body(context, fn);
    push_instruction(
      &builder.code_block.instructions, source_range,
      (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {call, {label_storage, 0, 0}}}
    );
  }
  Storage entry_label_storage = ensure_compiled_function_body(context, program->entry_point);
  push_instruction(
    &builder.code_block.instructions, source_range,
    (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {jmp, {entry_label_storage, 0, 0}}}
  );

  program->entry_point = function;
  dyn_array_push(program->functions, builder);
}

