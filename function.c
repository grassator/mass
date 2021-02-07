#include "function.h"

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
reserve_stack(
  Allocator *allocator,
  Function_Builder *fn,
  Descriptor *descriptor
) {
  s32 byte_size = u64_to_s32(descriptor_byte_size(descriptor));
  fn->stack_reserve = s32_align(fn->stack_reserve, byte_size);
  fn->stack_reserve += byte_size;
  Storage operand = stack(-fn->stack_reserve, byte_size);
  Value *result = allocator_allocate(allocator, Value);
  *result = (Value) {
    .descriptor = descriptor,
    .storage = operand,
  };
  return result;
}

void
register_acquire(
  Function_Builder *builder,
  Register reg_index
) {
  assert(!register_bitset_get(builder->code_block.register_occupied_bitset, reg_index));
  register_bitset_set(&builder->used_register_bitset, reg_index);
  register_bitset_set(&builder->code_block.register_occupied_bitset, reg_index);
}

Register
register_acquire_temp(
  Function_Builder *builder
) {
  // FIXME We are skipping Register_A here as it is hardcoded in quite a few places still
  static const Register temp_registers[] = {
    Register_C, Register_B, Register_D, Register_R8, Register_R9, Register_R10,
    Register_R11, Register_R12,  Register_R13, Register_R14,  Register_R15,
  };
  for (u32 i = 0; i < countof(temp_registers); ++i) {
    Register reg_index = temp_registers[i];
    if (!register_bitset_get(builder->code_block.register_occupied_bitset, reg_index)) {
      register_acquire(builder, reg_index);
      return reg_index;
    }
  }

  // FIXME
  panic("Could not acquire a temp register");
  return -1;
}

void
register_release(
  Function_Builder *builder,
  Register reg_index
) {
  assert(register_bitset_get(builder->code_block.register_occupied_bitset, reg_index));
  register_bitset_unset(&builder->code_block.register_occupied_bitset, reg_index);
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
  if (!register_bitset_get(builder->code_block.register_occupied_bitset, reg_index)) {
    register_acquire(builder, reg_index);
    return result;
  }

  // Save RDX as it will be used for the remainder
  result.saved_index = register_acquire_temp(builder);
  result.saved = true;

  push_instruction(
    &builder->code_block.instructions, *source_range,
    (Instruction) {.assembly = {mov, {
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
  register_release(builder, maybe_saved_register->index);
  if (maybe_saved_register->saved) {
    push_instruction(
      &builder->code_block.instructions, *maybe_saved_register->source_range,
      (Instruction) {.assembly = {mov, {
        storage_register_for_descriptor(maybe_saved_register->index, &descriptor_s64),
        storage_register_for_descriptor(maybe_saved_register->saved_index, &descriptor_s64),
      }}}
    );
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
      push_instruction(instructions, *source_range, (Instruction) {.assembly = {movss, {*target, *source, 0}}});
    } else if (target_size == 8) {
      push_instruction(instructions, *source_range, (Instruction) {.assembly = {movsd, {*target, *source, 0}}});
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
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {sete, {temp, *source}}});
        break;
      }
      case Compare_Type_Not_Equal: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {setne, {temp, *source}}});
        break;
      }

      case Compare_Type_Unsigned_Below: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {setb, {temp, *source}}});
        break;
      }
      case Compare_Type_Unsigned_Below_Equal: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {setbe, {temp, *source}}});
        break;
      }
      case Compare_Type_Unsigned_Above: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {seta, {temp, *source}}});
        break;
      }
      case Compare_Type_Unsigned_Above_Equal: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {setae, {temp, *source}}});
        break;
      }

      case Compare_Type_Signed_Less: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {setl, {temp, *source}}});
        break;
      }
      case Compare_Type_Signed_Less_Equal: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {setle, {temp, *source}}});
        break;
      }
      case Compare_Type_Signed_Greater: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {setg, {temp, *source}}});
        break;
      }
      case Compare_Type_Signed_Greater_Equal: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {setge, {temp, *source}}});
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
      push_instruction(instructions, *source_range, (Instruction) {.assembly = {movsx, {resized_temp, temp}}});
      push_instruction(instructions, *source_range, (Instruction) {.assembly = {mov, {*target, resized_temp}}});
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
      for (u64 offset = 0; offset < source->byte_size; offset += chunk_size) {
        Storage adjusted_target = *target;
        adjusted_target.byte_size = chunk_size;
        adjusted_target.Memory.location.Indirect.offset += offset;
        Storage adjusted_source = (Storage) {
          .tag = Storage_Tag_Static,
          .byte_size = chunk_size,
          .Static.memory = (s8 *)source->Static.memory + offset,
        };
        push_instruction(
          instructions, *source_range,
          (Instruction) {.assembly = {mov, {adjusted_target, adjusted_source}}}
        );
      }
      return;
    }
    s64 immediate = storage_immediate_value_up_to_s64(source);
    if (immediate == 0 && target->tag == Storage_Tag_Register) {
      // This messes up flags register so comparisons need to be aware of this optimization
      push_instruction(instructions, *source_range, (Instruction) {.assembly = {xor, {*target, *target}}});
      return;
    }
    Storage adjusted_source;
    switch(target_size) {
      case 1: {
        adjusted_source = imm8(allocator, s64_to_s8(immediate));
        break;
      }
      case 2: {
        adjusted_source = imm16(allocator, s64_to_s16(immediate));
        break;
      }
      case 4: {
        adjusted_source = imm32(allocator, s64_to_s32(immediate));
        break;
      }
      case 8: {
        // FIXME This does sign extension so will be broken for unsigned
        if (s64_fits_into_s32(immediate)) {
          adjusted_source = imm32(allocator, s64_to_s32(immediate));
        } else {
          adjusted_source = imm64(allocator, immediate);
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
      push_instruction(instructions, *source_range, (Instruction) {.assembly = {mov, {temp, adjusted_source}}});
      push_instruction(instructions, *source_range, (Instruction) {.assembly = {mov, {*target, temp}}});
      register_release(builder, temp.Register.index);
    } else {
      push_instruction(instructions, *source_range, (Instruction) {.assembly = {mov, {*target, adjusted_source}}});
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
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {mov, {adjusted_target, *source}}});
        } else {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {movsx, {*target, *source}}});
        }
      } else {
        Storage temp = {
          .tag = Storage_Tag_Register,
          .byte_size = target->byte_size,
          .Register.index = register_acquire_temp(builder),
        };
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {movsx, {temp, *source}}});
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {mov, {*target, temp}}});
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
      Value *temp_rsi = value_register_for_descriptor(allocator, register_acquire_temp(builder), &descriptor_s64);
      Value *temp_rdi = value_register_for_descriptor(allocator, register_acquire_temp(builder), &descriptor_s64);
      Value *temp_rcx = value_register_for_descriptor(allocator, register_acquire_temp(builder), &descriptor_s64);
      {
        Value *reg_rsi = value_register_for_descriptor(allocator, Register_SI, &descriptor_s64);
        Value *reg_rdi = value_register_for_descriptor(allocator, Register_DI, &descriptor_s64);
        Value *reg_rcx = value_register_for_descriptor(allocator, Register_C, &descriptor_s64);
        move_value(allocator, builder, source_range, &temp_rsi->storage, &reg_rsi->storage);
        move_value(allocator, builder, source_range, &temp_rdi->storage, &reg_rdi->storage);
        move_value(allocator, builder, source_range, &temp_rcx->storage, &reg_rcx->storage);

        push_instruction(instructions, *source_range, (Instruction) {.assembly = {lea, {reg_rsi->storage, *source}}});
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {lea, {reg_rdi->storage, *target}}});
        Storage size_operand = imm64(allocator, target_size);
        move_value(allocator, builder, source_range, &reg_rcx->storage, &size_operand);
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {rep_movsb}});

        move_value(allocator, builder, source_range, &reg_rsi->storage, &temp_rsi->storage);
        move_value(allocator, builder, source_range, &reg_rdi->storage, &temp_rdi->storage);
        move_value(allocator, builder, source_range, &reg_rcx->storage, &temp_rcx->storage);
      }
      register_release(builder, temp_rsi->storage.Register.index);
      register_release(builder, temp_rdi->storage.Register.index);
      register_release(builder, temp_rcx->storage.Register.index);
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

  push_instruction(instructions, *source_range, (Instruction) {.assembly = {mov, {*target, *source}}});
}

void
move_to_result_from_temp(
  Allocator *allocator,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *target,
  Value *temp_source
) {
  if (target->descriptor->tag == Descriptor_Tag_Any) {
    target->descriptor = temp_source->descriptor;
    assert(!target->next_overload);
    target->next_overload = temp_source->next_overload;
  }
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

// TODO move to platform code somehow
void
win32_set_volatile_registers_for_function(
  Function_Builder *builder
) {
  // Arguments
  register_bitset_set(&builder->code_block.register_volatile_bitset, Register_C);
  register_bitset_set(&builder->code_block.register_volatile_bitset, Register_D);
  register_bitset_set(&builder->code_block.register_volatile_bitset, Register_R8);
  register_bitset_set(&builder->code_block.register_volatile_bitset, Register_R9);

  // Return
  register_bitset_set(&builder->code_block.register_volatile_bitset, Register_A);

  // Other
  register_bitset_set(&builder->code_block.register_volatile_bitset, Register_R10);
  register_bitset_set(&builder->code_block.register_volatile_bitset, Register_R11);
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
  for (u8 storage_index = 0; storage_index < countof(instruction->assembly.operands); ++storage_index) {
    Storage *storage = &instruction->assembly.operands[storage_index];
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
  if (last_instruction->type != Instruction_Type_Assembly) return;
  if (last_instruction->assembly.mnemonic != jmp) return;
  Storage storage = last_instruction->assembly.operands[0];
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
  assert(builder->function->returns.descriptor->tag != Descriptor_Tag_Any);

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
    // @Leak
    &(Instruction) {.assembly = {mov, {rax, imm64(allocator_default, address)}}}
  );
  encode_instruction_with_compiler_location(
    program, buffer,
    &(Instruction) {.assembly = {jmp, {rax}}}
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

  Label_Index label_index = builder->label_index;

  Label *label = program_get_label(program, label_index);

  // Calls to `fn_encode` do not do anything if we already encoded
  if (label->resolved) return;

  s64 code_base_rva = label->section->base_rva;
  out_layout->begin_rva = u64_to_u32(code_base_rva + buffer->occupied);
  // @Leak
  Storage stack_size_operand = imm_auto_8_or_32(allocator_default, out_layout->stack_reserve);
  encode_instruction_with_compiler_location(
    program, buffer, &(Instruction) {
      .type = Instruction_Type_Label,
      .label = label_index
    }
  );

  // :RegisterPushPop
  // :Win32UnwindCodes Must match what happens in the unwind code generation
  // Push non-volatile registers (in reverse order)
  u8 push_index = 0;
  for (s32 reg_index = Register_R15; reg_index >= Register_A; --reg_index) {
    if (register_bitset_get(builder->used_register_bitset, reg_index)) {
      if (!register_bitset_get(builder->code_block.register_volatile_bitset, reg_index)) {
        out_layout->volatile_register_push_offsets[push_index++] =
          u64_to_u8(code_base_rva + buffer->occupied - out_layout->begin_rva);
        Storage to_save = storage_register_for_descriptor(reg_index, &descriptor_s64);
        encode_instruction_with_compiler_location(
          program, buffer, &(Instruction) {.assembly = {push, {to_save}}}
        );
      }
    }
  }

  encode_instruction_with_compiler_location(
    program, buffer, &(Instruction) {.assembly = {sub, {rsp, stack_size_operand}}}
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
      .type = Instruction_Type_Label, .label = builder->code_block.end_label
    }
  );

  // :ReturnTypeLargerThanRegister
  if(descriptor_byte_size(builder->function->returns.descriptor) > 8) {
    // FIXME :RegisterAllocation
    //       make sure that return value is always available in RCX at this point
    encode_instruction_with_compiler_location(
      program, buffer, &(Instruction) {.assembly = {mov, {rax, rcx}}}
    );
  }

  encode_instruction_with_compiler_location(
    program, buffer, &(Instruction) {.assembly = {add, {rsp, stack_size_operand}}}
  );

  // :RegisterPushPop
  // Pop non-volatile registers (in original order)
  for (Register reg_index = 0; reg_index <= Register_R15; ++reg_index) {
    if (register_bitset_get(builder->used_register_bitset, reg_index)) {
      if (!register_bitset_get(builder->code_block.register_volatile_bitset, reg_index)) {
        Storage to_save = storage_register_for_descriptor(reg_index, &descriptor_s64);
        encode_instruction_with_compiler_location(
          program, buffer, &(Instruction) {.assembly = {pop, {to_save}}}
        );
      }
    }
  }

  encode_instruction_with_compiler_location(program, buffer, &(Instruction) {.assembly = {ret, {0}}});
  out_layout->end_rva = u64_to_u32(code_base_rva + buffer->occupied);

  encode_instruction_with_compiler_location(program, buffer, &(Instruction) {.assembly = {int3, {0}}});
}

Value
function_return_value_for_descriptor(
  Descriptor *descriptor,
  Function_Argument_Mode mode
) {
  if (descriptor->tag == Descriptor_Tag_Void) {
    return void_value;
  }
  // TODO handle 16 byte non-float return values in XMM0
  if (descriptor_is_float(descriptor)) {
    return (Value) {
      .descriptor = descriptor,
      .storage = storage_register_for_descriptor(Register_Xmm0, descriptor),
    };
  }
  u64 byte_size = descriptor_byte_size(descriptor);
  if (byte_size <= 8) {
    return (Value) {
      .descriptor = descriptor,
      .storage = storage_register_for_descriptor(Register_A, descriptor),
    };
  }
  // :ReturnTypeLargerThanRegister
  // Inside the function large returns are pointed to by RCX,
  // but this pointer is also returned in A
  Register base_register = Register_A;
  if (mode == Function_Argument_Mode_Body) {
    base_register = Register_C;
  }
  return (Value){
    .descriptor = descriptor,
    .storage = {
      .tag = Storage_Tag_Memory,
      .byte_size = byte_size,
      .Memory.location = {
        .tag = Memory_Location_Tag_Indirect,
        .Indirect = {
          .base_register = base_register,
        }
      }
    },
  };
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
  Label_Index label = make_label(program, &program->memory.sections.code, slice_literal("if"));
  if(value->storage.tag == Storage_Tag_Static) {
    s64 imm = storage_immediate_value_up_to_s64(&value->storage);
    if (imm == 0) return label;
    is_always_true = true;
  }

  if (!is_always_true) {
    if (value->storage.tag == Storage_Tag_Eflags) {
      switch(value->storage.Eflags.compare_type) {
        case Compare_Type_Equal: {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {jne, {code_label32(label), value->storage, 0}}});
          break;
        }
        case Compare_Type_Not_Equal: {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {je, {code_label32(label), value->storage, 0}}});
          break;
        }

        case Compare_Type_Unsigned_Below: {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {jae, {code_label32(label), value->storage, 0}}});
          break;
        }
        case Compare_Type_Unsigned_Below_Equal: {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {ja, {code_label32(label), value->storage, 0}}});
          break;
        }
        case Compare_Type_Unsigned_Above: {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {jbe, {code_label32(label), value->storage, 0}}});
          break;
        }
        case Compare_Type_Unsigned_Above_Equal: {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {jb, {code_label32(label), value->storage, 0}}});
          break;
        }

        case Compare_Type_Signed_Less: {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {jge, {code_label32(label), value->storage, 0}}});
          break;
        }
        case Compare_Type_Signed_Less_Equal: {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {jg, {code_label32(label), value->storage, 0}}});
          break;
        }
        case Compare_Type_Signed_Greater: {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {jle, {code_label32(label), value->storage, 0}}});
          break;
        }
        case Compare_Type_Signed_Greater_Equal: {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {jl, {code_label32(label), value->storage, 0}}});
          break;
        }
        default: {
          assert(!"Unsupported comparison");
        }
      }
    } else {
      u64 byte_size = descriptor_byte_size(value->descriptor);
      if (byte_size == 4 || byte_size == 8) {
        push_instruction(
          instructions, *source_range,
          (Instruction) {.assembly = {cmp, {value->storage, imm32(context->allocator, 0), 0}}}
        );
      } else if (byte_size == 1) {
        push_instruction(
          instructions, *source_range,
          (Instruction) {.assembly = {cmp, {value->storage, imm8(context->allocator, 0), 0}}}
        );
      } else {
        assert(!"Unsupported value inside `if`");
      }
      Value *eflags = value_from_compare(context->allocator, Compare_Type_Equal);
      push_instruction(instructions, *source_range, (Instruction) {.assembly = {je, {code_label32(label), eflags->storage, 0}}});
    }
  }
  return label;
}

typedef enum {
  Arithmetic_Operation_Plus,
  Arithmetic_Operation_Minus,
} Arithmetic_Operation;


#define maybe_constant_fold(_context_, _loc_, _result_, _a_, _b_, _operator_)\
  do {\
    Storage *a_operand = &(_a_)->storage;\
    Storage *b_operand = &(_b_)->storage;\
    if (a_operand->tag == Storage_Tag_Static && b_operand->tag == Storage_Tag_Static) {\
      s64 a_s64 = storage_immediate_value_up_to_s64(a_operand);\
      s64 b_s64 = storage_immediate_value_up_to_s64(b_operand);\
      Value *imm_value = value_from_signed_immediate((_context_)->allocator, a_s64 _operator_ b_s64);\
      MASS_ON_ERROR(assign((_context_), (_loc_), (_result_), imm_value));\
      return;\
    }\
  } while(0)

void
plus_or_minus(
  Execution_Context *context,
  Arithmetic_Operation operation,
  const Source_Range *source_range,
  Value *result_value,
  Value *a,
  Value *b
) {
  bool is_pointer_arithmetic = (
    a->descriptor->tag == Descriptor_Tag_Pointer &&
    descriptor_is_integer(b->descriptor) &&
    b->descriptor->Opaque.bit_size == 64
  );
  bool both_operands_are_integers = (
    descriptor_is_integer(a->descriptor) &&
    descriptor_is_integer(b->descriptor)
  );
  assert(is_pointer_arithmetic || both_operands_are_integers);

  switch(operation) {
    case Arithmetic_Operation_Plus: {
      maybe_constant_fold(context, source_range, result_value, a, b, +);
      break;
    }
    case Arithmetic_Operation_Minus: {
      maybe_constant_fold(context, source_range, result_value, a, b, -);
      break;
    }
  }

  // TODO this is not optimal as the immediate might actually fit into a smaller size
  //      that we do support but it is currently very messy to handle
  // There is no `add r/m64 imm64` so use a temp register
  Value *temp_immediate = 0;
  if (a->storage.tag == Storage_Tag_Static && a->storage.byte_size == 8) {
    temp_immediate = value_register_for_descriptor(
      context->allocator, register_acquire_temp(context->builder), a->descriptor
    );
    move_value(context->allocator, context->builder, source_range, &temp_immediate->storage, &a->storage);
    a = temp_immediate;
  } else if (b->storage.tag == Storage_Tag_Static && b->storage.byte_size == 8) {
    temp_immediate = value_register_for_descriptor(
      context->allocator, register_acquire_temp(context->builder), b->descriptor
    );
    move_value(context->allocator, context->builder, source_range, &temp_immediate->storage, &b->storage);
    b = temp_immediate;
  }

  u64 a_size = descriptor_byte_size(a->descriptor);
  u64 b_size = descriptor_byte_size(b->descriptor);
  Value *maybe_a_or_b_temp = 0;

  if (a_size != b_size) {
    if (a_size > b_size) {
      Value *b_sized_to_a = value_register_for_descriptor(
        context->allocator, register_acquire_temp(context->builder), a->descriptor
      );
      move_value(context->allocator, context->builder, source_range, &b_sized_to_a->storage, &b->storage);
      b = b_sized_to_a;
      maybe_a_or_b_temp = b;
    } else {
      Value *a_sized_to_b = value_register_for_descriptor(
        context->allocator, register_acquire_temp(context->builder), b->descriptor
      );
      move_value(context->allocator, context->builder, source_range, &a_sized_to_b->storage, &b->storage);
      a = a_sized_to_b;
      maybe_a_or_b_temp = a;
    }
  }

  const X64_Mnemonic *mnemonic = 0;
  switch(operation) {
    case Arithmetic_Operation_Plus: {
      mnemonic = add;
      // Addition is commutative (a + b == b + a)
      // so we can swap operands and save one instruction
      if (storage_equal(&result_value->storage, &b->storage) || maybe_a_or_b_temp == b) {
        value_swap(Value *, a, b);
      }
      break;
    }
    case Arithmetic_Operation_Minus: {
      mnemonic = sub;
      break;
    }
  }

  bool can_reuse_result_as_temp = (
    result_value->storage.tag == Storage_Tag_Register &&
    !storage_equal(&result_value->storage, &b->storage)
  );
  Value *temp = 0;
  if (can_reuse_result_as_temp) {
    temp = result_value;
  } else if (maybe_a_or_b_temp == a) {
    temp = a;
  } else {
    temp = value_register_for_descriptor(
      context->allocator, register_acquire_temp(context->builder), a->descriptor
    );
  }

  move_value(context->allocator, context->builder, source_range, &temp->storage, &a->storage);
  push_instruction(
    &context->builder->code_block.instructions,
    *source_range,
    (Instruction) {.assembly = {mnemonic, {temp->storage, b->storage}}}
  );
  if (temp != result_value) {
    assert(temp->storage.tag == Storage_Tag_Register);
    move_to_result_from_temp(context->allocator, context->builder, source_range, result_value, temp);
  }
  if (temp_immediate) {
    register_release(context->builder, temp_immediate->storage.Register.index);
  }
}

void
plus(
  Execution_Context *context,
  const Source_Range *source_range,
  Value *result_value,
  Value *a,
  Value *b
) {
  plus_or_minus(context, Arithmetic_Operation_Plus, source_range, result_value, a, b);
}

void
minus(
  Execution_Context *context,
  const Source_Range *source_range,
  Value *result_value,
  Value *a,
  Value *b
) {
  plus_or_minus(context, Arithmetic_Operation_Minus, source_range, result_value, a, b);
}

void
multiply(
  Execution_Context *context,
  const Source_Range *source_range,
  Value *result_value,
  Value *x,
  Value *y
) {
  Allocator *allocator = context->allocator;
  Function_Builder *builder = context->builder;
  Array_Instruction *instructions = &builder->code_block.instructions;
  assert(same_value_type(x, y));
  assert(descriptor_is_integer(x->descriptor));

  maybe_constant_fold(context, source_range, result_value, x, y, *);

  // TODO deal with signed / unsigned
  // TODO support double the size of the result?
  Value *y_temp = reserve_stack(allocator, builder, y->descriptor);

  Register temp_register_index = register_acquire_temp(builder);
  Value *temp_register = value_register_for_descriptor(allocator, temp_register_index, y->descriptor);
  move_value(allocator, builder, source_range, &temp_register->storage, &y->storage);
  move_value(allocator, builder, source_range, &y_temp->storage, &temp_register->storage);

  temp_register = value_register_for_descriptor(allocator, temp_register_index, x->descriptor);
  move_value(allocator, builder, source_range, &temp_register->storage, &x->storage);

  push_instruction(
    instructions, *source_range,
    (Instruction) {.assembly = {imul, {temp_register->storage, y_temp->storage}}}
  );

  move_to_result_from_temp(allocator, builder, source_range, result_value, temp_register);
}

typedef enum {
  Divide_Operation_Divide,
  Divide_Operation_Remainder,
} Divide_Operation;

void
divide_or_remainder(
  Execution_Context *context,
  Divide_Operation operation,
  const Source_Range *source_range,
  Value *result_value,
  Value *a,
  Value *b
) {
  Allocator *allocator = context->allocator;
  Function_Builder *builder = context->builder;
  Array_Instruction *instructions = &builder->code_block.instructions;
  assert(same_value_type_or_can_implicitly_move_cast(a, b));
  assert(descriptor_is_integer(a->descriptor));

  switch(operation) {
    case Divide_Operation_Divide: {
      maybe_constant_fold(context, source_range, result_value, a, b, /);
      break;
    }
    case Divide_Operation_Remainder: {
      maybe_constant_fold(context, source_range, result_value, a, b, %);
      break;
    }
  }

  // Save RDX as it will be used for the remainder
  Maybe_Saved_Register maybe_saved_rdx = register_acquire_maybe_save_if_already_acquired(
    allocator, builder, source_range, Register_D
  );

  Descriptor *larger_descriptor =
    descriptor_byte_size(a->descriptor) > descriptor_byte_size(b->descriptor)
    ? a->descriptor
    : b->descriptor;

  // TODO deal with signed / unsigned
  Value *divisor = reserve_stack(allocator, builder, larger_descriptor);
  move_value(allocator, builder, source_range, &divisor->storage, &b->storage);

  Value *reg_a = value_register_for_descriptor(allocator, Register_A, larger_descriptor);
  {
    move_value(allocator, builder, source_range, &reg_a->storage, &a->storage);

    switch (descriptor_byte_size(larger_descriptor)) {
      case 8: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {cqo, {0}}});
        break;
      }
      case 4: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {cdq, {0}}});
        break;
      }
      case 2: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {cwd, {0}}});
        break;
      }
      case 1: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {cwb, {0}}});
        break;
      }
      default: {
        assert(!"Unsupported byte size when dividing");
      }
    }
  }
  push_instruction(instructions, *source_range, (Instruction) {.assembly = {idiv, {divisor->storage, 0, 0}}});


  // FIXME division uses specific registers so if the result_value operand is `any`
  //       we need to create a new temporary value and "return" that
  if (operation == Divide_Operation_Divide) {
    move_value(allocator, builder, source_range, &result_value->storage, &reg_a->storage);
  } else {
    if (descriptor_byte_size(larger_descriptor) == 1) {
      Value *temp_result = value_register_for_descriptor(allocator, Register_AH, larger_descriptor);
      move_value(allocator, builder, source_range, &result_value->storage, &temp_result->storage);
    } else {
      Value *temp_result = value_register_for_descriptor(allocator, Register_D, larger_descriptor);
      move_value(allocator, builder, source_range, &result_value->storage, &temp_result->storage);
    }
  }

  register_release_maybe_restore(builder, &maybe_saved_rdx);
}

void
divide(
  Execution_Context *context,
  const Source_Range *source_range,
  Value *result_value,
  Value *a,
  Value *b
) {
  divide_or_remainder(context, Divide_Operation_Divide, source_range, result_value, a, b);
}

void
value_remainder(
  Execution_Context *context,
  const Source_Range *source_range,
  Value *result_value,
  Value *a,
  Value *b
) {
  divide_or_remainder(context, Divide_Operation_Remainder, source_range, result_value, a, b);
}


void
compare(
  Execution_Context *context,
  Compare_Type operation,
  const Source_Range *source_range,
  Value *result_value,
  Value *a,
  Value *b
) {
  Allocator *allocator = context->allocator;
  Function_Builder *builder = context->builder;
  Array_Instruction *instructions = &builder->code_block.instructions;
  assert(descriptor_is_integer(a->descriptor));
  assert(same_value_type_or_can_implicitly_move_cast(a, b));

  switch(operation) {
    case Compare_Type_Equal: {
      maybe_constant_fold(context, source_range, result_value, a, b, ==);
      break;
    }
    case Compare_Type_Not_Equal: {
      maybe_constant_fold(context, source_range, result_value, a, b, !=);
      break;
    }

    case Compare_Type_Unsigned_Below: {
      maybe_constant_fold(context, source_range, result_value, a, b, <);
      break;
    }
    case Compare_Type_Unsigned_Below_Equal: {
      maybe_constant_fold(context, source_range, result_value, a, b, <=);
      break;
    }
    case Compare_Type_Unsigned_Above: {
      maybe_constant_fold(context, source_range, result_value, a, b, >);
      break;
    }
    case Compare_Type_Unsigned_Above_Equal: {
      maybe_constant_fold(context, source_range, result_value, a, b, >=);
      break;
    }

    case Compare_Type_Signed_Less: {
      maybe_constant_fold(context, source_range, result_value, a, b, <);
      break;
    }
    case Compare_Type_Signed_Less_Equal: {
      maybe_constant_fold(context, source_range, result_value, a, b, <=);
      break;
    }
    case Compare_Type_Signed_Greater: {
      maybe_constant_fold(context, source_range, result_value, a, b, >);
      break;
    }
    case Compare_Type_Signed_Greater_Equal: {
      maybe_constant_fold(context, source_range, result_value, a, b, >=);
      break;
    }
    default: {
      assert(!"Unsupported comparison");
    }
  }

  Descriptor *larger_descriptor =
    descriptor_byte_size(a->descriptor) > descriptor_byte_size(b->descriptor)
    ? a->descriptor
    : b->descriptor;

  Value *temp_b = reserve_stack(allocator, builder, larger_descriptor);
  move_value(allocator, builder, source_range, &temp_b->storage, &b->storage);

  Value *reg_r11 = value_register_for_descriptor(allocator, Register_R11, larger_descriptor);
  move_value(allocator, builder, source_range, &reg_r11->storage, &a->storage);

  push_instruction(instructions, *source_range, (Instruction) {.assembly = {cmp, {reg_r11->storage, temp_b->storage, 0}}});

  // FIXME if the result_value operand is any we should create a temp value
  Value *comparison_value = value_from_compare(allocator, operation);
  MASS_ON_ERROR(assign(context, source_range, result_value, comparison_value));
}

void
load_address(
  Execution_Context *context,
  const Source_Range *source_range,
  Value *result_value,
  const Value *memory
) {
  assert(memory->storage.tag == Storage_Tag_Memory);
  Descriptor *result_descriptor = descriptor_pointer_to(context->allocator, memory->descriptor);

  Value *temp_register = result_value->storage.tag == Storage_Tag_Register
    ? result_value
    : value_register_for_descriptor(
        context->allocator, register_acquire_temp(context->builder), result_descriptor
    );

  // TODO rethink operand sizing
  // We need to manually adjust the size here because even if we loading one byte
  // the right side is treated as an opaque address and does not participate in
  // instruction encoding.
  Storage source_operand = memory->storage;
  source_operand.byte_size = descriptor_byte_size(result_descriptor);

  push_instruction(
    &context->builder->code_block.instructions, *source_range,
    (Instruction) {.assembly = {lea, {temp_register->storage, source_operand, 0}}}
  );

  move_to_result_from_temp(
    context->allocator, context->builder, source_range, result_value, temp_register
  );
}

typedef struct {
  Value saved;
  Value *stack_value;
} Saved_Register;
typedef dyn_array_type(Saved_Register) Array_Saved_Register;

void
ensure_compiled_function_body(
  Execution_Context *context,
  Value *fn_value
) {
  // If the value already has the operand we assume it is compiled
  if (fn_value->storage.tag != Storage_Tag_None) return;

  assert(fn_value->descriptor->tag == Descriptor_Tag_Function);
  Descriptor_Function *function = &fn_value->descriptor->Function;

  // No need to compile macro body as it will be compiled inline
  if (function->flags & Descriptor_Function_Flags_Macro) return;
  assert(function->body);

  if (function->flags & Descriptor_Function_Flags_External) {
    assert(function->body->tag == Token_Tag_Value);
    Value *body_value = function->body->Value.value;
    assert(body_value->descriptor == &descriptor_external_symbol);
    assert(body_value->storage.tag == Storage_Tag_Static);
    External_Symbol *symbol = body_value->storage.Static.memory;
    fn_value->storage = import_symbol(context, symbol->library_name, symbol->symbol_name);
    return;
  }

  Program *program = context->program;
  // FIXME @Speed switch this to a hash map lookup
  // If we already built the function for the target program just set the operand
  for (u64 i = 0; i < dyn_array_length(program->functions); ++i) {
    Function_Builder *builder = dyn_array_get(program->functions, i);
    if (builder->function == function) {
      fn_value->storage = code_label32(builder->label_index);
      return;
    }
  }

  // TODO better name (coming from the function)
  Slice fn_name = fn_value->descriptor->name.length
    ? fn_value->descriptor->name
    : slice_literal("anonymous_function");
  Label_Index fn_label = make_label(program, &program->memory.sections.code, fn_name);
  fn_value->storage = code_label32(fn_label);

  Function_Builder builder = (Function_Builder){
    .function = function,
    .label_index = fn_label,
    .code_block = {
      // FIXME use fn_value->descriptor->name
      .end_label = make_label(program, &program->memory.sections.code, slice_literal("fn end")),
      .instructions = dyn_array_make(Array_Instruction, .allocator = context->allocator),
    },
  };

  win32_set_volatile_registers_for_function(&builder);

  Execution_Context body_context = *context;
  Scope *body_scope = scope_make(context->allocator, function->scope);
  for (u64 index = 0; index < dyn_array_length(function->arguments); ++index) {
    Function_Argument *argument = dyn_array_get(function->arguments, index);
    switch(argument->tag) {
      case Function_Argument_Tag_Any_Of_Type: {
        Value *arg_value = function_argument_value_at_index(
          context->allocator, function, index, Function_Argument_Mode_Body
        );
        Slice name = argument->Any_Of_Type.name;
        scope_define(body_scope, name, (Scope_Entry) {
          .tag = Scope_Entry_Tag_Value,
          .Value.value = arg_value,
        });
        if (arg_value->storage.tag == Storage_Tag_Register) {
          register_bitset_set(
            &builder.code_block.register_occupied_bitset,
            arg_value->storage.Register.index
          );
        }
        break;
      }
      case Function_Argument_Tag_Exact: {
        // Nothing to do since there is no way to refer to this in the body
        break;
      }
    }
  }

  Value *return_value = allocator_allocate(context->allocator, Value);
  *return_value = function_return_value_for_descriptor(
    function->returns.descriptor, Function_Argument_Mode_Body
  );

  scope_define(body_scope, MASS_RETURN_VALUE_NAME, (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = return_value,
  });

  Value *return_label_value = allocator_allocate(context->allocator, Value);
  *return_label_value = (Value) {
    .descriptor = &descriptor_void,
    .storage = code_label32(builder.code_block.end_label),
  };
  scope_define(body_scope, MASS_RETURN_LABEL_NAME, (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = return_label_value,
  });

  // :ReturnTypeLargerThanRegister
  // Make sure we don't stomp the address of a larger-than-register
  // return value during the execution of the function
  if (
    return_value->storage.tag == Storage_Tag_Memory &&
    return_value->storage.Memory.location.tag == Memory_Location_Tag_Indirect
  ) {
    register_bitset_set(
      &builder.code_block.register_occupied_bitset,
      return_value->storage.Memory.location.Indirect.base_register
    );
  }

  // Return value can be named in which case it should be accessible in the fn body
  if (function->returns.name.length) {
    scope_define(body_scope, function->returns.name, (Scope_Entry) {
      .tag = Scope_Entry_Tag_Value,
      .Value.value = return_value,
    });
  }

  // TODO Should this set compilation_mode?
  body_context.scope = body_scope;
  body_context.builder = &builder;
  token_parse_block_no_scope(&body_context, function->body, return_value);

  fn_end(program, &builder);

  // Only push the builder at the end to avoid problems in nested JIT compiles
  dyn_array_push(program->functions, builder);
}

void
call_function_overload(
  Execution_Context *context,
  const Source_Range *source_range,
  Value *to_call,
  Array_Value_Ptr arguments,
  Value *result_value
) {
  Function_Builder *builder = context->builder;
  Array_Instruction *instructions = &builder->code_block.instructions;
  Descriptor *to_call_descriptor = maybe_unwrap_pointer_descriptor(to_call->descriptor);
  assert(to_call_descriptor->tag == Descriptor_Tag_Function);
  Descriptor_Function *descriptor = &to_call_descriptor->Function;

  ensure_compiled_function_body(context, to_call);

  Array_Saved_Register saved_array = dyn_array_make(Array_Saved_Register);

  for (Register reg_index = 0; reg_index <= Register_R15; ++reg_index) {
    if (register_bitset_get(builder->code_block.register_volatile_bitset, reg_index)) {
      if (register_bitset_get(builder->code_block.register_occupied_bitset, reg_index)) {
        Value to_save = {
          .descriptor = &descriptor_s64,
          .storage = {
            .tag = Storage_Tag_Register,
            .byte_size = 8,
            .Register.index = reg_index,
          }
        };
        Storage source = storage_register_for_descriptor(reg_index, &descriptor_s64);
        Value *stack_value = reserve_stack(context->allocator, builder, to_save.descriptor);
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {mov, {stack_value->storage, source}}});
        dyn_array_push(saved_array, (Saved_Register){.saved = to_save, .stack_value = stack_value});
      }
    }
  }

  Scope *default_arguments_scope = scope_make(context->allocator, descriptor->scope);
  for (u64 i = 0; i < dyn_array_length(descriptor->arguments); ++i) {
    Function_Argument *target_arg_definition = dyn_array_get(descriptor->arguments, i);
    Value *target_arg = function_argument_value_at_index(
      context->allocator, descriptor, i, Function_Argument_Mode_Call
    );
    Value *source_arg;
    if (i >= dyn_array_length(arguments)) {
      Token_View default_expression = target_arg_definition->Any_Of_Type.maybe_default_expression;
      assert(default_expression.length);
      // FIXME do not force the result on the stack
      source_arg = reserve_stack(
        context->allocator, context->builder, target_arg_definition->Any_Of_Type.descriptor
      );
      Execution_Context arg_context = *context;
      arg_context.scope = default_arguments_scope;
      token_parse_expression(
        &arg_context, default_expression, source_arg, Expression_Parse_Mode_Default
      );
      MASS_ON_ERROR(*arg_context.result) return;
    } else {
      source_arg = *dyn_array_get(arguments, i);
    }
    if (
      descriptor_byte_size(source_arg->descriptor) <= 8 ||
      // TODO Number literals are larger than a register, but only converted into
      //      a proper value in the assign below so need this explicit check.
      //      Maybe we should do the conversion at some step before?
      source_arg->descriptor == &descriptor_number_literal
    ) {
      assign(context, source_range, target_arg, source_arg);
    } else {
      // Large values are copied to the stack and passed by a reference
      Value *stack_value = reserve_stack(context->allocator, builder, source_arg->descriptor);
      assign(context, source_range, stack_value, source_arg);
      load_address(context, source_range, target_arg, stack_value);
    }
    Slice name;
    switch(target_arg_definition->tag) {
      case Function_Argument_Tag_Any_Of_Type: {
        name = target_arg_definition->Any_Of_Type.name;
        break;
      }
      default:
      case Function_Argument_Tag_Exact: {
        name = (Slice){0};
        break;
      }
    }
    if (name.length) {
      scope_define(default_arguments_scope, name, (Scope_Entry) {
        .tag = Scope_Entry_Tag_Value,
        .Value.value = target_arg,
      });
    }
  }

  // If we call a function, then we need to reserve space for the home
  // area of at least 4 arguments?
  u64 parameters_stack_size = u64_max(4, dyn_array_length(arguments)) * 8;

  Value fn_return_value = function_return_value_for_descriptor(
    descriptor->returns.descriptor, Function_Argument_Mode_Call
  );

  // :ReturnTypeLargerThanRegister
  u64 return_size = descriptor_byte_size(descriptor->returns.descriptor);
  if (return_size > 8) {
    Storage result_operand;
    // If we want the result at a memory location can just pass that address to the callee
    if (result_value->storage.tag == Storage_Tag_Memory) {
      result_operand = result_value->storage;
    } else {
      result_operand =
        reserve_stack(context->allocator, builder, descriptor->returns.descriptor)->storage;
    }
    Storage reg_c = storage_register_for_descriptor(Register_C, &descriptor_s64);
    push_instruction(
      instructions, *source_range,
      (Instruction) {.assembly = {lea, {reg_c, result_operand}}}
    );
  }

  builder->max_call_parameters_stack_size = u64_to_u32(u64_max(
    builder->max_call_parameters_stack_size,
    parameters_stack_size
  ));

  if (to_call->storage.tag == Storage_Tag_Static) {
    // TODO it will not be safe to use this register with other calling conventions
    Storage reg = storage_register_for_descriptor(Register_A, to_call_descriptor);
    push_instruction(instructions, *source_range, (Instruction) {.assembly = {mov, {reg, to_call->storage}}});
    push_instruction(instructions, *source_range, (Instruction) {.assembly = {call, {reg}}});
  } else {
    push_instruction(instructions, *source_range, (Instruction) {.assembly = {call, {to_call->storage, 0, 0}}});
  }

  Value *saved_result = &fn_return_value;
  if (return_size <= 8) {
    if (return_size != 0) {
      // FIXME Should not be necessary with correct register allocation
      saved_result = reserve_stack(context->allocator, builder, descriptor->returns.descriptor);
      move_value(context->allocator, builder, source_range, &saved_result->storage, &fn_return_value.storage);
    }
  }

  MASS_ON_ERROR(assign(context, source_range, result_value, saved_result));

  for (u64 i = 0; i < dyn_array_length(saved_array); ++i) {
    Saved_Register *reg = dyn_array_get(saved_array, i);
    move_value(context->allocator, builder, source_range, &reg->saved.storage, &reg->stack_value->storage);
    // TODO :FreeStackAllocation
  }
}


s64
calculate_arguments_match_score(
  Descriptor_Function *descriptor,
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
    Value *source_arg;
    Value fake_source_value = {0};
    if (arg_index >= dyn_array_length(arguments)) {
      if (target_arg->tag != Function_Argument_Tag_Any_Of_Type) return -1;
      if (!target_arg->Any_Of_Type.maybe_default_expression.length) return -1;
      fake_source_value = (Value) {
        .descriptor = target_arg->Any_Of_Type.descriptor,
        .storage = {.tag = Storage_Tag_Any },
      };
      source_arg = &fake_source_value;
    } else {
      source_arg = *dyn_array_get(arguments, arg_index);
    }
    switch(target_arg->tag) {
      case Function_Argument_Tag_Any_Of_Type: {
        Value fake_target_value = {
          .descriptor = target_arg->Any_Of_Type.descriptor,
          .storage = {.tag = Storage_Tag_Any },
        };
        if (same_value_type(&fake_target_value, source_arg)) {
          score += Score_Exact_Type;
        } else if(same_value_type_or_can_implicitly_move_cast(&fake_target_value, source_arg)) {
          score += Score_Cast;
        } else {
          return -1;
        }
        break;
      }
      case Function_Argument_Tag_Exact: {
        if (same_type(target_arg->Exact.descriptor, source_arg->descriptor)) {
          if(source_arg->descriptor == &descriptor_number_literal) {
            assert(source_arg->storage.tag == Storage_Tag_Static);
            assert(target_arg->Exact.storage.tag == Storage_Tag_Static);
            Number_Literal *source_literal = source_arg->storage.Static.memory;
            Number_Literal *target_literal = source_arg->storage.Static.memory;
            if (
              source_literal->bits == target_literal->bits &&
              source_literal->negative == target_literal->negative
            ) {
              return Score_Exact_Literal;
            }
          } else if (
            source_arg->storage.tag == Storage_Tag_Static &&
            storage_equal(&target_arg->Exact.storage, &source_arg->storage)
          ) {
            return Score_Exact_Literal;
          }
        }
        return -1;
      }
    }

  }
  return score;
}

Value *
make_and(
  Execution_Context *context,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *a,
  Value *b
) {
  Program *program = context->program;
  Array_Instruction *instructions = &builder->code_block.instructions;
  Value *result = reserve_stack(context->allocator, builder, &descriptor_s8);
  Label_Index label = make_label(program, &program->memory.sections.code, slice_literal("&&"));

  Value zero = {
    .descriptor = &descriptor_s8,
    .storage = imm8(context->allocator, 0),
  };

  Label_Index else_label = make_if(context, instructions, source_range, a);
  {
    compare(context, Compare_Type_Not_Equal, source_range, result, b, &zero);
    push_instruction(instructions, *source_range, (Instruction) {.assembly = {jmp, {code_label32(label)}}});
  }
  push_instruction(instructions, *source_range, (Instruction) {
    .type = Instruction_Type_Label,
    .label = else_label,
  });

  move_value(context->allocator, builder, source_range, &result->storage, &zero.storage);
  push_instruction(instructions, *source_range, (Instruction) {
    .type = Instruction_Type_Label,
    .label = label,
  });
  return result;
}

Value *
make_or(
  Execution_Context *context,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *a,
  Value *b
) {
  Program *program = context->program;
  Array_Instruction *instructions = &builder->code_block.instructions;
  Value *result = reserve_stack(context->allocator, builder, &descriptor_s8);
  Label_Index label = make_label(program, &program->memory.sections.code, slice_literal("||"));

  Value zero = {
    .descriptor = &descriptor_s8,
    .storage = imm8(context->allocator, 0),
  };

  compare(context, Compare_Type_Equal, source_range, result, a, &zero);
  Label_Index else_label = make_if(context, instructions, source_range, result);
  {
    compare(context, Compare_Type_Not_Equal, source_range, result, b, &zero);
    push_instruction(instructions, *source_range, (Instruction) {.assembly = {jmp, {code_label32(label)}}});
  }
  push_instruction(instructions, *source_range, (Instruction) {
    .type = Instruction_Type_Label,
    .label = else_label,
  });

  Storage one = imm8(context->allocator, 1);
  move_value(context->allocator, builder, source_range, &result->storage, &one);
  push_instruction(instructions, *source_range, (Instruction) {
    .type = Instruction_Type_Label,
    .label = label,
  });
  return result;
}

Value *
ensure_memory(
  Allocator *allocator,
  Value *value
) {
  Storage operand = value->storage;
  if (operand.tag == Storage_Tag_Memory) return value;
  Value *result = allocator_allocate(allocator, Value);
  if (value->descriptor->tag != Descriptor_Tag_Pointer) assert(!"Not implemented");
  if (value->storage.tag != Storage_Tag_Register) assert(!"Not implemented");
  *result = (const Value) {
    .descriptor = value->descriptor->Pointer.to,
    .storage = {
      .tag = Storage_Tag_Memory,
      .Memory.location = {
        .tag = Memory_Location_Tag_Indirect,
        .Indirect = {
          .base_register = value->storage.Register.index
        }
      }
    },
  };
  return result;
}


