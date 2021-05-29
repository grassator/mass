#include "function.h"

static Storage
reserve_stack_storage(
  Function_Builder *builder,
  u64 raw_byte_size
) {
  s32 byte_size = u64_to_s32(raw_byte_size);
  builder->stack_reserve = s32_align(builder->stack_reserve, byte_size);
  builder->stack_reserve += byte_size;
  // The value is negative here because the stack grows down
  return storage_stack_local(-builder->stack_reserve, raw_byte_size);
}

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
  Function_Builder *builder,
  u64 register_disallowed_bit_mask
) {
  // FIXME this should be all registers except for RSP
  static const Register temp_registers[] = {
    Register_C, Register_B, Register_D, Register_BP, Register_SI, Register_DI,
    Register_R8, Register_R9, Register_R10, Register_R11,
    Register_R12, Register_R13, Register_R14, Register_R15
  };
  for (u32 i = 0; i < countof(temp_registers); ++i) {
    Register reg_index = temp_registers[i];
    if (register_bitset_get(register_disallowed_bit_mask, reg_index)) continue;
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
  return register_acquire(builder, register_find_available(builder, 0));
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
  Register reg_index,
  u64 register_disallowed_bit_mask
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

  result.saved_index = register_find_available(builder, register_disallowed_bit_mask);
  register_acquire(builder, result.saved_index);
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
      static const s32 chunk_size = 4;
      assert(source->byte_size % chunk_size == 0);
      // TODO can there be something else?
      assert(source->byte_size == target->byte_size);
      for (s32 offset = 0; offset < u64_to_s32(source->byte_size); offset += chunk_size) {
        Storage adjusted_target = storage_adjusted_memory_location(target, offset);
        adjusted_target.byte_size = chunk_size;
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
      print_storage(target);
      printf(" ");
      print_storage(source);
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

static void
fn_encode(
  Program *program,
  Virtual_Memory_Buffer *buffer,
  const Function_Builder *builder,
  Function_Layout *out_layout
) {
  assert(!(builder->function->flags & Descriptor_Function_Flags_Macro));

  Label_Index label_index = builder->code_block.start_label;
  Label *label = program_get_label(program, label_index);
  assert(!label->resolved);

  *out_layout = (Function_Layout) {
    .stack_reserve = builder->stack_reserve,
  };

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
  assert(result_value->descriptor->tag == Descriptor_Tag_Pointer_To);
  assert(source.tag == Storage_Tag_Memory);

  bool can_reuse_result_as_temp = result_value->storage.tag == Storage_Tag_Register;
  Storage temp_storage = can_reuse_result_as_temp
    ? result_value->storage
    : storage_register_for_descriptor(register_acquire_temp(builder), result_value->descriptor);

  push_instruction(
    &builder->code_block.instructions, *source_range,
    (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {lea, {temp_storage, source, 0}}}
  );

  if (!can_reuse_result_as_temp) {
    assert(temp_storage.tag == Storage_Tag_Register);
    register_release(builder, temp_storage.Register.index);
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
      if (storage->tag == Storage_Tag_Memory) {
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
  // Inside the function large returns are pointed to by RCX,
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
      if (storage->tag == Storage_Tag_Memory) {
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

static Value *
ensure_function_instance(
  Execution_Context *context,
  Value *fn_value
) {
  if (fn_value->descriptor->tag == Descriptor_Tag_Function_Instance) {
    return fn_value;
  }

  const Descriptor *descriptor = fn_value->descriptor;
  assert(descriptor == &descriptor_function_literal);
  // TODO figure out how to avoid the const cast here
  Function_Literal *literal = (Function_Literal *)storage_static_as_c_type(&fn_value->storage, Function_Literal);
  Function_Info *function = literal->info;

  assert(!(function->flags & Descriptor_Function_Flags_Macro));
  Value **cached_instance = context_is_compile_time_eval(context)
    ? &literal->compile_time_instance
    : &literal->runtime_instance;

  if (cached_instance[0]) return cached_instance[0];

  Program *program = context->program;
  const Calling_Convention *calling_convention = program->default_calling_convention;

  const Function_Return *returns = &function->returns;
  Storage return_storage = calling_convention->return_storage_proc(function, Function_Argument_Mode_Body);
  Value *return_value = value_make(context, returns->descriptor, return_storage, returns->source_range);

  Slice fn_name = fn_value->descriptor->name.length
    ? fn_value->descriptor->name
    : slice_literal("__anonymous__");

  const Descriptor *instance_descriptor = descriptor_function_instance(
    context->allocator, fn_name, function, calling_convention
  );

  const Memory_Layout *arguments_layout = &instance_descriptor->Function_Instance.arguments_layout;

  if (value_is_external_symbol(literal->body)) {
    const External_Symbol *symbol = storage_static_as_c_type(&literal->body->storage, External_Symbol);
    Storage storage = import_symbol(context, symbol->library_name, symbol->symbol_name);
    *cached_instance = value_make(context, instance_descriptor, storage, fn_value->source_range);
    return cached_instance[0];
  }

  Label_Index call_label = make_label(program, &program->memory.code, fn_name);
  // It is important to cache the label here for recursive calls
  *cached_instance =
    value_make(context, instance_descriptor, code_label32(call_label), fn_value->source_range);

  Execution_Context body_context = *context;
  Scope *body_scope = scope_make(context->allocator, function->scope);
  body_context.flags &= ~Execution_Context_Flags_Global;
  body_context.scope = body_scope;
  body_context.epoch = get_new_epoch();

  Slice end_label_pieces[] = {fn_name, slice_literal(":end")};
  Slice end_label_name = slice_join(context->allocator, end_label_pieces, countof(end_label_pieces));

  Function_Builder *builder = &(Function_Builder){
    .function = function,
    .register_volatile_bitset = calling_convention->register_volatile_bitset,
    .return_value = return_value,
    .code_block = {
      .start_label = call_label,
      .end_label = make_label(program, &program->memory.code, end_label_name),
      .instructions = dyn_array_make(Array_Instruction, .allocator = context->allocator),
    },
  };

  {
    for(u64 i = 0; i < dyn_array_length(arguments_layout->items); ++i) {
      Memory_Layout_Item *item = dyn_array_get(arguments_layout->items, i);
      Storage storage = memory_layout_item_storage_at_index(arguments_layout, i);
      Value *arg_value = value_make(&body_context, item->descriptor, storage, item->source_range);
      if (item->name.length) {
        scope_define_value(body_scope, body_context.epoch, item->source_range, item->name, arg_value);
      }
      Register arg_reg = Register_SP;
      if (arg_value->storage.tag == Storage_Tag_Register) {
        arg_reg = arg_value->storage.Register.index;
      } else if(arg_value->storage.tag == Storage_Tag_Memory) {
        switch(arg_value->storage.Memory.location.tag) {
          case Memory_Location_Tag_Instruction_Pointer_Relative: {
            panic("Unsupported argument memory storage");
            break;
          }
          case Memory_Location_Tag_Indirect: {
            arg_reg = arg_value->storage.Memory.location.Indirect.base_register;
            break;
          }
          case Memory_Location_Tag_Stack: {
            arg_reg = Register_SP;
            break;
          }
        }
      } else {
        panic("Unexpected storage tag for an argument");
      }
      if (arg_reg != Register_SP) {
        register_bitset_set(&builder->register_occupied_bitset, arg_reg);
        builder->register_occupied_values[arg_reg] = arg_value;
      }
    }
  }

  // Return value can be named in which case it should be accessible in the fn body
  if (function->returns.name.length) {
    scope_define_value(body_scope, body_context.epoch, return_value->source_range, function->returns.name, return_value);
  }
  Value *parse_result = token_parse_block_no_scope(&body_context, literal->body);
  MASS_ON_ERROR(*context->result) return 0;

  value_force_exact(&body_context, builder, return_value, parse_result);

  push_instruction(
    &builder->code_block.instructions, return_value->source_range,
    (Instruction) { .tag = Instruction_Tag_Label, .Label.index = builder->code_block.end_label }
  );

  calling_convention->body_end_proc(program, builder);

  // Only push the builder at the end to avoid problems in nested JIT compiles
  dyn_array_push(program->functions, *builder);

  return *cached_instance;
}


s64
calculate_arguments_match_score(
  const Function_Info *descriptor,
  Array_Value_Ptr arguments
) {
  enum {
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
  const Calling_Convention *calling_convention =
    context->compilation->runtime_program->default_calling_convention;
  Slice fn_name = slice_literal("__startup");
  Descriptor *descriptor = descriptor_function_instance(
    context->allocator, fn_name, fn_info, calling_convention
  );
  Label_Index fn_label = make_label(program, &program->memory.code, fn_name);
  Storage storage = code_label32(fn_label);

  Source_Range source_range = COMPILER_SOURCE_RANGE;
  Value *function = value_make(context, descriptor, storage, source_range);

  Function_Builder builder = (Function_Builder){
    .function = fn_info,
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
    Value *instance = ensure_function_instance(context, fn);
    push_instruction(
      &builder.code_block.instructions, source_range,
      (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {call, {instance->storage, 0, 0}}}
    );
  }
  Value *entry_instance = ensure_function_instance(context, program->entry_point);
  push_instruction(
    &builder.code_block.instructions, source_range,
    (Instruction) {.tag = Instruction_Tag_Assembly, .Assembly = {jmp, {entry_instance->storage, 0, 0}}}
  );

  program->entry_point = function;
  dyn_array_push(program->functions, builder);
}

