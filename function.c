#include "function.h"

#define CALLING_CONVENTION_IMPLEMENTATION
#include "calling_convention.h"

static Storage
reserve_stack_storage(
  Function_Builder *builder,
  Bits bit_size
) {
  s32 byte_size = u64_to_s32(bit_size.as_u64 / 8);
  builder->stack_reserve = s32_align(builder->stack_reserve, byte_size);
  builder->stack_reserve += byte_size;
  // The value is negative here because the stack grows down
  return storage_stack(-builder->stack_reserve, bit_size, Stack_Area_Local);
}

static const u64 registers_that_can_be_temp = (
  // FIXME this should be all registers except for RSP
  (1llu << Register_C) | (1llu << Register_B) | (1llu << Register_D) |
  (1llu << Register_BP) | (1llu << Register_SI) | (1llu << Register_DI) |
  (1llu << Register_R8) | (1llu << Register_R9) | (1llu << Register_R10) |
  (1llu << Register_R11) | (1llu << Register_R12) | (1llu << Register_R13) |
  (1llu << Register_R14) | (1llu << Register_R15)
);

static Register
register_find_available(
  Function_Builder *builder,
  u64 disallowed_bit_mask
) {
  // Start with the registers that we can theoretically use for temp values
  u64 available_bit_set = registers_that_can_be_temp;
  // Narrow it down by the ones that are not in use
  available_bit_set &= ~builder->register_occupied_bitset;
  // Apply any additional constraints from the user
  available_bit_set &= ~disallowed_bit_mask;

  u32 available_index = u64_count_trailing_zeros(available_bit_set);
  if (available_index == 64) {
    panic("TODO: Could not find an empty temp register");
  }
  return available_index;
}

static void
move_value(
  Function_Builder *builder,
  const Source_Range *source_range,
  const Storage *target,
  const Storage *source
) {
  if (target == source) return;
  if (storage_equal(target, source)) return;

  if (target->tag == Storage_Tag_Eflags) {
    panic("Internal Error: Trying to move into Eflags");
  }

  push_instruction(&builder->code_block, (Instruction) {
    .tag = Instruction_Tag_Location,
    .Location = { .source_range = *source_range },
  });

  u64 target_bit_size = target->bit_size.as_u64;
  u64 source_bit_size = source->bit_size.as_u64;

  if (target->tag == Storage_Tag_Xmm || source->tag == Storage_Tag_Xmm) {
    assert(target_bit_size == source_bit_size);
    if (target_bit_size == 32) {
      push_eagerly_encoded_assembly_no_source_range(
        &builder->code_block, *source_range, &(Instruction_Assembly){movss, {*target, *source}}
      );
    } else if (target_bit_size == 64) {
      push_eagerly_encoded_assembly_no_source_range(
        &builder->code_block, *source_range, &(Instruction_Assembly){movsd, {*target, *source}}
      );
    } else {
      panic("Internal Error: XMM operand of unexpected size");
    }
    return;
  }

  if (source->tag == Storage_Tag_Eflags) {
    assert(storage_is_register_or_memory(target));
    Storage temp = *target;
    if (source_bit_size != 8) {
      temp = (Storage) {
        .tag = Storage_Tag_Register,
        .bit_size = {8},
        .Register.index = register_acquire_temp(builder),
      };
    }
    switch(source->Eflags.compare_type) {
      case Compare_Type_Equal: {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, *source_range, &(Instruction_Assembly){sete, {temp}}
        );
        break;
      }
      case Compare_Type_Not_Equal: {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, *source_range, &(Instruction_Assembly){setne, {temp}}
        );
        break;
      }

      case Compare_Type_Unsigned_Below: {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, *source_range, &(Instruction_Assembly){setb, {temp}}
        );
        break;
      }
      case Compare_Type_Unsigned_Below_Equal: {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, *source_range, &(Instruction_Assembly){setbe, {temp}}
        );
        break;
      }
      case Compare_Type_Unsigned_Above: {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, *source_range, &(Instruction_Assembly){seta, {temp}}
        );
        break;
      }
      case Compare_Type_Unsigned_Above_Equal: {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, *source_range, &(Instruction_Assembly){setae, {temp}}
        );
        break;
      }

      case Compare_Type_Signed_Less: {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, *source_range, &(Instruction_Assembly){setl, {temp}}
        );
        break;
      }
      case Compare_Type_Signed_Less_Equal: {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, *source_range, &(Instruction_Assembly){setle, {temp}}
        );
        break;
      }
      case Compare_Type_Signed_Greater: {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, *source_range, &(Instruction_Assembly){setg, {temp}}
        );
        break;
      }
      case Compare_Type_Signed_Greater_Equal: {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, *source_range, &(Instruction_Assembly){setge, {temp}}
        );
        break;
      }
      default: {
        assert(!"Unsupported comparison");
      }
    }
    if (!storage_equal(&temp, target)) {
      assert(temp.tag == Storage_Tag_Register);
      Storage resized_temp = temp;
      resized_temp.bit_size = target->bit_size;
      push_eagerly_encoded_assembly_no_source_range(
        &builder->code_block, *source_range, &(Instruction_Assembly){movsx, {resized_temp, temp}}
      );
      push_eagerly_encoded_assembly_no_source_range(
        &builder->code_block, *source_range, &(Instruction_Assembly){mov, {*target, resized_temp}}
      );
      register_release(builder, temp.Register.index);
    }
    return;
  }
  if (source->tag == Storage_Tag_Register && source->Register.offset_in_bits != 0) {
    assert(source_bit_size <= 32);
    assert(source->Register.offset_in_bits <= 32);
    Storage temp_full_register = {
      .tag = Storage_Tag_Register,
      .bit_size = {64},
      .Register.index = register_acquire_temp(builder),
    };
    Storage source_full_register = {
      .tag = Storage_Tag_Register,
      .bit_size = {64},
      .Register.index = source->Register.index,
    };
    push_eagerly_encoded_assembly_no_source_range(
      &builder->code_block, *source_range,
      &(Instruction_Assembly){mov, {temp_full_register, source_full_register}}
    );
    push_eagerly_encoded_assembly_no_source_range(
      &builder->code_block, *source_range,
      &(Instruction_Assembly){shr, {temp_full_register, imm8((u8)source->Register.offset_in_bits)}}
    );

    Storage right_size_temp = temp_full_register;
    right_size_temp.bit_size = source->bit_size;
    move_value(builder, source_range, target, &right_size_temp);
    register_release(builder, temp_full_register.Register.index);
    return;
  }

  if (target->tag == Storage_Tag_Register && target->Register.packed) {
    assert(source_bit_size <= 32);
    assert(target->Register.offset_in_bits <= 32);
    if (source->tag == Storage_Tag_Register && source->Register.offset_in_bits != 0) {
      panic("Expected unpacking to be handled by the recursion above");
    }
    s64 clear_mask = ~(((1ll << source_bit_size) - 1) << target->Register.offset_in_bits);
    Storage temp_full_register = {
      .tag = Storage_Tag_Register,
      .bit_size = {64},
      .Register.index = register_acquire_temp(builder),
    };
    Storage target_full_register = {
      .tag = Storage_Tag_Register,
      .bit_size = {64},
      .Register.index = target->Register.index,
    };

    // Clear bits from the target register
    {
      push_eagerly_encoded_assembly_no_source_range(
        &builder->code_block, *source_range,
        &(Instruction_Assembly){mov, {temp_full_register, imm64(clear_mask)}}
      );
      push_eagerly_encoded_assembly_no_source_range(
        &builder->code_block, *source_range,
        &(Instruction_Assembly){and, {target_full_register, temp_full_register}}
      );
    }

    // Prepare new bits from the source register
    {
      push_eagerly_encoded_assembly_no_source_range(
        &builder->code_block, *source_range,
        &(Instruction_Assembly){xor, {temp_full_register, temp_full_register}}
      );
      Storage right_size_temp = temp_full_register;
      right_size_temp.bit_size = source->bit_size;
      move_value(builder, source_range, &right_size_temp, source);
      if (target->Register.offset_in_bits) {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, *source_range,
          &(Instruction_Assembly){shl, {temp_full_register, imm8((u8)target->Register.offset_in_bits)}}
        );
      }
    }

    // Merge new bits into the target register
    push_eagerly_encoded_assembly_no_source_range(
      &builder->code_block, *source_range,
      &(Instruction_Assembly){or, {target_full_register, temp_full_register}}
    );
    register_release(builder, temp_full_register.Register.index);
    return;
  }

  if (source->tag == Storage_Tag_Static) {
    assert(source->bit_size.as_u64 <= 64);
    s64 immediate = storage_static_value_up_to_s64(source);
    if (immediate == 0 && target->tag == Storage_Tag_Register) {
      // This messes up flags register so comparisons need to be aware of this optimization
      push_eagerly_encoded_assembly_no_source_range(
        &builder->code_block, *source_range, &(Instruction_Assembly){xor, {*target, *target}}
      );
      return;
    }
    Storage adjusted_source;
    switch(target_bit_size) {
      case 8: {
        adjusted_source = imm8(s64_to_s8(immediate));
        break;
      }
      case 16: {
        adjusted_source = imm16(s64_to_s16(immediate));
        break;
      }
      case 32: {
        adjusted_source = imm32(s64_to_s32(immediate));
        break;
      }
      case 64: {
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
    bool is_64bit_immediate = adjusted_source.bit_size.as_u64 == 64;
    if (is_64bit_immediate && target->tag != Storage_Tag_Register) {
      Storage temp = {
        .tag = Storage_Tag_Register,
        .bit_size = adjusted_source.bit_size,
        .Register.index = register_acquire_temp(builder),
      };
      push_eagerly_encoded_assembly_no_source_range(
        &builder->code_block, *source_range, &(Instruction_Assembly){mov, {temp, adjusted_source}}
      );
      push_eagerly_encoded_assembly_no_source_range(
        &builder->code_block, *source_range, &(Instruction_Assembly){mov, {*target, temp}}
      );
      register_release(builder, temp.Register.index);
    } else {
      push_eagerly_encoded_assembly_no_source_range(
        &builder->code_block, *source_range, &(Instruction_Assembly){mov, {*target, adjusted_source}}
      );
    }
    return;
  }

  assert(target_bit_size == source_bit_size);

  if (target->tag == Storage_Tag_Memory && source->tag == Storage_Tag_Memory) {
    Storage temp = {
      .tag = Storage_Tag_Register,
      .bit_size = target->bit_size,
      .Register.index = register_acquire_temp(builder),
    };
    // TODO avoid and extra source range push for recursion
    move_value(builder, source_range, &temp, source);
    move_value(builder, source_range, target, &temp);
    register_release(builder, temp.Register.index);
    return;
  }

  push_eagerly_encoded_assembly_no_source_range(
    &builder->code_block, *source_range, &(Instruction_Assembly){mov, {*target, *source}}
  );
}

static inline u32
make_trampoline(
  Virtual_Memory_Buffer *buffer,
  s64 address
) {
  u32 result = u64_to_u32(buffer->occupied);
  Storage rax = storage_register(Register_A, (Bits){64});
  encode_and_write_assembly(buffer, &(Instruction_Assembly) {mov, {rax, imm64(address)}});
  encode_and_write_assembly(buffer, &(Instruction_Assembly) {jmp, {rax}});
  return result;
}

static void
fn_encode(
  Program *program,
  Virtual_Memory_Buffer *buffer,
  const Function_Builder *builder,
  Function_Layout *out_layout
) {
  Label *label = builder->code_block.start_label;
  assert(!label->resolved);

  *out_layout = (Function_Layout) {
    .stack_reserve = builder->stack_reserve,
  };

  s64 code_base_rva = label->section->base_rva;
  out_layout->begin_rva = u64_to_u32(code_base_rva + buffer->occupied);
  Storage stack_size_operand = imm_auto_8_or_32(out_layout->stack_reserve);
  program_resolve_label(program, buffer, label);

  // :RegisterPushPop
  // :Win32UnwindCodes Must match what happens in the unwind code generation
  // Push non-volatile registers (in reverse order)
  u8 push_index = 0;
  for (s32 reg_index = Register_R15; reg_index >= Register_A; --reg_index) {
    if (register_bitset_get(builder->register_used_bitset, reg_index)) {
      if (!register_bitset_get(builder->register_volatile_bitset, reg_index)) {
        out_layout->volatile_register_push_offsets[push_index++] =
          u64_to_u8(code_base_rva + buffer->occupied - out_layout->begin_rva);
        Storage to_save = storage_register(reg_index, (Bits){64});
        encode_and_write_assembly(buffer, &(Instruction_Assembly) {push, {to_save}});
      }
    }
  }

  Storage rsp = storage_register(Register_SP, (Bits){64});
  encode_and_write_assembly(buffer, &(Instruction_Assembly) {sub, {rsp, stack_size_operand}});
  out_layout->stack_allocation_offset_in_prolog =
    u64_to_u8(code_base_rva + buffer->occupied -out_layout->begin_rva);
  out_layout->size_of_prolog =
    u64_to_u8(code_base_rva + buffer->occupied - out_layout->begin_rva);

  for (Instruction_Bucket *bucket = builder->code_block.first_bucket; bucket; bucket = bucket->next) {
    for (u64 i = 0; i < bucket->length; ++i) {
      Instruction *instruction = &bucket->items[i];
      encode_instruction(program, buffer, instruction);
    }
  }

  encode_and_write_assembly(buffer, &(Instruction_Assembly) {add, {rsp, stack_size_operand}});

  // :RegisterPushPop
  // Pop non-volatile registers (in original order)
  for (Register reg_index = 0; reg_index <= Register_R15; ++reg_index) {
    if (register_bitset_get(builder->register_used_bitset, reg_index)) {
      if (!register_bitset_get(builder->register_volatile_bitset, reg_index)) {
        Storage to_save = storage_register(reg_index, (Bits){64});
        encode_and_write_assembly(buffer, &(Instruction_Assembly) {pop, {to_save}});
      }
    }
  }

  encode_and_write_assembly(buffer, &(Instruction_Assembly) {ret});
  out_layout->end_rva = u64_to_u32(code_base_rva + buffer->occupied);
}

static void
encode_inverted_conditional_jump(
  Function_Builder *builder,
  Label *to_label,
  const Source_Range *source_range,
  Value *value
) {
  if (value->storage.tag == Storage_Tag_Eflags) {
    const X64_Mnemonic *mnemonic = 0;
    switch(value->storage.Eflags.compare_type) {
      case Compare_Type_Equal: mnemonic = jne; break;
      case Compare_Type_Not_Equal: mnemonic = je; break;

      case Compare_Type_Unsigned_Below: mnemonic = jae; break;
      case Compare_Type_Unsigned_Below_Equal: mnemonic = ja; break;
      case Compare_Type_Unsigned_Above: mnemonic = jbe; break;
      case Compare_Type_Unsigned_Above_Equal: mnemonic = jb; break;

      case Compare_Type_Signed_Less: mnemonic = jge; break;
      case Compare_Type_Signed_Less_Equal: mnemonic = jg; break;
      case Compare_Type_Signed_Greater: mnemonic = jle; break;
      case Compare_Type_Signed_Greater_Equal: mnemonic = jl; break;
      default: assert(!"Unsupported comparison"); break;
    }
    push_eagerly_encoded_assembly(
      &builder->code_block, *source_range,
      &(Instruction_Assembly){mnemonic, {code_label32(to_label)}}
    );
  } else {
    if (value->storage.tag == Storage_Tag_Register) {
      Storage test_storage = value->storage;
      bool is_packed = value->storage.Register.offset_in_bits != 0;
      if (is_packed) {
        test_storage = storage_register(register_acquire_temp(builder), value->descriptor->bit_size);
        move_value(builder, source_range, &test_storage, &value->storage);
      }
      push_eagerly_encoded_assembly(
        &builder->code_block, *source_range,
        &(Instruction_Assembly){x64_test, {test_storage, test_storage}}
      );
      if (is_packed) register_release(builder, test_storage.Register.index);
    } else {
      u64 bit_size = value->descriptor->bit_size.as_u64;
      if (bit_size == 32 || bit_size == 64) {
        push_eagerly_encoded_assembly(
          &builder->code_block, *source_range,
          &(Instruction_Assembly){cmp, {value->storage, imm32(0)}}
        );
      } else if (bit_size == 8) {
        push_eagerly_encoded_assembly(
          &builder->code_block, *source_range,
          &(Instruction_Assembly){cmp, {value->storage, imm8(0)}}
        );
      } else {
        assert(!"Unsupported value inside `if`");
      }
    }
    push_eagerly_encoded_assembly(
      &builder->code_block, *source_range,
      &(Instruction_Assembly){jz, {code_label32(to_label)}}
    );
  }
}

static Value *
maybe_constant_fold_internal(
  Compilation *compilation,
  Function_Builder *builder,
  s64 constant_result,
  const Expected_Result *expected_result,
  const Source_Range *source_range
) {
  const Descriptor *descriptor = expected_result_descriptor(expected_result);
  if (!descriptor) descriptor = &descriptor_s64;
  Storage imm_storage;
  switch(descriptor->bit_size.as_u64) {
    case 8: imm_storage = imm8(s64_to_s8(constant_result)); break;
    case 16: imm_storage = imm16(s64_to_s16(constant_result)); break;
    case 32: imm_storage = imm32(s64_to_s32(constant_result)); break;
    case 64: imm_storage = imm64(s64_to_s64(constant_result)); break;
    default: imm_storage = (Storage){0}; panic("Unexpected operand size"); break;
  }
  Value *imm_value = value_init(
    allocator_allocate(compilation->allocator, Value),
    descriptor, imm_storage, *source_range
  );
  return expected_result_ensure_value_or_temp(compilation, builder, expected_result, imm_value);
}

static inline Storage
storage_adjusted_for_lea(
  Storage source
) {
  assert(source.tag == Storage_Tag_Memory);
  // `LEA` is a weird instruction in that the size of the operands affects
  // what the instruction *does*, instead of describing the operands.
  // For the purposes of this compiler we always want it to generate 64-bit
  // effective address and then store that full address in the target register.
  // This is why here we are forcing the source memory operand to be 8 bytes.
  Storage adjusted_source = source;
  adjusted_source.bit_size.as_u64 = 64;
  return adjusted_source;
}

static void
load_address(
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *result_value,
  Storage source
) {
  assert(
    result_value->descriptor->tag == Descriptor_Tag_Pointer_To ||
    result_value->descriptor->tag == Descriptor_Tag_Function_Instance
  );

  bool can_reuse_result_as_temp = result_value->storage.tag == Storage_Tag_Register;
  Storage register_storage = can_reuse_result_as_temp
    ? result_value->storage
    : storage_register(register_acquire_temp(builder), result_value->descriptor->bit_size);

  assert(register_storage.bit_size.as_u64 == 64);
  push_eagerly_encoded_assembly(
    &builder->code_block, *source_range,
    &(Instruction_Assembly){lea, {register_storage, storage_adjusted_for_lea(source)}}
  );

  if (!can_reuse_result_as_temp) {
    assert(register_storage.tag == Storage_Tag_Register);
    move_value(builder, source_range, &result_value->storage, &register_storage);
    register_release(builder, register_storage.Register.index);
  }
}

static void
load_address_to_indirect(
  Function_Builder *builder,
  const Source_Range *source_range,
  Storage target,
  Storage source
) {
  assert(target.tag == Storage_Tag_Memory);
  assert(target.Memory.location.tag == Memory_Location_Tag_Indirect);
  assert(target.Memory.location.Indirect.offset == 0);
  assert(source.tag == Storage_Tag_Memory);

  Storage register_storage = {
    .tag = Storage_Tag_Register,
    .Register.index = target.Memory.location.Indirect.base_register,
    .bit_size = {64},
  };

  push_eagerly_encoded_assembly(
    &builder->code_block, *source_range,
    &(Instruction_Assembly){lea, {register_storage, storage_adjusted_for_lea(source)}}
  );
}

static void
mark_occupied_registers(
  Function_Builder *builder,
  const Storage *stack_argument_base,
  const Descriptor *descriptor,
  Storage *storage
) {
  switch(storage->tag) {
    case Storage_Tag_None: {
      // Nothing to do
      break;
    }
    case Storage_Tag_Unpacked: {
      register_bitset_set(&builder->register_occupied_bitset, storage->Unpacked.registers[0]);
      register_bitset_set(&builder->register_occupied_bitset, storage->Unpacked.registers[1]);
      break;
    }
    case Storage_Tag_Register:
    case Storage_Tag_Xmm: {
      register_bitset_set(&builder->register_occupied_bitset, storage->Register.index);
      break;
    }
    case Storage_Tag_Memory: {
      switch(storage->Memory.location.tag) {
        case Memory_Location_Tag_Instruction_Pointer_Relative: {
          panic("Unsupported argument memory storage");
          break;
        }
        case Memory_Location_Tag_Indirect: {
          Register reg = storage->Memory.location.Indirect.base_register;
          register_bitset_set(&builder->register_occupied_bitset, reg);
          break;
        }
        case Memory_Location_Tag_Stack: {
          // Nothing to do
          break;
        }
      }
      break;
    }
    case Storage_Tag_Static:
    case Storage_Tag_Eflags: {
      panic("Unexpected storage tag for an argument");
      break;
    }
  }
}

static inline Register
function_return_value_register_from_storage(
  const Storage *storage
) {
  switch(storage->tag) {
    case Storage_Tag_Register: {
      return storage->Register.index;
    }
    case Storage_Tag_Xmm: {
      return storage->Xmm.index;
    }
    case Storage_Tag_Memory: {
      switch(storage->Memory.location.tag) {
        case Memory_Location_Tag_Stack:
        case Memory_Location_Tag_Instruction_Pointer_Relative: {
          break;
        }
        case Memory_Location_Tag_Indirect: {
          return storage->Memory.location.Indirect.base_register;
        }
      }
      break;
    }
    case Storage_Tag_None:
    case Storage_Tag_Unpacked:
    case Storage_Tag_Static:
    case Storage_Tag_Eflags: {
      break;
    }
  }
  panic("Unexpected storage for a return value");
  return 0;
}

static Value *
ensure_function_instance(
  Compilation *compilation,
  Program *program,
  Value *fn_value,
  Value_View args
) {
  if (fn_value->descriptor->tag == Descriptor_Tag_Function_Instance) {
    return fn_value;
  }

  // TODO figure out how to avoid the const cast here
  Function_Literal *literal = (Function_Literal *)value_as_function_literal(fn_value);
  assert(!(literal->flags & Function_Literal_Flags_Macro));
  const Function_Info *fn_info = function_literal_info_for_args(literal, args);

  const Calling_Convention *calling_convention = program->default_calling_convention;

  if (!dyn_array_is_initialized(literal->instances)) {
    literal->instances = dyn_array_make(
      Array_Value_Ptr,
      .allocator = compilation->allocator,
      .capacity = 4
    );
  }

  for (u64 i = 0; i < dyn_array_length(literal->instances); ++i) {
    Value *instance_value = *dyn_array_get(literal->instances, i);
    assert(instance_value->descriptor->tag == Descriptor_Tag_Function_Instance);
    const Descriptor_Function_Instance *instance = &instance_value->descriptor->Function_Instance;
    if (instance->info != fn_info) continue;
    if (instance->call_setup.calling_convention != calling_convention) {
      continue;
    }
    return instance_value;
  }

  Slice fn_name = fn_value->descriptor->name.length
    ? fn_value->descriptor->name
    : slice_literal("__anonymous__");

  MASS_ON_ERROR(*compilation->result) return 0;

  Function_Call_Setup call_setup = calling_convention->call_setup_proc(compilation->allocator, fn_info);
  const Descriptor *instance_descriptor =
    descriptor_function_instance(compilation->allocator, fn_name, fn_info, call_setup);

  if (value_is_external_symbol(literal->body)) {
    const External_Symbol *symbol = storage_static_as_c_type(&literal->body->storage, External_Symbol);
    Storage storage = import_symbol(compilation->allocator, program, symbol->library_name, symbol->symbol_name);
    Value *cached_instance = value_init(
      allocator_allocate(compilation->allocator, Value),
      instance_descriptor, storage, fn_value->source_range
    );
    dyn_array_push(literal->instances, cached_instance);
    return cached_instance;
  }

  Label *call_label = make_label(compilation->allocator, program, &program->memory.code, fn_name);
  // It is important to cache the label here for recursive calls
  Value *cached_instance = value_init(
    allocator_allocate(compilation->allocator, Value),
    instance_descriptor, code_label32(call_label), fn_value->source_range
  );
  dyn_array_push(literal->instances, cached_instance);

  Execution_Context body_context = execution_context_from_compilation(compilation);
  Scope *body_scope = scope_make(compilation->allocator, literal->context.scope);
  body_context.flags &= ~Execution_Context_Flags_Global;
  body_context.scope = body_scope;
  body_context.program = program;
  body_context.epoch = get_new_epoch();

  Slice end_label_pieces[] = {fn_name, slice_literal(":end")};
  Slice end_label_name = slice_join(compilation->allocator, end_label_pieces, countof(end_label_pieces));

  const Descriptor *return_descriptor = fn_info->returns.declaration.descriptor;
  Storage return_storage = instance_descriptor->Function_Instance.call_setup.callee_return;
  Value *return_value = value_init(
    allocator_allocate(compilation->allocator, Value),
    return_descriptor, return_storage, fn_info->returns.declaration.source_range
  );

  Function_Builder *builder = &(Function_Builder){
    .program = program,
    .epoch = body_context.epoch,
    .function = fn_info,
    .register_volatile_bitset = calling_convention->register_volatile_bitset,
    .return_value = return_value,
    .code_block = {
      .allocator = compilation->allocator,
      .start_label = call_label,
      .end_label = make_label(compilation->allocator, program, &program->memory.code, end_label_name),
    },
  };

  {
    const Memory_Layout *arguments_layout = &call_setup.arguments_layout;
    Storage stack_argument_base = storage_stack(0, (Bits){8}, Stack_Area_Received_Argument);
    DYN_ARRAY_FOREACH(Memory_Layout_Item, item, arguments_layout->items) {
      Storage storage = memory_layout_item_storage(&stack_argument_base, arguments_layout, item);
      Value *arg_value = value_make(&body_context, item->descriptor, storage, item->source_range);
      arg_value->flags |= Value_Flags_Constant;
      if (item->name.length) {
        // TODO figure out how to avoid this lookup
        const Symbol *item_symbol = mass_ensure_symbol(compilation, item->name);
        scope_define_value(body_scope, body_context.epoch, item->source_range, item_symbol, arg_value);
      }
      mark_occupied_registers(builder, &stack_argument_base, arg_value->descriptor, &arg_value->storage);
    }
  }

  // Return value can be named in which case it should be accessible in the fn body
  if (fn_info->returns.declaration.symbol) {
    scope_define_value(
      body_scope,
      body_context.epoch,
      return_value->source_range,
      fn_info->returns.declaration.symbol,
      return_value
    );
  }
  Value *parse_result = 0;
  if (value_is_group_curly(literal->body)) {
    parse_result = token_parse_block_no_scope(&body_context, value_as_group_curly(literal->body));
  } else if (literal->body->descriptor == &descriptor_value_view) {
    const Value_View *view = storage_static_as_c_type(&literal->body->storage, Value_View);
    parse_result = token_parse_block_view(&body_context, *view);
  } else if (literal->body->descriptor == &descriptor_lazy_value) {
    parse_result = literal->body;
  } else {
    panic("Unexpected function body type");
  }
  MASS_ON_ERROR(*compilation->result) return 0;

  value_force_exact(compilation, builder, return_value, parse_result);

  push_instruction(&builder->code_block, (Instruction) {
    .tag = Instruction_Tag_Label,
    .Label.pointer = builder->code_block.end_label,
  });

  if (!storage_equal(&call_setup.callee_return, &call_setup.caller_return)) {
    Register caller_register = function_return_value_register_from_storage(&call_setup.caller_return);
    Register callee_register = function_return_value_register_from_storage(&call_setup.callee_return);
    Storage callee_register_storage = storage_register(callee_register, (Bits){64});
    Storage caller_register_storage = storage_register(caller_register, (Bits){64});
    push_eagerly_encoded_assembly(
      &builder->code_block, return_value->source_range,
      &(Instruction_Assembly){mov, {caller_register_storage, callee_register_storage}}
    );
  }

  calling_convention_x86_64_common_end_proc(program, builder);

  // Only push the builder at the end to avoid problems in nested JIT compiles
  dyn_array_push(program->functions, *builder);

  return cached_instance;
}


static s64
calculate_arguments_match_score(
  const Function_Info *descriptor,
  Value_View args_view
) {
  enum {
    Score_Exact_Static = 1000 * 1000 * 1000,
    Score_Exact_Type = 1000 * 1000,
    Score_Exact_Default = 1000,
    Score_Cast = 1,
  };
  assert(args_view.length < 1000);
  s64 score = 0;
  if (args_view.length > dyn_array_length(descriptor->parameters)) return -1;
  for (u64 arg_index = 0; arg_index < dyn_array_length(descriptor->parameters); ++arg_index) {
    Function_Parameter *param = dyn_array_get(descriptor->parameters, arg_index);
    const Descriptor *target_descriptor = param->declaration.descriptor;
    Value *source_arg = 0;
    const Descriptor *source_descriptor;
    if (arg_index >= args_view.length) {
      if (!param->maybe_default_value) return -1;
      source_descriptor = target_descriptor;
    } else {
      source_arg = value_view_get(args_view, arg_index);
      source_descriptor = value_or_lazy_value_descriptor(source_arg);
    }
    switch(param->tag) {
      case Function_Parameter_Tag_Runtime: {
        if (same_type(target_descriptor, source_descriptor)) {
          score += Score_Exact_Type;
        } else if (
          source_arg &&
          same_value_type_or_can_implicitly_move_cast(target_descriptor, source_arg)
        ) {
          score += Score_Cast;
        } else {
          return -1;
        }
      } break;
      case Function_Parameter_Tag_Exact_Static: {
        if (!source_arg || !value_is_non_lazy_static(source_arg)) return -1;
        if (!storage_static_equal(
          target_descriptor, &param->Exact_Static.storage,
          source_arg->descriptor, &source_arg->storage
        )) return -1;
        score += Score_Exact_Static;
      } break;
      case Function_Parameter_Tag_Generic: {
        score += Score_Cast; // TODO consider if implicit casts actually have a higher priority
      } break;
    }
  }
  return score;
}

static void
program_init_startup_code(
  Execution_Context *context
) {
  Program *program = context->program;
  Compilation *compilation = context->compilation;
  Function_Info *fn_info = allocator_allocate(context->allocator, Function_Info);
  function_info_init(fn_info);
  const Calling_Convention *calling_convention =
    context->compilation->runtime_program->default_calling_convention;
  Slice fn_name = slice_literal("__startup");
  Function_Call_Setup call_setup = calling_convention->call_setup_proc(context->allocator, fn_info);
  Descriptor *descriptor =
    descriptor_function_instance(context->allocator, fn_name, fn_info, call_setup);
  Label *fn_label = make_label(context->allocator, program, &program->memory.code, fn_name);
  Label *end_label =
    make_label(context->allocator, program, &program->memory.code, slice_literal("__startup end"));
  Storage storage = code_label32(fn_label);

  Source_Range source_range;
  INIT_LITERAL_SOURCE_RANGE(&source_range, "__startup");
  Value *function = value_make(context, descriptor, storage, source_range);

  Function_Builder builder = (Function_Builder){
    .program = program,
    .epoch = get_new_epoch(),
    .function = fn_info,
    .code_block = {
      .allocator = context->allocator,
      .start_label = fn_label,
      .end_label = end_label,
    },
  };

  // Resolve relocations
  Storage register_a = storage_register(Register_A, (Bits){64});
  u64 relocation_count = dyn_array_length(program->relocations);
  for (u64 i = 0; i < relocation_count; ++i) {
    Relocation *relocation = dyn_array_get(program->relocations, i);
    push_eagerly_encoded_assembly(
      &builder.code_block, source_range,
      &(Instruction_Assembly){lea, {register_a, relocation->address_of}}
    );
    push_eagerly_encoded_assembly(
      &builder.code_block, source_range,
      &(Instruction_Assembly){mov, {relocation->patch_at, register_a}}
    );
  }

  for (u64 i = 0; i < dyn_array_length(context->program->startup_functions); ++i) {
    Value *fn = *dyn_array_get(context->program->startup_functions, i);
    Value *instance = ensure_function_instance(compilation, program, fn, (Value_View){0});
    push_eagerly_encoded_assembly(
      &builder.code_block, source_range,
      &(Instruction_Assembly){call, {instance->storage}}
    );
  }
  Value *entry_instance =
    ensure_function_instance(compilation, program, program->entry_point, (Value_View){0});
  push_eagerly_encoded_assembly(
    &builder.code_block, source_range,
    &(Instruction_Assembly){jmp, {entry_instance->storage}}
  );

  program->entry_point = function;
  dyn_array_push(program->functions, builder);
}

