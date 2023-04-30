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
  available_bit_set &= ~builder->register_occupied_bitset.bits;
  // Apply any additional constraints from the user
  available_bit_set &= ~disallowed_bit_mask;

  u32 available_index = u64_count_trailing_zeros(available_bit_set);
  if (available_index == 64) {
    panic("TODO: Could not find an empty temp register");
  }
  return available_index;
}

static u64
register_bitset_from_storage(
  const Storage *storage
) {
  u64 result = 0;
  switch(storage->tag) {
    case Storage_Tag_Static:
    case Storage_Tag_Immediate: {
      // Nothing to do
      break;
    }
    default:
    case Storage_Tag_Eflags: {
      panic("Internal Error: Unexpected storage type for a function argument");
      break;
    }
    case Storage_Tag_Register:
    case Storage_Tag_Xmm: {
      Register reg_index = storage->Register.index;
      register_bitset_set(&result, reg_index);
    } break;
    case Storage_Tag_Memory: {
      if (storage->Memory.location.tag == Memory_Location_Tag_Indirect) {
        Register reg_index = storage->Memory.location.Indirect.base_register;
        register_bitset_set(&result, reg_index);
      }
    } break;
    case Storage_Tag_Disjoint: {
      for (u64 i = 0; i < dyn_array_length(storage->Disjoint.pieces); ++i) {
        const Storage *piece = *dyn_array_get(storage->Disjoint.pieces, i);
        result |= register_bitset_from_storage(piece);
      }
    } break;
  }
  return result;
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

  if (source->tag == Storage_Tag_Static) {
    assert(source->bit_size.as_u64 <= 64);
    Storage immediate = imm64(0);
    immediate.bit_size = source->bit_size;
    const void *source_memory = get_static_storage_with_bit_size(source, source->bit_size);
    memcpy(&immediate.Immediate.bits, source_memory, immediate.bit_size.as_u64 / 8);
    move_value(builder, source_range, target, &immediate);
    return;
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
        &builder->code_block, &(Instruction_Assembly){movss, {*target, *source}} );
    } else if (target_bit_size == 64) {
      push_eagerly_encoded_assembly_no_source_range(
        &builder->code_block, &(Instruction_Assembly){movsd, {*target, *source}} );
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
          &builder->code_block, &(Instruction_Assembly){sete, {temp}} );
        break;
      }
      case Compare_Type_Not_Equal: {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, &(Instruction_Assembly){setne, {temp}} );
        break;
      }

      case Compare_Type_Unsigned_Below: {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, &(Instruction_Assembly){setb, {temp}} );
        break;
      }
      case Compare_Type_Unsigned_Below_Equal: {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, &(Instruction_Assembly){setbe, {temp}} );
        break;
      }
      case Compare_Type_Unsigned_Above: {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, &(Instruction_Assembly){seta, {temp}} );
        break;
      }
      case Compare_Type_Unsigned_Above_Equal: {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, &(Instruction_Assembly){setae, {temp}} );
        break;
      }

      case Compare_Type_Signed_Less: {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, &(Instruction_Assembly){setl, {temp}} );
        break;
      }
      case Compare_Type_Signed_Less_Equal: {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, &(Instruction_Assembly){setle, {temp}} );
        break;
      }
      case Compare_Type_Signed_Greater: {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, &(Instruction_Assembly){setg, {temp}} );
        break;
      }
      case Compare_Type_Signed_Greater_Equal: {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, &(Instruction_Assembly){setge, {temp}} );
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
        &builder->code_block, &(Instruction_Assembly){movsx, {resized_temp, temp}} );
      push_eagerly_encoded_assembly_no_source_range(
        &builder->code_block, &(Instruction_Assembly){mov, {*target, resized_temp}} );
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
      &builder->code_block, &(Instruction_Assembly){mov, {temp_full_register, source_full_register}}
    );
    push_eagerly_encoded_assembly_no_source_range(
      &builder->code_block, &(Instruction_Assembly){shr, {temp_full_register, imm8((u8)source->Register.offset_in_bits)}}
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
    u64 clear_mask = ~(((UINT64_C(1) << source_bit_size) - 1) << target->Register.offset_in_bits);
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
        &builder->code_block, &(Instruction_Assembly){mov, {temp_full_register, imm64(clear_mask)}}
      );
      push_eagerly_encoded_assembly_no_source_range(
        &builder->code_block, &(Instruction_Assembly){and, {target_full_register, temp_full_register}}
      );
    }

    // Prepare new bits from the source register
    {
      push_eagerly_encoded_assembly_no_source_range(
        &builder->code_block, &(Instruction_Assembly){xor, {temp_full_register, temp_full_register}}
      );
      Storage right_size_temp = temp_full_register;
      right_size_temp.bit_size = source->bit_size;
      move_value(builder, source_range, &right_size_temp, source);
      if (target->Register.offset_in_bits) {
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, &(Instruction_Assembly){shl, {temp_full_register, imm8((u8)target->Register.offset_in_bits)}}
        );
      }
    }

    // Merge new bits into the target register
    push_eagerly_encoded_assembly_no_source_range(
      &builder->code_block, &(Instruction_Assembly){or, {target_full_register, temp_full_register}}
    );
    register_release(builder, temp_full_register.Register.index);
    return;
  }

  if (source->tag == Storage_Tag_Immediate) {
    assert(source->bit_size.as_u64 <= 64);
    bool is_zero = memcmp(&source->Immediate.bits, &(u64){0}, source_bit_size / 8) == 0;
    if (is_zero && target->tag == Storage_Tag_Register) {
      // This messes up flags register so comparisons need to be aware of this optimization
      push_eagerly_encoded_assembly_no_source_range(
        &builder->code_block, &(Instruction_Assembly){xor, {*target, *target}} );
      return;
    }
    Storage adjusted_source = *source;
    adjusted_source.bit_size = target->bit_size;
    // Because of 15 byte instruction limit on x86 there is no way to move 64bit immediate
    // to a memory location. In which case we do a move through a temp register
    bool is_64bit_immediate = adjusted_source.bit_size.as_u64 == 64;
    if (is_64bit_immediate && target->tag != Storage_Tag_Register) {
      Storage temp = storage_register_temp(builder, adjusted_source.bit_size);
      push_eagerly_encoded_assembly_no_source_range(
        &builder->code_block, &(Instruction_Assembly){mov, {temp, adjusted_source}} );
      push_eagerly_encoded_assembly_no_source_range(
        &builder->code_block, &(Instruction_Assembly){mov, {*target, temp}} );
      register_release(builder, temp.Register.index);
    } else {
      push_eagerly_encoded_assembly_no_source_range(
        &builder->code_block, &(Instruction_Assembly){mov, {*target, adjusted_source}} );
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
    &builder->code_block, &(Instruction_Assembly){mov, {*target, *source}} );
}

static void
mass_zero_storage(
  Function_Builder *builder,
  const Storage *target,
  const Source_Range *source_range
) {
  static const Storage imm_zero_8  = {.tag = Storage_Tag_Immediate, .bit_size = {8},  .Immediate = { .bits = 0 } };
  static const Storage imm_zero_16 = {.tag = Storage_Tag_Immediate, .bit_size = {16}, .Immediate = { .bits = 0 } };
  static const Storage imm_zero_32 = {.tag = Storage_Tag_Immediate, .bit_size = {32}, .Immediate = { .bits = 0 } };
  static const Storage imm_zero_64 = {.tag = Storage_Tag_Immediate, .bit_size = {64}, .Immediate = { .bits = 0 } };

  u64 byte_size = target->bit_size.as_u64 / 8;
  u64 offset = 0;
  while (offset < byte_size) {
    u64 remainder = byte_size - offset;
    const Storage *zero;
    if (remainder >= 8) {
      zero = &imm_zero_64;
    } else if (remainder >= 4) {
      zero = &imm_zero_32;
    } else if (remainder >= 2) {
      zero = &imm_zero_16;
    } else {
      zero = &imm_zero_8;
    }
    Storage chunk = storage_with_offset_and_bit_size(target, u64_to_s32(offset), zero->bit_size);
    move_value(builder, source_range, &chunk, zero);
    offset += zero->bit_size.as_u64 / 8;
  }
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
    if (register_bitset_get(builder->register_used_bitset.bits, reg_index)) {
      if (!register_bitset_get(builder->register_volatile_bitset.bits, reg_index)) {
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
    if (register_bitset_get(builder->register_used_bitset.bits, reg_index)) {
      if (!register_bitset_get(builder->register_volatile_bitset.bits, reg_index)) {
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
  const Value *value
) {
  assert(value->tag == Value_Tag_Forced);
  const Storage *storage = &value->Forced.storage;
  if (storage->tag == Storage_Tag_Immediate) {
    u64 bit_size = storage->bit_size.as_u64;
    assert(bit_size <= 64);
    bool is_zero = memcmp(&storage->Immediate.bits, &(u64){0}, bit_size / 8) == 0;
    if (is_zero) {
      push_eagerly_encoded_assembly(
        &builder->code_block, *source_range,
        &(Instruction_Assembly){jmp, {code_label32(to_label)}}
      );
    } else {
      // nothing to do, just fall through to the next code
    }
    return;
  }
  if (storage->tag == Storage_Tag_Eflags) {
    const X64_Mnemonic *mnemonic = 0;
    switch(storage->Eflags.compare_type) {
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
    if (storage->tag == Storage_Tag_Register) {
      Storage test_storage = *storage;
      bool is_packed = storage->Register.offset_in_bits != 0;
      if (is_packed) {
        test_storage = storage_register(register_acquire_temp(builder), value->descriptor->bit_size);
        move_value(builder, source_range, &test_storage, storage);
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
          &(Instruction_Assembly){cmp, {*storage, imm32(0)}}
        );
      } else if (bit_size == 8) {
        push_eagerly_encoded_assembly(
          &builder->code_block, *source_range,
          &(Instruction_Assembly){cmp, {*storage, imm8(0)}}
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
    case Storage_Tag_Disjoint:
    case Storage_Tag_Immediate:
    case Storage_Tag_Static:
    case Storage_Tag_Eflags: {
      break;
    }
  }
  panic("Unexpected storage for an indirect return value");
  return 0;
}

static const Descriptor *
mass_infer_function_return_type(
  Mass_Context *context,
  const Function_Info *info,
  const Scope *scope,
  Value *body
) {
  Scope *body_scope = scope_make(context->allocator, scope);
  const Descriptor *return_descriptor = 0; // Inferred
  Parser body_parser = {
    .return_descriptor_pointer = &return_descriptor,
    .flags = Parser_Flags_None,
    .scope = body_scope,
    .epoch = get_new_epoch(),
    .module = 0, // FIXME provide module here
  };

  for (u64 i = 0; i < dyn_array_length(info->parameters); ++i) {
    const Resolved_Function_Parameter *def_param = dyn_array_get(info->parameters, i);
    assert(def_param->descriptor);
    assert(def_param->symbol);
    Value *arg_value = 0;
    const Descriptor *descriptor = def_param->descriptor;
    const Source_Range *source_range = &def_param->source_range;
    switch(def_param->tag) {
      case Resolved_Function_Parameter_Tag_Unknown: {
        // We don't need payload or the proc, because in this pass we do not expect
        // any of the values to be forced. In fact this will act as a free assertion.
        void *payload = 0;
        Lazy_Value_Proc proc = 0;
        arg_value = mass_make_lazy_value(context, &body_parser, *source_range, payload, descriptor, proc);
      } break;
      case Resolved_Function_Parameter_Tag_Known: {
        // Exact static parameters are defined as static, since they might influence the inference
        Storage storage = def_param->Known.storage;
        arg_value = value_make(context, descriptor, storage, *source_range);
      } break;
    }
    arg_value->flags |= Value_Flags_Constant;
    scope_define_value(body_scope, body_parser.epoch, *source_range, def_param->symbol, arg_value);
  }
  assert(value_is_ast_block(body));
  const Ast_Block *block = value_as_ast_block(body);
  Value *lazy_value = token_parse_block(context, &body_parser, block, &body->source_range);
  if (mass_has_error(context)) return 0;
  if (lazy_value->descriptor->tag == Descriptor_Tag_Never) {
    if (return_descriptor) {
      return return_descriptor;
    }
  }
  return lazy_value->descriptor;
}

static Value *
mass_function_literal_instance_for_info(
  Mass_Context *context,
  Function_Literal *literal,
  const Function_Info *fn_info
) {
  Program *program = context->program;
  const Calling_Convention *calling_convention = program->default_calling_convention;

  if (!dyn_array_is_initialized(literal->instances)) {
    literal->instances = dyn_array_make(
      Array_Value_Ptr,
      .allocator = context->allocator,
      .capacity = 4
    );
  }

  for (u64 i = 0; i < dyn_array_length(literal->instances); ++i) {
    Value *instance_value = *dyn_array_get(literal->instances, i);
    assert(instance_value->descriptor->tag == Descriptor_Tag_Function_Instance);
    const Descriptor_Function_Instance *instance = &instance_value->descriptor->Function_Instance;
    assert(instance->info);
    assert(instance->program);
    assert(instance->call_setup.calling_convention);
    if (instance->info != fn_info) continue;
    if (instance->call_setup.calling_convention != calling_convention) continue;
    if (instance->program != program) continue;
    return instance_value;
  }

  if (mass_has_error(context)) return 0;

  Function_Call_Setup call_setup = calling_convention->call_setup_proc(context->allocator, fn_info);
  const Descriptor *instance_descriptor =
    descriptor_function_instance(context->allocator, fn_info, call_setup, program);

  if (value_is_external_symbol(literal->body)) {
    const External_Symbol *symbol = value_as_external_symbol(literal->body);
    Storage storage = import_symbol(context->allocator, program, symbol->library_name, symbol->symbol_name);
    Value *cached_instance = value_init(
      allocator_allocate(context->allocator, Value),
      instance_descriptor, storage, literal->body->source_range
    );
    dyn_array_push(literal->instances, cached_instance);
    return cached_instance;
  }

  Label *call_label = make_label(context->allocator, program, &program->memory.code, slice_literal(":start"));
  // It is important to cache the label here for recursive calls
  Value *cached_instance = value_make(
    context, instance_descriptor, code_label32(call_label), literal->body->source_range
  );
  dyn_array_push(literal->instances, cached_instance);

  const Descriptor *return_descriptor = fn_info->return_descriptor;
  Scope *body_scope = scope_make(context->allocator, literal->own_scope);
  Parser body_parser = {
    .return_descriptor_pointer = &return_descriptor,
    .flags = Parser_Flags_None,
    .scope = body_scope,
    .epoch = get_new_epoch(),
    .module = 0, // FIXME provide module here
  };

  Slice end_label_name = slice_literal(":end");

  Storage return_storage = instance_descriptor->Function_Instance.call_setup.callee_return;
  Source_Range return_range = literal->header.returns.source_range;

  Function_Builder *builder = &(Function_Builder){
    .epoch = body_parser.epoch,
    .function = fn_info,
    .register_volatile_bitset = calling_convention->register_volatile_bitset,
    .return_value = {0},
    .code_block = {
      .allocator = context->allocator,
      .start_label = call_label,
      .end_label = make_label(context->allocator, program, &program->memory.code, end_label_name),
    },
  };
  value_init(&builder->return_value, return_descriptor, return_storage, return_range);

  {
    for (u64 i = 0; i < dyn_array_length(call_setup.parameters); ++i) {
      const Function_Call_Parameter *call_param = dyn_array_get(call_setup.parameters, i);
      Storage storage = call_param->storage;
      if (call_param->flags & Function_Call_Parameter_Flags_Uninitialized) {
        continue;
      }
      // :ParamStackAreaAdjust
      if (
        storage_is_stack(&storage) &&
        storage.Memory.location.Stack.area == Stack_Area_Call_Target_Argument
      ) {
        storage.Memory.location.Stack.area = Stack_Area_Received_Argument;
      }
      // :ParameterOriginalIndex
      // `Exact_Static` parameters are not present in call params because they have no runtime storage.
      // This is great for performance, but can cause mismatches between the indexes the parameters
      // defined in the `Function_Info` and ones in the call setup. To make sure parameters are
      // mapped correctly, `Function_Call_Parameter` keeps track of the original index.
      const Resolved_Function_Parameter *def_param = dyn_array_get(fn_info->parameters, call_param->original_index);
      assert(def_param->tag == Resolved_Function_Parameter_Tag_Unknown);
      Value *arg_value;
      if (call_param->flags & Function_Call_Parameter_Flags_Implicit_Pointer) {
        arg_value = 0;
        switch(call_param->storage.tag) {
          case Storage_Tag_Register: {
            Register reg = call_param->storage.Register.index;
            storage = storage_indirect(call_param->descriptor->bit_size, reg);
            arg_value = value_make(context, call_param->descriptor, storage, def_param->source_range);
          } break;
          case Storage_Tag_Memory: {
            if (storage_is_stack(&call_param->storage)) {
              arg_value = mass_allocate(context, Value);
              *arg_value = (Value) {
                .tag = Value_Tag_Lazy,
                .descriptor = call_param->descriptor,
                .source_range = def_param->source_range,
                .Lazy = {
                  .is_factory = true,
                  .scope = literal->own_scope,
                  .epoch = body_parser.epoch,
                  .proc = (Lazy_Value_Proc)mass_implicit_function_parameter_factory_proc,
                  .payload = call_param,
                },
              };
            } else {
              panic("UNEXPECTED implicit pointer arg storage");
            }
          } break;
          case Storage_Tag_Eflags:
          case Storage_Tag_Xmm:
          case Storage_Tag_Immediate:
          case Storage_Tag_Disjoint:
          case Storage_Tag_Static: {
            panic("UNEXPECTED implicit pointer arg storage");
          } break;
        }
      } else {
        arg_value = value_make(context, call_param->descriptor, storage, def_param->source_range);
      }
      arg_value->flags |= Value_Flags_Constant;
      const Symbol *param_symbol = def_param->symbol;
      if (param_symbol) {
        scope_define_value(body_scope, body_parser.epoch, def_param->source_range, param_symbol, arg_value);
      }
    }
    for (u64 i = 0; i < dyn_array_length(fn_info->parameters); ++i) {
      const Resolved_Function_Parameter *param = dyn_array_get(fn_info->parameters, i);
      if (param->tag != Resolved_Function_Parameter_Tag_Known) continue;

      Value *arg_value = value_make(context, param->descriptor, param->Known.storage, param->source_range);
      arg_value->flags |= Value_Flags_Constant;
      scope_define_value(body_scope, VALUE_STATIC_EPOCH, param->source_range, param->symbol, arg_value);
    }
  }
  register_acquire_bitset(builder, call_setup.parameter_registers_bitset.bits);

  Value *parse_result = 0;
  if (value_is_ast_block(literal->body)) {
    const Ast_Block *block = value_as_ast_block(literal->body);
    parse_result = token_parse_block(context, &body_parser, block, &literal->body->source_range);
  } else if (literal->body->descriptor == &descriptor_value_view) {
    const Value_View *view = value_as_value_view(literal->body);
    parse_result = token_parse_expression(context, &body_parser, *view, &(u32){0}, 0);
  } else if (literal->body->tag == Value_Tag_Lazy) {
    parse_result = literal->body;
  } else {
    panic("Unexpected function body type");
  }
  if (mass_has_error(context)) return 0;

  if (same_type(parse_result->descriptor, &descriptor_never)) {
    // TODO probably should verify that there are explicit returns
    //      in the function, otherwise the return type of the function
    //      itself should be `never`
    Expected_Result expected_result = expected_result_any(&descriptor_never);
    (void)value_force(context, builder, &expected_result, parse_result);
  } else {
    value_force_exact(context, builder, &builder->return_value, parse_result);
  }
  if (mass_has_error(context)) return 0;

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
      &builder->code_block, return_range,
      &(Instruction_Assembly){mov, {caller_register_storage, callee_register_storage}}
    );
  }

  calling_convention_x86_64_common_end_proc(program, builder);

  // Only push the builder at the end to avoid problems in nested JIT compiles
  dyn_array_push(program->functions, *builder);

  return cached_instance;
}

static Value *
ensure_function_instance(
  Mass_Context *context,
  Value *fn_value,
  Array_Resolved_Function_Parameter source_parameters
) {
  if (fn_value->descriptor->tag == Descriptor_Tag_Function_Instance) {
    return fn_value;
  }
  Function_Literal *literal = value_as_function_literal(fn_value);
  const Function_Info *fn_info =
    function_literal_info_for_parameters(context, literal, source_parameters);
  return mass_function_literal_instance_for_info(context, literal, fn_info);
}

static void
program_init_startup_code(
  Mass_Context *context
) {
  Program *program = context->program;
  Function_Info *fn_info = allocator_allocate(context->allocator, Function_Info);
  Source_Range source_range;
  INIT_LITERAL_SOURCE_RANGE(&source_range, "__startup");
  function_info_init(fn_info, &descriptor_void);
  const Calling_Convention *calling_convention =
    context->compilation->runtime_program->default_calling_convention;
  Function_Call_Setup call_setup = calling_convention->call_setup_proc(context->allocator, fn_info);
  Descriptor *descriptor = descriptor_function_instance(context->allocator, fn_info, call_setup, program);
  Label *fn_label = make_label(
    context->allocator, program, &program->memory.code, slice_literal("__startup:start")
  );
  Label *end_label =
    make_label(context->allocator, program, &program->memory.code, slice_literal("__startup:end"));
  Storage storage = code_label32(fn_label);

  Value *function = value_make(context, descriptor, storage, source_range);

  Function_Builder builder = (Function_Builder){
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

  Array_Resolved_Function_Parameter params = dyn_array_static_empty(Array_Resolved_Function_Parameter);
  Value *entry_instance = ensure_function_instance(context, program->entry_point, params);
  assert(entry_instance->tag == Value_Tag_Forced);
  push_eagerly_encoded_assembly(
    &builder.code_block, source_range,
    &(Instruction_Assembly){jmp, {entry_instance->Forced.storage}}
  );

  program->entry_point = function;
  dyn_array_push(program->functions, builder);
}

