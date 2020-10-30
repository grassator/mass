#include "function.h"

Value *
reserve_stack(
  Function_Builder *fn,
  Descriptor *descriptor
) {
  u32 byte_size = descriptor_byte_size(descriptor);
  fn->stack_reserve = s32_align(fn->stack_reserve, byte_size);
  fn->stack_reserve += byte_size;
  Operand operand = stack(-fn->stack_reserve, byte_size);
  Value *result = temp_allocate(Value);
  *result = (const Value) {
    .descriptor = descriptor,
    .operand = operand,
  };
  return result;
}

Register
temp_register_acquire(
  Function_Builder *builder
) {
  for (Register reg_index = 0; reg_index <= Register_R15; ++reg_index) {
    if (!register_bitset_get(builder->code_block.register_occupied_bitset, reg_index)) {
      register_bitset_set(&builder->used_register_bitset, reg_index);
      register_bitset_set(&builder->code_block.register_occupied_bitset, reg_index);
      return reg_index;
    }
  }

  // FIXME
  panic("Could not acquire a temp register");
  return -1;
}

void
temp_register_release(
  Function_Builder *builder,
  Register reg_index
) {
  assert(register_bitset_get(builder->code_block.register_occupied_bitset, reg_index));
  register_bitset_unset(&builder->code_block.register_occupied_bitset, reg_index);
}

void
move_value(
  Function_Builder *builder,
  const Source_Location *location,
  Value *target,
  Value *source
) {
  Array_Instruction *instructions = &builder->code_block.instructions;
  if (target == source) return;
  if (operand_equal(&target->operand, &source->operand)) return;

  if (target->descriptor->type == Descriptor_Type_Any) {
    target->descriptor = source->descriptor;
  }
  if (target->operand.type == Operand_Type_Any) {
    target->operand = source->operand;
    return;
  }

  u32 target_size = descriptor_byte_size(target->descriptor);
  u32 source_size = descriptor_byte_size(source->descriptor);

  assert(target_size >= source_size);

  if (
    target->descriptor->type == Descriptor_Type_Float ||
    source->descriptor->type == Descriptor_Type_Float
  ) {
    assert(target_size == source_size);
    assert(target->descriptor->type == source->descriptor->type);
    if (
      target->operand.type == Operand_Type_Xmm ||
      source->operand.type == Operand_Type_Xmm
    ) {
      if (target_size == 4) {
        push_instruction(instructions, location, (Instruction) {movss, {target->operand, source->operand, 0}});
      } else if (target_size == 8) {
        push_instruction(instructions, location, (Instruction) {movsd, {target->operand, source->operand, 0}});
      } else {
        panic("Internal Error: XMM operand of unexpected size");
      }
      return;
    } else {
      assert(operand_is_memory(&target->operand));
      assert(operand_is_memory(&source->operand));
      // Using xmm4 as it is volatile and not used in function arguments
      Value *reg_xmm4 = value_register_for_descriptor(Register_Xmm4, target->descriptor);
      move_value(builder, location, reg_xmm4, source);
      move_value(builder, location, target, reg_xmm4);
      return;
    }
  }

  if (source->operand.type == Operand_Type_Eflags) {
    assert(operand_is_register_or_memory(&target->operand));
    Value *temp = target;
    if (descriptor_byte_size(temp->descriptor) != 1) {
      temp = value_register_for_descriptor(temp_register_acquire(builder), &descriptor_s8);
    }
    switch(source->operand.compare_type) {
      case Compare_Type_Equal: {
        push_instruction(instructions, location, (Instruction) {sete, {temp->operand, source->operand}});
        break;
      }
      case Compare_Type_Not_Equal: {
        push_instruction(instructions, location, (Instruction) {setne, {temp->operand, source->operand}});
        break;
      }
      case Compare_Type_Less: {
        push_instruction(instructions, location, (Instruction) {setl, {temp->operand, source->operand}});
        break;
      }
      case Compare_Type_Less_Equal: {
        push_instruction(instructions, location, (Instruction) {setle, {temp->operand, source->operand}});
        break;
      }
      case Compare_Type_Greater: {
        push_instruction(instructions, location, (Instruction) {setg, {temp->operand, source->operand}});
        break;
      }
      case Compare_Type_Greater_Equal: {
        push_instruction(instructions, location, (Instruction) {setge, {temp->operand, source->operand}});
        break;
      }
      default: {
        assert(!"Unsupported comparison");
      }
    }
    if (temp != target) {
      assert(temp->operand.type == Operand_Type_Register);
      Operand resized_temp = operand_register_for_descriptor(temp->operand.reg, target->descriptor);
      push_instruction(instructions, location, (Instruction) {movsx, {resized_temp, temp->operand}});
      push_instruction(instructions, location, (Instruction) {mov, {target->operand, resized_temp}});
      temp_register_release(builder, temp->operand.reg);
    }
    return;
  }

  if (operand_is_immediate(&source->operand)) {
    s64 immediate = operand_immediate_as_s64(&source->operand);
    if (immediate == 0 && target->operand.type == Operand_Type_Register) {
      // This messes up flags register so comparisons need to be aware of this optimization
      push_instruction(instructions, location, (Instruction) {xor, {target->operand, target->operand}});
      return;
    }
    Value *adjusted_source = 0;
    switch(target_size) {
      case 1: {
        adjusted_source = value_from_s8(s64_to_s8(immediate));
        break;
      }
      case 2: {
        adjusted_source = value_from_s16(s64_to_s16(immediate));
        break;
      }
      case 4: {
        adjusted_source = value_from_s32(s64_to_s32(immediate));
        break;
      }
      case 8: {
        if (s64_fits_into_s32(immediate)) {
          adjusted_source = value_from_s32(s64_to_s32(immediate));
        } else {
          adjusted_source = value_from_s64(immediate);
        }
        break;
      }
    }
    // Because of 15 byte instruction limit on x86 there is no way to move 64bit immediate
    // to a memory location. In which case we do a move through a temp register
    bool is_64bit_immediate = descriptor_byte_size(adjusted_source->descriptor) == 8;
    if (is_64bit_immediate && target->operand.type != Operand_Type_Register) {
      Operand temp = operand_register_for_descriptor(temp_register_acquire(builder), target->descriptor);
      push_instruction(instructions, location, (Instruction) {mov, {temp, adjusted_source->operand}});
      push_instruction(instructions, location, (Instruction) {mov, {target->operand, temp}});
      temp_register_release(builder, temp.reg);
    } else {
      push_instruction(instructions, location, (Instruction) {mov, {target->operand, adjusted_source->operand}});
    }
    return;
  }

  // TODO figure out more type checking

  if (target_size != source_size) {
    if (source_size < target_size) {
      // TODO deal with unsigned numbers
      if (target->operand.type == Operand_Type_Register) {
        if (source_size == 4) {
          // TODO check whether this correctly sign extends
          Operand adjusted_target = {
            .type = Operand_Type_Register,
            .reg = target->operand.reg,
            .byte_size = 4,
          };
          push_instruction(instructions, location, (Instruction) {mov, {adjusted_target, source->operand}});
        } else {
          push_instruction(instructions, location, (Instruction) {movsx, {target->operand, source->operand}});
        }
      } else {
        Operand temp = operand_register_for_descriptor(temp_register_acquire(builder), target->descriptor);
        push_instruction(instructions, location, (Instruction) {movsx, {temp, source->operand}});
        push_instruction(instructions, location, (Instruction) {mov, {target->operand, temp}});
        temp_register_release(builder, temp.reg);
      }
      return;
    } else {
      print_operand(&target->operand);
      printf(" ");
      print_operand(&source->operand);
      printf("\n");
      slice_print(location->filename);
      printf("(%llu:%llu)", location->line, location->column);
      assert(!"Mismatched operand size when moving");
    }
  }

  if (operand_is_memory(&target->operand) && operand_is_memory(&source->operand)) {
    if (target_size >= 16) {
      // TODO probably can use larger chunks for copying but need to check alignment
      Value *temp_rsi = value_register_for_descriptor(temp_register_acquire(builder), &descriptor_s64);
      Value *temp_rdi = value_register_for_descriptor(temp_register_acquire(builder), &descriptor_s64);
      Value *temp_rcx = value_register_for_descriptor(temp_register_acquire(builder), &descriptor_s64);
      {
        Value *reg_rsi = value_register_for_descriptor(Register_SI, &descriptor_s64);
        Value *reg_rdi = value_register_for_descriptor(Register_DI, &descriptor_s64);
        Value *reg_rcx = value_register_for_descriptor(Register_C, &descriptor_s64);
        move_value(builder, location, temp_rsi, reg_rsi);
        move_value(builder, location, temp_rdi, reg_rdi);
        move_value(builder, location, temp_rcx, reg_rcx);

        push_instruction(instructions, location, (Instruction) {lea, {reg_rsi->operand, source->operand}});
        push_instruction(instructions, location, (Instruction) {lea, {reg_rdi->operand, target->operand}});
        move_value(builder, location, reg_rcx, value_from_s64(target_size));
        push_instruction(instructions, location, (Instruction) {rep_movsb});

        move_value(builder, location, reg_rsi, temp_rsi);
        move_value(builder, location, reg_rdi, temp_rdi);
        move_value(builder, location, reg_rcx, temp_rcx);
      }
      temp_register_release(builder, temp_rsi->operand.reg);
      temp_register_release(builder, temp_rdi->operand.reg);
      temp_register_release(builder, temp_rcx->operand.reg);
    } else {
      Value *temp = value_register_for_descriptor(temp_register_acquire(builder), target->descriptor);
      move_value(builder, location, temp, source);
      move_value(builder, location, target, temp);
      temp_register_release(builder, temp->operand.reg);
    }
    return;
  }

  push_instruction(instructions, location, (Instruction) {mov, {target->operand, source->operand}});
}

Function_Builder *
fn_begin(
  Value **result,
  Program *program
) {
  Descriptor *descriptor = temp_allocate(Descriptor);
  *descriptor = (const Descriptor) {
    .type = Descriptor_Type_Function,
    .function = {
      .arguments = dyn_array_make(Array_Value_Ptr, .allocator = temp_allocator),
      .returns = 0,
    },
  };
  Value *fn_value = temp_allocate(Value);
  *fn_value = (const Value) {
    .descriptor = descriptor,
    .operand = label32(make_label()),
  };
  Function_Builder *builder = dyn_array_push(program->functions, (Function_Builder){
    .program = program,
    .stack_reserve = 0,
    .descriptor = descriptor,
    .value = fn_value,
    .code_block = {
      .end_label = make_label(),
      .instructions = dyn_array_make(Array_Instruction, .allocator = temp_allocator),
    },
  });

  {
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

  *result = fn_value;

  return builder;
}

void
fn_end(
  Function_Builder *builder
) {
  u8 alignment = 0x8;
  builder->stack_reserve += builder->max_call_parameters_stack_size;
  builder->stack_reserve = s32_align(builder->stack_reserve, 16) + alignment;
}

u32
make_trampoline(
  const Program *program,
  Fixed_Buffer *buffer,
  s64 address
) {
  u32 result = u64_to_u32(buffer->occupied);
  encode_instruction_with_compiler_location(
    program, buffer, &(Instruction) {mov, {rax, imm64(address)}}
  );
  encode_instruction_with_compiler_location(program, buffer, &(Instruction) {jmp, {rax}});
  return result;
}


void
fn_maybe_remove_unnecessary_jump_from_return_statement_at_the_end_of_function(
  Function_Builder *builder
) {
  Instruction *last_instruction = dyn_array_last(builder->code_block.instructions);
  if (!last_instruction) return;
  if (last_instruction->maybe_label) return;
  if (last_instruction->mnemonic != jmp) return;
  Operand op = last_instruction->operands[0];
  if (op.type != Operand_Type_Label_32) return;
  if (op.label32 != builder->code_block.end_label) return;
  dyn_array_pop(builder->code_block.instructions);
}

s32
fn_adjust_stack_displacement(
  const Function_Builder *builder,
  s32 displacement
) {
  // Negative diplacement is used to encode local variables
  if (displacement < 0) {
    displacement += builder->stack_reserve;
  } else
  // Positive values larger than max_call_parameters_stack_size
  if (displacement >= u32_to_s32(builder->max_call_parameters_stack_size)) {
    // Return address will be pushed on the stack by the caller
    // and we need to account for that
    s32 return_address_size = 8;
    displacement += builder->stack_reserve + return_address_size;
  }
  return displacement;
}

void
fn_normalize_instruction_operands(
  const Function_Builder *builder,
  Instruction *instruction
) {
  // :OperandNormalization
  // Normalizing operands to simplify future handling in the encoder
  for (u8 operand_index = 0; operand_index < countof(instruction->operands); ++operand_index) {
    Operand *operand = &instruction->operands[operand_index];
    // RIP-relative imports are regular RIP-relative operands that we only know
    // target offset of at the point of encoding
    if (operand->type == Operand_Type_RIP_Relative_Import) {
      Import_Symbol *symbol = program_find_import(
        builder->program,
        operand->import.library_name,
        operand->import.symbol_name
      );
      *operand = (Operand){
        .type = Operand_Type_RIP_Relative,
        .byte_size = operand->byte_size,
        .rip_offset_in_data = symbol->offset_in_data,
      };
    }
    // [RSP + X] always needs to be encoded as SIB because RSP register index
    // in MOD R/M is occupied by RIP-relative encoding
    else if (operand->type == Operand_Type_Memory_Indirect && operand->indirect.reg == rsp.reg) {
      *operand = (Operand){
        .type = Operand_Type_Sib,
        .byte_size = operand->byte_size,
        .sib = {
          .scale = SIB_Scale_1,
          .base = Register_SP,
          .index = Register_SP,
          .displacement = operand->indirect.displacement,
        },
      };
    }
    bool is_stack_operand = operand->type == Operand_Type_Sib && operand->sib.base == Register_SP;
    if (is_stack_operand) {
      operand->sib.displacement = fn_adjust_stack_displacement(builder, operand->sib.displacement);
    }
  }
}

typedef struct {
  u8 register_index;
  u8 offset_in_prolog;
} Function_Pushed_Register;

void
fn_encode(
  Fixed_Buffer *buffer,
  Function_Builder *builder,
  RUNTIME_FUNCTION *function_exception_info,
  UNWIND_INFO *unwind_info,
  u32 unwind_data_rva
) {
  Program *program = builder->program;
  fn_maybe_remove_unnecessary_jump_from_return_statement_at_the_end_of_function(builder);
  Operand *operand = &builder->value->operand;
  assert(operand->type == Operand_Type_Label_32);

  s64 code_base_rva = builder->program->code_base_rva;
  u32 fn_start_rva = u64_to_u32(code_base_rva + buffer->occupied);
  Operand stack_size_operand = imm_auto_8_or_32(builder->stack_reserve);
  encode_instruction_with_compiler_location(
    program, buffer, &(Instruction) {.maybe_label = operand->label32}
  );

  Array_UNWIND_CODE unwind_codes = dyn_array_make(Array_UNWIND_CODE);

  #define fn_offset_in_prolog()\
    u64_to_u8(code_base_rva + buffer->occupied - fn_start_rva)

  // Push non-volatile registers (in reverse order)
  for (Register reg_index = Register_R15; reg_index >= Register_A; --reg_index) {
    if (register_bitset_get(builder->used_register_bitset, reg_index)) {
      if (!register_bitset_get(builder->code_block.register_volatile_bitset, reg_index)) {
        Operand to_save = operand_register_for_descriptor(reg_index, &descriptor_s64);
        encode_instruction_with_compiler_location(program, buffer, &(Instruction) {push, {to_save}});
        dyn_array_push(unwind_codes, (UNWIND_CODE) {
          .CodeOffset = fn_offset_in_prolog(),
          .UnwindOp = UWOP_PUSH_NONVOL,
          .OpInfo = reg_index,
        });
      }
    }
  }

  encode_instruction_with_compiler_location(
    program, buffer, &(Instruction) {sub, {rsp, stack_size_operand}}
  );
  u32 stack_allocation_offset_in_prolog = fn_offset_in_prolog();
  u32 size_of_prolog = u64_to_u32(code_base_rva + buffer->occupied) - fn_start_rva;

  #undef fn_offset_in_prolog

  for (u64 i = 0; i < dyn_array_length(builder->code_block.instructions); ++i) {
    Instruction *instruction = dyn_array_get(builder->code_block.instructions, i);
    fn_normalize_instruction_operands(builder, instruction);
    encode_instruction(program, buffer, instruction);
  }

  encode_instruction_with_compiler_location(
    program, buffer, &(Instruction) {.maybe_label = builder->code_block.end_label}
  );

  // :ReturnTypeLargerThanRegister
  if(descriptor_byte_size(builder->value->descriptor->function.returns->descriptor) > 8) {
    // FIXME :RegisterAllocation
    //       make sure that return value is always available in RCX at this point
    encode_instruction_with_compiler_location(
      program, buffer, &(Instruction) {mov, {rax, rcx}}
    );
  }

  encode_instruction_with_compiler_location(
    program, buffer, &(Instruction) {add, {rsp, stack_size_operand}}
  );

  // Pop non-volatile registers
  for (s64 i = dyn_array_length(unwind_codes) - 1; i >= 0; --i) {
    Operand to_save = {
      .type = Operand_Type_Register,
      .byte_size = 8,
      .reg = dyn_array_get(unwind_codes, i)->OpInfo,
    };
    encode_instruction_with_compiler_location(program, buffer, &(Instruction) {pop, {to_save}});
  }

  encode_instruction_with_compiler_location(program, buffer, &(Instruction) {ret, {0}});
  u32 fn_end_rva = u64_to_u32(code_base_rva + buffer->occupied);

  encode_instruction_with_compiler_location(program, buffer, &(Instruction) {int3, {0}});

  if (function_exception_info || unwind_info) {
    // Make sure either both or none are provided
    assert(unwind_info);
    assert(function_exception_info);
    *unwind_info = (UNWIND_INFO) {
      .Version = 1,
      .Flags = 0,
      .SizeOfProlog = u32_to_u8(size_of_prolog),
      .CountOfCodes = 0,
      .FrameRegister = 0,
      .FrameOffset = 0,
    };

    u8 unwind_code_index = 0;
    for (u64 i = 0; i < dyn_array_length(unwind_codes); ++i) {
      unwind_info->UnwindCode[unwind_code_index++] = *dyn_array_get(unwind_codes, i);
    }

    if (builder->stack_reserve) {
      assert(builder->stack_reserve >= 8);
      assert(builder->stack_reserve % 8 == 0);
      if (builder->stack_reserve <= 128) {
        unwind_info->UnwindCode[unwind_code_index++] = (UNWIND_CODE){
          .CodeOffset = u32_to_u8(stack_allocation_offset_in_prolog),
          .UnwindOp = UWOP_ALLOC_SMALL,
          .OpInfo = (builder->stack_reserve - 8) / 8,
        };
      } else {
        unwind_info->UnwindCode[unwind_code_index++] = (UNWIND_CODE){
          .CodeOffset = u32_to_u8(stack_allocation_offset_in_prolog),
          .UnwindOp = UWOP_ALLOC_LARGE,
          .OpInfo = 0,
        };
        unwind_info->UnwindCode[unwind_code_index++] = (UNWIND_CODE){
          .DataForPreviousCode = u32_to_u16(builder->stack_reserve / 8),
        };
        // TODO support 512k + allocations
      }
      unwind_info->CountOfCodes = unwind_code_index;
    }
    *function_exception_info = (RUNTIME_FUNCTION) {
      .BeginAddress = fn_start_rva,
      .EndAddress = fn_end_rva,
      .UnwindData = unwind_data_rva,
    };
  }
  dyn_array_destroy(unwind_codes);
}

void
function_return_descriptor(
  Descriptor_Function *function,
  Descriptor *descriptor
) {
  if (!function->returns) {
    if (descriptor->type != Descriptor_Type_Void) {
      // TODO handle 16 bit non-float return values are returned in XMM0
      if (descriptor->type == Descriptor_Type_Float) {
        function->returns = value_register_for_descriptor(Register_Xmm0, descriptor);
      } else {
        // :ReturnTypeLargerThanRegister
        u32 byte_size = descriptor_byte_size(descriptor);
        if (byte_size > 8) {
          function->returns = temp_allocate(Value);
          *function->returns = (Value){
            .descriptor = descriptor,
            .operand = {
              .type = Operand_Type_Memory_Indirect,
              .byte_size = byte_size,
              .indirect = {
                .reg = Register_C,
                .displacement = 0,
              },
            },
          };
        } else {
          function->returns = value_register_for_descriptor(Register_A, descriptor);
        }
      }
    } else {
      function->returns = &void_value;
    }
  }
}

Label *
make_if(
  Array_Instruction *instructions,
  const Source_Location *location,
  Value *value
) {
  bool is_always_true = false;
  if(operand_is_immediate(&value->operand)) {
    s64 imm = operand_immediate_as_s64(&value->operand);
    if (imm == 0) return 0;
    is_always_true = true;
  }

  Label *label = make_label();
  if (!is_always_true) {
    if (value->operand.type == Operand_Type_Eflags) {
      switch(value->operand.compare_type) {
        case Compare_Type_Equal: {
          push_instruction(instructions, location, (Instruction) {jne, {label32(label), value->operand, 0}});
          break;
        }
        case Compare_Type_Not_Equal: {
          push_instruction(instructions, location, (Instruction) {je, {label32(label), value->operand, 0}});
          break;
        }
        case Compare_Type_Less: {
          push_instruction(instructions, location, (Instruction) {jge, {label32(label), value->operand, 0}});
          break;
        }
        case Compare_Type_Less_Equal: {
          push_instruction(instructions, location, (Instruction) {jg, {label32(label), value->operand, 0}});
          break;
        }
        case Compare_Type_Greater: {
          push_instruction(instructions, location, (Instruction) {jle, {label32(label), value->operand, 0}});
          break;
        }
        case Compare_Type_Greater_Equal: {
          push_instruction(instructions, location, (Instruction) {jl, {label32(label), value->operand, 0}});
          break;
        }
        default: {
          assert(!"Unsupported comparison");
        }
      }
    } else {
      u32 byte_size = descriptor_byte_size(value->descriptor);
      if (byte_size == 4 || byte_size == 8) {
        push_instruction(instructions, location, (Instruction) {cmp, {value->operand, imm32(0), 0}});
      } else if (byte_size == 1) {
        push_instruction(instructions, location, (Instruction) {cmp, {value->operand, imm8(0), 0}});
      } else {
        assert(!"Unsupported value inside `if`");
      }
      Value *eflags = value_from_compare(Compare_Type_Equal);
      push_instruction(instructions, location, (Instruction) {je, {label32(label), eflags->operand, 0}});
    }
  }
  return label;
}

Loop_Builder
loop_start(
  Array_Instruction *instructions,
  const Source_Location *location
) {
  Label *label_start = make_label();
  push_instruction(instructions, location, (Instruction) { .maybe_label = label_start });
  return (Loop_Builder) {
    .done = false,
    .label_start = label_start,
    .label_end = make_label(),
  };
}

void
assert_not_register_ax(
  Value *overload
) {
  assert(overload);
  if (overload->operand.type == Operand_Type_Register) {
    assert(overload->operand.reg != Register_A);
  }
}

typedef enum {
  Arithmetic_Operation_Plus,
  Arithmetic_Operation_Minus,
} Arithmetic_Operation;


#define maybe_constant_fold(_builder_, _loc_, _result_, _a_, _b_, _operator_)\
  do {\
    Operand *a_operand = &(_a_)->operand;\
    Operand *b_operand = &(_b_)->operand;\
    if (operand_is_immediate(a_operand) && operand_is_immediate(b_operand)) {\
      s64 a_s64 = operand_immediate_as_s64(a_operand);\
      s64 b_s64 = operand_immediate_as_s64(b_operand);\
      move_value(\
        (_builder_), (_loc_), (_result_), value_from_signed_immediate(a_s64 _operator_ b_s64)\
      );\
    }\
  } while(0)

void
plus_or_minus(
  Arithmetic_Operation operation,
  Function_Builder *builder,
  const Source_Location *location,
  Value *result_value,
  Value *a,
  Value *b
) {
  if (!(
    a->descriptor->type == Descriptor_Type_Pointer &&
    b->descriptor->type == Descriptor_Type_Integer &&
    b->descriptor->integer.byte_size == 8
  )) {
    assert(same_value_type_or_can_implicitly_move_cast(a, b));
    assert(a->descriptor->type == Descriptor_Type_Integer);
  }

  if (operand_is_immediate(&a->operand) && operand_is_immediate(&b->operand)) {
    s64 a_s64 = operand_immediate_as_s64(&a->operand);
    s64 b_s64 = operand_immediate_as_s64(&b->operand);
    s64 folded = 0;
    switch(operation) {
      case Arithmetic_Operation_Plus: folded = a_s64 + b_s64; break;
      case Arithmetic_Operation_Minus: folded = a_s64 - b_s64; break;
    }
    move_value(builder, location, result_value, value_from_signed_immediate(folded));
    return;
  }

  const X64_Mnemonic *mnemonic = 0;
  switch(operation) {
    case Arithmetic_Operation_Plus: {
      mnemonic = add;
      // Addition is commutative (a + b == b + a)
      // so we can swap operands and save one instruction
      if (operand_equal(&result_value->operand, &b->operand)) {
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
    result_value->operand.type == Operand_Type_Register &&
    !operand_equal(&result_value->operand, &b->operand)
  );
  Value *temp = can_reuse_result_as_temp
    ? result_value
    : value_register_for_descriptor(temp_register_acquire(builder), a->descriptor);

  move_value(builder, location, temp, a);
  push_instruction(
    &builder->code_block.instructions,
    location,
    (Instruction) {mnemonic, {temp->operand, b->operand}}
  );
  if (temp != result_value) {
    move_value(builder, location, result_value, temp);
    assert(temp->operand.type == Operand_Type_Register);
    temp_register_release(builder, temp->operand.reg);
  }
}

void
plus(
  Function_Builder *builder,
  const Source_Location *location,
  Value *result_value,
  Value *a,
  Value *b
) {
  plus_or_minus(Arithmetic_Operation_Plus, builder, location, result_value, a, b);
}

void
minus(
  Function_Builder *builder,
  const Source_Location *location,
  Value *result_value,
  Value *a,
  Value *b
) {
  plus_or_minus(Arithmetic_Operation_Minus, builder, location, result_value, a, b);
}

void
multiply(
  Function_Builder *builder,
  const Source_Location *location,
  Value *result_value,
  Value *x,
  Value *y
) {
  Array_Instruction *instructions = &builder->code_block.instructions;
  assert(same_value_type(x, y));
  assert(x->descriptor->type == Descriptor_Type_Integer);

  assert_not_register_ax(x);
  assert_not_register_ax(y);

  maybe_constant_fold(builder, location, result_value, x, y, *);

  // TODO deal with signed / unsigned
  // TODO support double the size of the result?
  // TODO make the move only for imm value
  Value *y_temp = reserve_stack(builder, y->descriptor);

  Value *reg_a = value_register_for_descriptor(Register_A, y->descriptor);
  move_value(builder, location, reg_a, y);
  move_value(builder, location, y_temp, reg_a);

  reg_a = value_register_for_descriptor(Register_A, x->descriptor);
  move_value(builder, location, reg_a, x);

  push_instruction(instructions, location, (Instruction) {imul, {reg_a->operand, y_temp->operand}});

  move_value(builder, location, result_value, reg_a);
}

typedef enum {
  Divide_Operation_Divide,
  Divide_Operation_Remainder,
} Divide_Operation;

void
divide_or_remainder(
  Divide_Operation operation,
  Function_Builder *builder,
  const Source_Location *location,
  Value *result_value,
  Value *a,
  Value *b
) {
  Array_Instruction *instructions = &builder->code_block.instructions;
  assert(same_value_type_or_can_implicitly_move_cast(a, b));
  assert(a->descriptor->type == Descriptor_Type_Integer);

  // TODO type check values
  assert_not_register_ax(a);
  assert_not_register_ax(b);

  if (operand_is_immediate(&a->operand) && operand_is_immediate(&a->operand)) {
    s64 divident = operand_immediate_as_s64(&a->operand);
    s64 divisor = operand_immediate_as_s64(&b->operand);
    assert(divisor != 0);
    s64 folded = 0;
    switch(operation) {
      case Divide_Operation_Divide: {
        folded = divident / divisor;
        break;
      }
      case Divide_Operation_Remainder: {
        folded = divident % divisor;
        break;
      }
    }
    move_value(builder, location, result_value, value_from_signed_immediate(folded));
    return;
  }

  // Save RDX as it will be used for the remainder
  Value *rdx_temp = reserve_stack(builder, &descriptor_s64);

  Value *reg_rdx = value_register_for_descriptor(Register_A, &descriptor_s64);
  move_value(builder, location, rdx_temp, reg_rdx);

  Descriptor *larger_descriptor =
    descriptor_byte_size(a->descriptor) > descriptor_byte_size(b->descriptor)
    ? a->descriptor
    : b->descriptor;

  // TODO deal with signed / unsigned
  Value *divisor = reserve_stack(builder, larger_descriptor);
  move_value(builder, location, divisor, b);

  Value *reg_a = value_register_for_descriptor(Register_A, larger_descriptor);
  {
    move_value(builder, location, reg_a, a);

    switch (descriptor_byte_size(larger_descriptor)) {
      case 8: {
        push_instruction(instructions, location, (Instruction) {cqo, {0}});
        break;
      }
      case 4: {
        push_instruction(instructions, location, (Instruction) {cdq, {0}});
        break;
      }
      case 2: {
        push_instruction(instructions, location, (Instruction) {cwd, {0}});
        break;
      }
      case 1: {
        push_instruction(instructions, location, (Instruction) {cwb, {0}});
        break;
      }
      default: {
        assert(!"Unsupported byte size when dividing");
      }
    }
  }
  push_instruction(instructions, location, (Instruction) {idiv, {divisor->operand, 0, 0}});


  if (operation == Divide_Operation_Divide) {
    move_value(builder, location, result_value, reg_a);
  } else {
    if (descriptor_byte_size(larger_descriptor) == 1) {
      Value *temp_result = value_register_for_descriptor(Register_AH, larger_descriptor);
      move_value(builder, location, result_value, temp_result);
    } else {
      Value *temp_result = value_register_for_descriptor(Register_D, larger_descriptor);
      move_value(builder, location, result_value, temp_result);
    }
  }

  // Restore RDX
  move_value(builder, location, reg_rdx, rdx_temp);
}

void
divide(
  Function_Builder *builder,
  const Source_Location *location,
  Value *result_value,
  Value *a,
  Value *b
) {
  divide_or_remainder(Divide_Operation_Divide, builder, location, result_value, a, b);
}

void
value_remainder(
  Function_Builder *builder,
  const Source_Location *location,
  Value *result_value,
  Value *a,
  Value *b
) {
  divide_or_remainder(Divide_Operation_Remainder, builder, location, result_value, a, b);
}


void
compare(
  Compare_Type operation,
  Function_Builder *builder,
  const Source_Location *location,
  Value *result_value,
  Value *a,
  Value *b
) {
  Array_Instruction *instructions = &builder->code_block.instructions;
  assert(a->descriptor->type == Descriptor_Type_Integer);
  assert(b->descriptor->type == Descriptor_Type_Integer);

  switch(operation) {
    case Compare_Type_Equal: {
      maybe_constant_fold(builder, location, result_value, a, b, ==);
      break;
    }
    case Compare_Type_Not_Equal: {
      maybe_constant_fold(builder, location, result_value, a, b, !=);
      break;
    }
    case Compare_Type_Less: {
      maybe_constant_fold(builder, location, result_value, a, b, <);
      break;
    }
    case Compare_Type_Greater: {
      maybe_constant_fold(builder, location, result_value, a, b, >);
      break;
    }
    case Compare_Type_Less_Equal: {
      maybe_constant_fold(builder, location, result_value, a, b, <=);
      break;
    }
    case Compare_Type_Greater_Equal: {
      maybe_constant_fold(builder, location, result_value, a, b, >=);
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

  Value *temp_b = reserve_stack(builder, larger_descriptor);
  move_value(builder, location, temp_b, b);

  Value *reg_r11 = value_register_for_descriptor(Register_R11, larger_descriptor);
  move_value(builder, location,  reg_r11, a);

  push_instruction(instructions, location, (Instruction) {cmp, {reg_r11->operand, temp_b->operand, 0}});

  move_value(builder, location, result_value, value_from_compare(operation));
}

Value *
value_pointer_to(
  Function_Builder *builder,
  const Source_Location *location,
  Value *value
) {
  Array_Instruction *instructions = &builder->code_block.instructions;
  // TODO support register
  // TODO support immediates
  assert(
    value->operand.type == Operand_Type_Memory_Indirect ||
    value->operand.type == Operand_Type_RIP_Relative
  );
  Descriptor *result_descriptor = descriptor_pointer_to(value->descriptor);

  Value *reg_a = value_register_for_descriptor(Register_A, result_descriptor);
  Operand source_operand = value->operand;

  // TODO rethink operand sizing
  // We need to manually adjust the size here because even if we loading one byte
  // the right side is treated as an opaque address and does not participate in
  // instruction encoding.
  source_operand.byte_size = descriptor_byte_size(result_descriptor);

  push_instruction(instructions, location, (Instruction) {lea, {reg_a->operand, source_operand, 0}});

  Value *result = reserve_stack(builder, result_descriptor);
  move_value(builder, location, result, reg_a);

  return result;
}

typedef struct {
  Value saved;
  Value *stack_value;
} Saved_Register;
typedef dyn_array_type(Saved_Register) Array_Saved_Register;

Value *
call_function_overload(
  Function_Builder *builder,
  const Source_Location *location,
  Value *to_call,
  Array_Value_Ptr arguments
) {
  Array_Instruction *instructions = &builder->code_block.instructions;
  assert(to_call->descriptor->type == Descriptor_Type_Function);
  Descriptor_Function *descriptor = &to_call->descriptor->function;
  assert(dyn_array_length(descriptor->arguments) == dyn_array_length(arguments));

  Array_Saved_Register saved_array = dyn_array_make(Array_Saved_Register);

  for (Register reg_index = 0; reg_index <= Register_R15; ++reg_index) {
    if (register_bitset_get(builder->code_block.register_volatile_bitset, reg_index)) {
      if (register_bitset_get(builder->code_block.register_occupied_bitset, reg_index)) {
        Value to_save = {
          .descriptor = &descriptor_s64,
          .operand = {
            .type = Operand_Type_Register,
            .byte_size = 8,
            .reg = reg_index,
          }
        };
        Operand source = operand_register_for_descriptor(reg_index, &descriptor_s64);
        Value *stack_value = reserve_stack(builder, to_save.descriptor);
        push_instruction(instructions, location, (Instruction) {mov, {stack_value->operand, source}});
        dyn_array_push(saved_array, (Saved_Register){.saved = to_save, .stack_value = stack_value});
      }
    }
  }

  for (u64 i = 0; i < dyn_array_length(arguments); ++i) {
    Value *source_arg = *dyn_array_get(arguments, i);
    Value *target_arg = *dyn_array_get(descriptor->arguments, i);
    move_value(builder, location, target_arg, source_arg);
  }

  // If we call a function, then we need to reserve space for the home
  // area of at least 4 arguments?
  u64 parameters_stack_size = u64_max(4, dyn_array_length(arguments)) * 8;

  // :ReturnTypeLargerThanRegister
  u32 return_size = descriptor_byte_size(descriptor->returns->descriptor);
  if (return_size > 8) {
    parameters_stack_size += return_size;
    Descriptor *return_pointer_descriptor = descriptor_pointer_to(descriptor->returns->descriptor);
    Value *reg_c = value_register_for_descriptor(Register_C, return_pointer_descriptor);
    push_instruction(
      instructions, location, (Instruction) {lea, {reg_c->operand, descriptor->returns->operand}}
    );
  }

  builder->max_call_parameters_stack_size = u64_to_u32(u64_max(
    builder->max_call_parameters_stack_size,
    parameters_stack_size
  ));

  if (to_call->operand.type == Operand_Type_Label_32) {
    push_instruction(instructions, location, (Instruction) {call, {to_call->operand, 0, 0}});
  } else {
    Value *reg_a = value_register_for_descriptor(Register_A, to_call->descriptor);
    move_value(builder, location, reg_a, to_call);
    push_instruction(instructions, location, (Instruction) {call, {reg_a->operand, 0, 0}});
  }

  Value *result = descriptor->returns;
  if (return_size <= 8) {
    if (return_size != 0) {
      result = reserve_stack(builder, descriptor->returns->descriptor);
      move_value(builder, location, result, descriptor->returns);
    }
  }

  for (u64 i = 0; i < dyn_array_length(saved_array); ++i) {
    Saved_Register *reg = dyn_array_get(saved_array, i);
    move_value(builder, location, &reg->saved, reg->stack_value);
    // TODO :FreeStackAllocation
  }

  return result;
}

typedef struct {
  Value *value;
  s64 score;
} Overload_Match;

s64
calculate_arguments_match_score(
  Descriptor_Function *descriptor,
  Array_Value_Ptr arguments
) {
  enum {
    Score_Exact = 100000,
    Score_Cast = 1,
  };
  s64 score = 0;
  for (u64 arg_index = 0; arg_index < dyn_array_length(arguments); ++arg_index) {
    Value *source_arg = *dyn_array_get(arguments, arg_index);
    Value *target_arg = *dyn_array_get(descriptor->arguments, arg_index);
    if (same_value_type(target_arg, source_arg)) {
      score += Score_Exact;
    } else if(same_value_type_or_can_implicitly_move_cast(target_arg, source_arg)) {
      score += Score_Cast;
    } else {
      return -1;
    }
  }
  return score;
}

Value *
call_function_value_array(
  Function_Builder *builder,
  const Source_Location *location,
  Value *to_call,
  Array_Value_Ptr arguments
) {
  Overload_Match match = {.score = -1};
  for (;to_call; to_call = to_call->descriptor->function.next_overload) {
    Descriptor_Function *descriptor = &to_call->descriptor->function;
    if (dyn_array_length(arguments) != dyn_array_length(descriptor->arguments)) continue;
    s64 score = calculate_arguments_match_score(descriptor, arguments);
    if (score > match.score) {
      // FIXME think about same scores
      match.value = to_call;
      match.score = score;
    }
  }
  if (match.value) {
    return call_function_overload(builder, location, match.value, arguments);
  }
  assert(!"No matching overload found");
  return 0;
}

Value *
make_and(
  Function_Builder *builder,
  const Source_Location *location,
  Value *a,
  Value *b
) {
  Array_Instruction *instructions = &builder->code_block.instructions;
  Value *result = reserve_stack(builder, &descriptor_s8);
  Label *label = make_label();

  Label *else_label = make_if(instructions, location, a);
  {
    compare(Compare_Type_Not_Equal, builder, location, result, b, value_from_s8(0));
    push_instruction(instructions, location, (Instruction) {jmp, {label32(label), 0, 0}});
  }
  push_instruction(instructions, location, (Instruction) {.maybe_label = else_label});

  move_value(builder, location, result, value_from_s8(0));
  push_instruction(instructions, location, (Instruction) {.maybe_label = label});
  return result;
}

Value *
make_or(
  Function_Builder *builder,
  const Source_Location *location,
  Value *a,
  Value *b
) {
  Array_Instruction *instructions = &builder->code_block.instructions;
  Value *result = reserve_stack(builder, &descriptor_s8);
  Label *label = make_label();

  compare(Compare_Type_Equal, builder, location, result, a, value_from_s8(0));
  Label *else_label = make_if(instructions, location, result);
  {
    compare(Compare_Type_Not_Equal, builder, location, result, b, value_from_s8(0));
    push_instruction(instructions, location, (Instruction) {jmp, {label32(label)}});
  }
  push_instruction(instructions, location, (Instruction) {.maybe_label = else_label});

  move_value(builder, location, result, value_from_s8(1));
  push_instruction(instructions, location, (Instruction) {.maybe_label = label});
  return result;
}

Value *
ensure_memory(
  Value *value
) {
  Operand operand = value->operand;
  if (operand.type == Operand_Type_Memory_Indirect) return value;
  Value *result = temp_allocate(Value);
  if (value->descriptor->type != Descriptor_Type_Pointer) assert(!"Not implemented");
  if (value->operand.type != Operand_Type_Register) assert(!"Not implemented");
  *result = (const Value) {
    .descriptor = value->descriptor->pointer_to,
    .operand = {
      .type = Operand_Type_Memory_Indirect,
      .indirect = {
        .reg = value->operand.reg,
        .displacement = 0,
      },
    },
  };
  return result;
}

Value *
struct_get_field(
  Value *raw_value,
  Slice name
) {
  Value *struct_value = ensure_memory(raw_value);
  Descriptor *descriptor = struct_value->descriptor;
  assert(descriptor->type == Descriptor_Type_Struct);
  for (u64 i = 0; i < dyn_array_length(descriptor->struct_.fields); ++i) {
    Descriptor_Struct_Field *field = dyn_array_get(descriptor->struct_.fields, i);
    if (slice_equal(name, field->name)) {
      Value *result = temp_allocate(Value);
      Operand operand = struct_value->operand;
      // FIXME support more operands
      assert(operand.type == Operand_Type_Memory_Indirect);
      operand.byte_size = descriptor_byte_size(field->descriptor);
      operand.indirect.displacement += field->offset;
      *result = (const Value) {
        .descriptor = field->descriptor,
        .operand = operand,
      };
      return result;
    }
  }

  assert(!"Could not find a field with specified name");
  return 0;
}








