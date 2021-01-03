#include "function.h"

Value *
reserve_stack(
  Allocator *allocator,
  Function_Builder *fn,
  Descriptor *descriptor
) {
  u32 byte_size = descriptor_byte_size(descriptor);
  fn->layout.stack_reserve = s32_align(fn->layout.stack_reserve, byte_size);
  fn->layout.stack_reserve += byte_size;
  Operand operand = stack(-fn->layout.stack_reserve, byte_size);
  Value *result = allocator_allocate(allocator, Value);
  *result = (Value) {
    .descriptor = descriptor,
    .operand = operand,
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
  for (Register reg_index = 0; reg_index <= Register_R15; ++reg_index) {
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

void
move_value(
  Allocator *allocator,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *target,
  Value *source
) {
  Array_Instruction *instructions = &builder->code_block.instructions;
  if (target == source) return;
  if (operand_equal(&target->operand, &source->operand)) return;

  if (target->descriptor->tag == Descriptor_Tag_Any) {
    target->descriptor = source->descriptor;
  }
  if (target->operand.tag == Operand_Tag_Any) {
    target->operand = source->operand;
    return;
  }

  u32 target_size = descriptor_byte_size(target->descriptor);
  u32 source_size = descriptor_byte_size(source->descriptor);

  if (descriptor_is_float(target->descriptor) || descriptor_is_float(source->descriptor)) {
    assert(target_size == source_size);
    assert(target->descriptor->tag == source->descriptor->tag);
    if (
      target->operand.tag == Operand_Tag_Xmm ||
      source->operand.tag == Operand_Tag_Xmm
    ) {
      if (target_size == 4) {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {movss, {target->operand, source->operand, 0}}});
      } else if (target_size == 8) {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {movsd, {target->operand, source->operand, 0}}});
      } else {
        panic("Internal Error: XMM operand of unexpected size");
      }
      return;
    } else {
      assert(operand_is_memory(&target->operand));
      assert(operand_is_memory(&source->operand));
      // Using xmm4 as it is volatile and not used in function arguments
      Value *reg_xmm4 = value_register_for_descriptor(allocator, Register_Xmm4, target->descriptor);
      move_value(allocator, builder, source_range, reg_xmm4, source);
      move_value(allocator, builder, source_range, target, reg_xmm4);
      return;
    }
  }

  if (source->operand.tag == Operand_Tag_Eflags) {
    assert(operand_is_register_or_memory(&target->operand));
    Value *temp = target;
    if (descriptor_byte_size(temp->descriptor) != 1) {
      temp = value_register_for_descriptor(allocator, register_acquire_temp(builder), &descriptor_s8);
    }
    switch(source->operand.Eflags.compare_type) {
      case Compare_Type_Equal: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {sete, {temp->operand, source->operand}}});
        break;
      }
      case Compare_Type_Not_Equal: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {setne, {temp->operand, source->operand}}});
        break;
      }

      case Compare_Type_Unsigned_Below: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {setb, {temp->operand, source->operand}}});
        break;
      }
      case Compare_Type_Unsigned_Below_Equal: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {setbe, {temp->operand, source->operand}}});
        break;
      }
      case Compare_Type_Unsigned_Above: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {seta, {temp->operand, source->operand}}});
        break;
      }
      case Compare_Type_Unsigned_Above_Equal: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {setae, {temp->operand, source->operand}}});
        break;
      }

      case Compare_Type_Signed_Less: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {setl, {temp->operand, source->operand}}});
        break;
      }
      case Compare_Type_Signed_Less_Equal: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {setle, {temp->operand, source->operand}}});
        break;
      }
      case Compare_Type_Signed_Greater: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {setg, {temp->operand, source->operand}}});
        break;
      }
      case Compare_Type_Signed_Greater_Equal: {
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {setge, {temp->operand, source->operand}}});
        break;
      }
      default: {
        assert(!"Unsupported comparison");
      }
    }
    if (temp != target) {
      assert(temp->operand.tag == Operand_Tag_Register);
      Operand resized_temp =
        operand_register_for_descriptor(temp->operand.Register.index, target->descriptor);
      push_instruction(instructions, *source_range, (Instruction) {.assembly = {movsx, {resized_temp, temp->operand}}});
      push_instruction(instructions, *source_range, (Instruction) {.assembly = {mov, {target->operand, resized_temp}}});
      register_release(builder, temp->operand.Register.index);
    }
    return;
  }

  if (operand_is_immediate(&source->operand)) {
    s64 immediate = operand_immediate_value_up_to_s64(&source->operand);
    if (immediate == 0 && target->operand.tag == Operand_Tag_Register) {
      // This messes up flags register so comparisons need to be aware of this optimization
      push_instruction(instructions, *source_range, (Instruction) {.assembly = {xor, {target->operand, target->operand}}});
      return;
    }
    Value *adjusted_source = 0;
    switch(target_size) {
      case 1: {
        adjusted_source = value_from_s8(allocator, s64_to_s8(immediate));
        break;
      }
      case 2: {
        adjusted_source = value_from_s16(allocator, s64_to_s16(immediate));
        break;
      }
      case 4: {
        adjusted_source = value_from_s32(allocator, s64_to_s32(immediate));
        break;
      }
      case 8: {
        if (s64_fits_into_s32(immediate)) {
          adjusted_source = value_from_s32(allocator, s64_to_s32(immediate));
        } else {
          adjusted_source = value_from_s64(allocator, immediate);
        }
        break;
      }
    }
    // Because of 15 byte instruction limit on x86 there is no way to move 64bit immediate
    // to a memory location. In which case we do a move through a temp register
    bool is_64bit_immediate = descriptor_byte_size(adjusted_source->descriptor) == 8;
    if (is_64bit_immediate && target->operand.tag != Operand_Tag_Register) {
      Operand temp = operand_register_for_descriptor(register_acquire_temp(builder), target->descriptor);
      push_instruction(instructions, *source_range, (Instruction) {.assembly = {mov, {temp, adjusted_source->operand}}});
      push_instruction(instructions, *source_range, (Instruction) {.assembly = {mov, {target->operand, temp}}});
      register_release(builder, temp.Register.index);
    } else {
      push_instruction(instructions, *source_range, (Instruction) {.assembly = {mov, {target->operand, adjusted_source->operand}}});
    }
    return;
  }

  // TODO figure out more type checking

  if (target_size != source_size) {
    if (source_size < target_size) {
      // TODO deal with unsigned numbers
      if (target->operand.tag == Operand_Tag_Register) {
        if (source_size == 4) {
          // TODO check whether this correctly sign extends
          Operand adjusted_target = {
            .tag = Operand_Tag_Register,
            .Register = target->operand.Register,
            .byte_size = 4,
          };
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {mov, {adjusted_target, source->operand}}});
        } else {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {movsx, {target->operand, source->operand}}});
        }
      } else {
        Operand temp = operand_register_for_descriptor(register_acquire_temp(builder), target->descriptor);
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {movsx, {temp, source->operand}}});
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {mov, {target->operand, temp}}});
        register_release(builder, temp.Register.index);
      }
      return;
    } else {
      print_operand(&target->operand);
      printf(" ");
      print_operand(&source->operand);
      printf("\nat ");
      source_range_print_start_position(source_range);
      assert(!"Mismatched operand size when moving");
    }
  }

  if (operand_is_memory(&target->operand) && operand_is_memory(&source->operand)) {
    if (target_size >= 16) {
      // TODO probably can use larger chunks for copying but need to check alignment
      Value *temp_rsi = value_register_for_descriptor(allocator, register_acquire_temp(builder), &descriptor_s64);
      Value *temp_rdi = value_register_for_descriptor(allocator, register_acquire_temp(builder), &descriptor_s64);
      Value *temp_rcx = value_register_for_descriptor(allocator, register_acquire_temp(builder), &descriptor_s64);
      {
        Value *reg_rsi = value_register_for_descriptor(allocator, Register_SI, &descriptor_s64);
        Value *reg_rdi = value_register_for_descriptor(allocator, Register_DI, &descriptor_s64);
        Value *reg_rcx = value_register_for_descriptor(allocator, Register_C, &descriptor_s64);
        move_value(allocator, builder, source_range, temp_rsi, reg_rsi);
        move_value(allocator, builder, source_range, temp_rdi, reg_rdi);
        move_value(allocator, builder, source_range, temp_rcx, reg_rcx);

        push_instruction(instructions, *source_range, (Instruction) {.assembly = {lea, {reg_rsi->operand, source->operand}}});
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {lea, {reg_rdi->operand, target->operand}}});
        move_value(allocator, builder, source_range, reg_rcx, value_from_s64(allocator, target_size));
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {rep_movsb}});

        move_value(allocator, builder, source_range, reg_rsi, temp_rsi);
        move_value(allocator, builder, source_range, reg_rdi, temp_rdi);
        move_value(allocator, builder, source_range, reg_rcx, temp_rcx);
      }
      register_release(builder, temp_rsi->operand.Register.index);
      register_release(builder, temp_rdi->operand.Register.index);
      register_release(builder, temp_rcx->operand.Register.index);
    } else {
      Value *temp = value_register_for_descriptor(allocator, register_acquire_temp(builder), target->descriptor);
      move_value(allocator, builder, source_range, temp, source);
      move_value(allocator, builder, source_range, target, temp);
      register_release(builder, temp->operand.Register.index);
    }
    return;
  }

  push_instruction(instructions, *source_range, (Instruction) {.assembly = {mov, {target->operand, source->operand}}});
}

Function_Builder *
fn_begin(
  Compilation_Context *context
) {
  Descriptor *descriptor = allocator_allocate(context->allocator, Descriptor);
  *descriptor = (const Descriptor) {
    .tag = Descriptor_Tag_Function,
    .Function = {
      .flags = 0,
      .arguments = dyn_array_make(Array_Value_Ptr, .allocator = context->allocator),
      .argument_names = dyn_array_make(Array_Slice, .allocator = context->allocator),
      .returns = 0,
    },
  };
  Value *fn_value = allocator_allocate(context->allocator, Value);
  *fn_value = (const Value) {
    .descriptor = descriptor,
    .operand = code_label32(make_label(context->program, &context->program->code_section)),
  };
  Function_Builder *builder = dyn_array_push(context->program->functions, (Function_Builder){
    .layout = {0},
    .value = fn_value,
    .code_block = {
      .end_label = make_label(context->program, &context->program->code_section),
      .instructions = dyn_array_make(Array_Instruction, .allocator = context->allocator),
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

  return builder;
}

void
fn_end(
  Function_Builder *builder
) {
  u8 alignment = 0x8;
  builder->layout.stack_reserve += builder->max_call_parameters_stack_size;
  builder->layout.stack_reserve = s32_align(builder->layout.stack_reserve, 16) + alignment;
  assert(builder->value->descriptor->Function.returns);
}

u32
make_trampoline(
  Compilation_Context *context,
  Fixed_Buffer *buffer,
  s64 address
) {
  u32 result = u64_to_u32(buffer->occupied);
  encode_instruction_with_compiler_location(
    context->program, buffer,
    &(Instruction) {.assembly = {mov, {rax, imm64(context->allocator, address)}}}
  );
  encode_instruction_with_compiler_location(
    context->program, buffer,
    &(Instruction) {.assembly = {jmp, {rax}}}
  );
  return result;
}


void
fn_maybe_remove_unnecessary_jump_from_return_statement_at_the_end_of_function(
  Function_Builder *builder
) {
  Instruction *last_instruction = dyn_array_last(builder->code_block.instructions);
  if (!last_instruction) return;
  if (last_instruction->type != Instruction_Type_Assembly) return;
  if (last_instruction->assembly.mnemonic != jmp) return;
  Operand op = last_instruction->assembly.operands[0];
  if (!operand_is_label(&op)) return;
  if (op.Memory.location.Instruction_Pointer_Relative.label_index.value
    != builder->code_block.end_label.value) return;
  dyn_array_pop(builder->code_block.instructions);
}

s64
fn_adjust_stack_displacement(
  const Function_Builder *builder,
  s64 displacement
) {
  // Negative diplacement is used to encode local variables
  if (displacement < 0) {
    displacement += builder->layout.stack_reserve;
  } else
  // Positive values larger than max_call_parameters_stack_size
  if (displacement >= u32_to_s64(builder->max_call_parameters_stack_size)) {
    // Return address will be pushed on the stack by the caller
    // and we need to account for that
    s32 return_address_size = 8;
    displacement += builder->layout.stack_reserve + return_address_size;
  }
  return displacement;
}

void
fn_normalize_instruction_operands(
  Program *program,
  const Function_Builder *builder,
  Instruction *instruction
) {
  // :OperandNormalization
  // Normalizing operands to simplify future handling in the encoder
  for (u8 operand_index = 0; operand_index < countof(instruction->assembly.operands); ++operand_index) {
    Operand *operand = &instruction->assembly.operands[operand_index];
    if (
      operand->tag == Operand_Tag_Memory &&
      operand->Memory.location.tag == Memory_Location_Tag_Indirect &&
      operand->Memory.location.Indirect.base.Register.index == Register_SP
    ) {
      operand->Memory.location.Indirect.offset =
        fn_adjust_stack_displacement(builder, operand->Memory.location.Indirect.offset);
    }
  }
}

typedef struct {
  u8 register_index;
  u8 offset_in_prolog;
} Function_Pushed_Register;

void
fn_encode(
  Compilation_Context *context,
  Fixed_Buffer *buffer,
  Function_Builder *builder
) {
  Program *program = context->program;
  fn_maybe_remove_unnecessary_jump_from_return_statement_at_the_end_of_function(builder);
  Operand *operand = &builder->value->operand;
  assert(operand_is_label(operand));
  Label_Index label_index = operand->Memory.location.Instruction_Pointer_Relative.label_index;
  Label *label = program_get_label(program, label_index);

  s64 code_base_rva = label->section->base_rva;
  builder->layout.begin_rva = u64_to_u32(code_base_rva + buffer->occupied);
  Operand stack_size_operand = imm_auto_8_or_32(context->allocator, builder->layout.stack_reserve);
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
        builder->layout.volatile_register_push_offsets[push_index++] =
          u64_to_u8(code_base_rva + buffer->occupied - builder->layout.begin_rva);
        Operand to_save = operand_register_for_descriptor(reg_index, &descriptor_s64);
        encode_instruction_with_compiler_location(
          program, buffer, &(Instruction) {.assembly = {push, {to_save}}}
        );
      }
    }
  }

  encode_instruction_with_compiler_location(
    program, buffer, &(Instruction) {.assembly = {sub, {rsp, stack_size_operand}}}
  );
  builder->layout.stack_allocation_offset_in_prolog =
    u64_to_u8(code_base_rva + buffer->occupied - builder->layout.begin_rva);
  builder->layout.size_of_prolog =
    u64_to_u8(code_base_rva + buffer->occupied - builder->layout.begin_rva);


  for (u64 i = 0; i < dyn_array_length(builder->code_block.instructions); ++i) {
    Instruction *instruction = dyn_array_get(builder->code_block.instructions, i);
    fn_normalize_instruction_operands(program, builder, instruction);
    encode_instruction(program, buffer, instruction);
  }

  encode_instruction_with_compiler_location(
    program, buffer, &(Instruction) {
      .type = Instruction_Type_Label, .label = builder->code_block.end_label
    }
  );

  // :ReturnTypeLargerThanRegister
  if(descriptor_byte_size(builder->value->descriptor->Function.returns->descriptor) > 8) {
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
        Operand to_save = operand_register_for_descriptor(reg_index, &descriptor_s64);
        encode_instruction_with_compiler_location(
          program, buffer, &(Instruction) {.assembly = {pop, {to_save}}}
        );
      }
    }
  }

  encode_instruction_with_compiler_location(program, buffer, &(Instruction) {.assembly = {ret, {0}}});
  builder->layout.end_rva = u64_to_u32(code_base_rva + buffer->occupied);

  encode_instruction_with_compiler_location(program, buffer, &(Instruction) {.assembly = {int3, {0}}});
}

void
function_return_descriptor(
  Compilation_Context *context,
  Descriptor_Function *function,
  Descriptor *descriptor
) {
  if (!function->returns) {
    if (descriptor->tag != Descriptor_Tag_Void) {
      // TODO handle 16 bit non-float return values are returned in XMM0
      if (descriptor_is_float(descriptor)) {
        function->returns = value_register_for_descriptor(context->allocator, Register_Xmm0, descriptor);
      } else {
        // :ReturnTypeLargerThanRegister
        u32 byte_size = descriptor_byte_size(descriptor);
        if (byte_size > 8) {
          function->returns = allocator_allocate(context->allocator, Value);
          *function->returns = (Value){
            .descriptor = descriptor,
            .operand = {
              .tag = Operand_Tag_Memory,
              .byte_size = byte_size,
              .Memory.location = {
                .tag = Memory_Location_Tag_Indirect,
                .Indirect = {
                  .base = {
                    .tag = Memory_Indirect_Operand_Tag_Register,
                    .Register.index = Register_C,
                  }
                }
              }
            },
          };
        } else {
          function->returns = value_register_for_descriptor(context->allocator, Register_A, descriptor);
        }
      }
    } else {
      function->returns = &void_value;
    }
  }
}

Label_Index
make_if(
  Compilation_Context *context,
  Array_Instruction *instructions,
  const Source_Range *source_range,
  Value *value
) {
  Program *program = context->program;
  bool is_always_true = false;
  Label_Index label = make_label(program, &program->code_section);
  if(operand_is_immediate(&value->operand)) {
    s64 imm = operand_immediate_value_up_to_s64(&value->operand);
    if (imm == 0) return label;
    is_always_true = true;
  }

  if (!is_always_true) {
    if (value->operand.tag == Operand_Tag_Eflags) {
      switch(value->operand.Eflags.compare_type) {
        case Compare_Type_Equal: {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {jne, {code_label32(label), value->operand, 0}}});
          break;
        }
        case Compare_Type_Not_Equal: {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {je, {code_label32(label), value->operand, 0}}});
          break;
        }

        case Compare_Type_Unsigned_Below: {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {jae, {code_label32(label), value->operand, 0}}});
          break;
        }
        case Compare_Type_Unsigned_Below_Equal: {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {ja, {code_label32(label), value->operand, 0}}});
          break;
        }
        case Compare_Type_Unsigned_Above: {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {jbe, {code_label32(label), value->operand, 0}}});
          break;
        }
        case Compare_Type_Unsigned_Above_Equal: {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {jb, {code_label32(label), value->operand, 0}}});
          break;
        }

        case Compare_Type_Signed_Less: {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {jge, {code_label32(label), value->operand, 0}}});
          break;
        }
        case Compare_Type_Signed_Less_Equal: {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {jg, {code_label32(label), value->operand, 0}}});
          break;
        }
        case Compare_Type_Signed_Greater: {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {jle, {code_label32(label), value->operand, 0}}});
          break;
        }
        case Compare_Type_Signed_Greater_Equal: {
          push_instruction(instructions, *source_range, (Instruction) {.assembly = {jl, {code_label32(label), value->operand, 0}}});
          break;
        }
        default: {
          assert(!"Unsupported comparison");
        }
      }
    } else {
      u32 byte_size = descriptor_byte_size(value->descriptor);
      if (byte_size == 4 || byte_size == 8) {
        push_instruction(
          instructions, *source_range,
          (Instruction) {.assembly = {cmp, {value->operand, imm32(context->allocator, 0), 0}}}
        );
      } else if (byte_size == 1) {
        push_instruction(
          instructions, *source_range,
          (Instruction) {.assembly = {cmp, {value->operand, imm8(context->allocator, 0), 0}}}
        );
      } else {
        assert(!"Unsupported value inside `if`");
      }
      Value *eflags = value_from_compare(context->allocator, Compare_Type_Equal);
      push_instruction(instructions, *source_range, (Instruction) {.assembly = {je, {code_label32(label), eflags->operand, 0}}});
    }
  }
  return label;
}

void
assert_not_register_ax(
  Value *overload
) {
  assert(overload);
  if (overload->operand.tag == Operand_Tag_Register) {
    assert(overload->operand.Register.index != Register_A);
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
      s64 a_s64 = operand_immediate_value_up_to_s64(a_operand);\
      s64 b_s64 = operand_immediate_value_up_to_s64(b_operand);\
      move_value(\
        allocator, (_builder_), (_loc_), (_result_),\
        value_from_signed_immediate(allocator, a_s64 _operator_ b_s64)\
      );\
      return;\
    }\
  } while(0)

void
plus_or_minus(
  Allocator *allocator,
  Arithmetic_Operation operation,
  Function_Builder *builder,
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
      maybe_constant_fold(builder, source_range, result_value, a, b, +);
      break;
    }
    case Arithmetic_Operation_Minus: {
      maybe_constant_fold(builder, source_range, result_value, a, b, -);
      break;
    }
  }

  u32 a_size = descriptor_byte_size(a->descriptor);
  u32 b_size = descriptor_byte_size(b->descriptor);
  Value *maybe_a_or_b_temp = 0;

  if (a_size != b_size) {
    if (a_size > b_size) {
      Value *b_sized_to_a =
        value_register_for_descriptor(allocator, register_acquire_temp(builder), a->descriptor);
      move_value(allocator, builder, source_range, b_sized_to_a, b);
      b = b_sized_to_a;
      maybe_a_or_b_temp = b;
    } else {
      Value *a_sized_to_b =
        value_register_for_descriptor(allocator, register_acquire_temp(builder), b->descriptor);
      move_value(allocator, builder, source_range, a_sized_to_b, b);
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
      if (operand_equal(&result_value->operand, &b->operand) || maybe_a_or_b_temp == b) {
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
    result_value->operand.tag == Operand_Tag_Register &&
    !operand_equal(&result_value->operand, &b->operand)
  );
  Value *temp = 0;
  if (can_reuse_result_as_temp) {
    temp = result_value;
  } else if (maybe_a_or_b_temp == a) {
    temp = a;
  } else {
    temp = value_register_for_descriptor(allocator, register_acquire_temp(builder), a->descriptor);
  }

  move_value(allocator, builder, source_range, temp, a);
  push_instruction(
    &builder->code_block.instructions,
    *source_range,
    (Instruction) {.assembly = {mnemonic, {temp->operand, b->operand}}}
  );
  if (temp != result_value) {
    move_value(allocator, builder, source_range, result_value, temp);
    assert(temp->operand.tag == Operand_Tag_Register);
    register_release(builder, temp->operand.Register.index);
  }
}

void
plus(
  Allocator *allocator,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *result_value,
  Value *a,
  Value *b
) {
  plus_or_minus(allocator, Arithmetic_Operation_Plus, builder, source_range, result_value, a, b);
}

void
minus(
  Allocator *allocator,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *result_value,
  Value *a,
  Value *b
) {
  plus_or_minus(allocator, Arithmetic_Operation_Minus, builder, source_range, result_value, a, b);
}

void
multiply(
  Allocator *allocator,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *result_value,
  Value *x,
  Value *y
) {
  Array_Instruction *instructions = &builder->code_block.instructions;
  assert(same_value_type(x, y));
  assert(descriptor_is_integer(x->descriptor));

  maybe_constant_fold(builder, source_range, result_value, x, y, *);

  // TODO deal with signed / unsigned
  // TODO support double the size of the result?
  Value *y_temp = reserve_stack(allocator, builder, y->descriptor);

  Register temp_register_index = register_acquire_temp(builder);
  Value *temp_register = value_register_for_descriptor(allocator, temp_register_index, y->descriptor);
  move_value(allocator, builder, source_range, temp_register, y);
  move_value(allocator, builder, source_range, y_temp, temp_register);

  temp_register = value_register_for_descriptor(allocator, temp_register_index, x->descriptor);
  move_value(allocator, builder, source_range, temp_register, x);

  push_instruction(
    instructions, *source_range,
    (Instruction) {.assembly = {imul, {temp_register->operand, y_temp->operand}}}
  );

  move_value(allocator, builder, source_range, result_value, temp_register);
  register_release(builder, temp_register_index);
}

typedef enum {
  Divide_Operation_Divide,
  Divide_Operation_Remainder,
} Divide_Operation;

void
divide_or_remainder(
  Allocator *allocator,
  Divide_Operation operation,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *result_value,
  Value *a,
  Value *b
) {
  Array_Instruction *instructions = &builder->code_block.instructions;
  assert(same_value_type_or_can_implicitly_move_cast(a, b));
  assert(descriptor_is_integer(a->descriptor));

  if (operand_is_immediate(&a->operand) && operand_is_immediate(&a->operand)) {
    s64 divident = operand_immediate_value_up_to_s64(&a->operand);
    s64 divisor = operand_immediate_value_up_to_s64(&b->operand);
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
    Value *folded_value = value_from_signed_immediate(allocator, folded);
    move_value(allocator, builder, source_range, result_value, folded_value);
    return;
  }

  // Save RDX as it will be used for the remainder
  Value *rdx_temp = reserve_stack(allocator, builder, &descriptor_s64);

  Value *reg_rdx = value_register_for_descriptor(allocator, Register_D, &descriptor_s64);
  move_value(allocator, builder, source_range, rdx_temp, reg_rdx);

  Descriptor *larger_descriptor =
    descriptor_byte_size(a->descriptor) > descriptor_byte_size(b->descriptor)
    ? a->descriptor
    : b->descriptor;

  // TODO deal with signed / unsigned
  Value *divisor = reserve_stack(allocator, builder, larger_descriptor);
  move_value(allocator, builder, source_range, divisor, b);

  Value *reg_a = value_register_for_descriptor(allocator, Register_A, larger_descriptor);
  {
    move_value(allocator, builder, source_range, reg_a, a);

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
  push_instruction(instructions, *source_range, (Instruction) {.assembly = {idiv, {divisor->operand, 0, 0}}});


  if (operation == Divide_Operation_Divide) {
    move_value(allocator, builder, source_range, result_value, reg_a);
  } else {
    if (descriptor_byte_size(larger_descriptor) == 1) {
      Value *temp_result = value_register_for_descriptor(allocator, Register_AH, larger_descriptor);
      move_value(allocator, builder, source_range, result_value, temp_result);
    } else {
      Value *temp_result = value_register_for_descriptor(allocator, Register_D, larger_descriptor);
      move_value(allocator, builder, source_range, result_value, temp_result);
    }
  }

  // Restore RDX
  move_value(allocator, builder, source_range, reg_rdx, rdx_temp);
}

void
divide(
  Allocator *allocator,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *result_value,
  Value *a,
  Value *b
) {
  divide_or_remainder(allocator, Divide_Operation_Divide, builder, source_range, result_value, a, b);
}

void
value_remainder(
  Allocator *allocator,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *result_value,
  Value *a,
  Value *b
) {
  divide_or_remainder(allocator, Divide_Operation_Remainder, builder, source_range, result_value, a, b);
}


void
compare(
  Allocator *allocator,
  Compare_Type operation,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *result_value,
  Value *a,
  Value *b
) {
  Array_Instruction *instructions = &builder->code_block.instructions;
  assert(descriptor_is_integer(a->descriptor));
  assert(same_value_type_or_can_implicitly_move_cast(a, b));

  switch(operation) {
    case Compare_Type_Equal: {
      maybe_constant_fold(builder, source_range, result_value, a, b, ==);
      break;
    }
    case Compare_Type_Not_Equal: {
      maybe_constant_fold(builder, source_range, result_value, a, b, !=);
      break;
    }

    case Compare_Type_Unsigned_Below: {
      maybe_constant_fold(builder, source_range, result_value, a, b, <);
      break;
    }
    case Compare_Type_Unsigned_Below_Equal: {
      maybe_constant_fold(builder, source_range, result_value, a, b, <=);
      break;
    }
    case Compare_Type_Unsigned_Above: {
      maybe_constant_fold(builder, source_range, result_value, a, b, >);
      break;
    }
    case Compare_Type_Unsigned_Above_Equal: {
      maybe_constant_fold(builder, source_range, result_value, a, b, >=);
      break;
    }

    case Compare_Type_Signed_Less: {
      maybe_constant_fold(builder, source_range, result_value, a, b, <);
      break;
    }
    case Compare_Type_Signed_Less_Equal: {
      maybe_constant_fold(builder, source_range, result_value, a, b, <=);
      break;
    }
    case Compare_Type_Signed_Greater: {
      maybe_constant_fold(builder, source_range, result_value, a, b, >);
      break;
    }
    case Compare_Type_Signed_Greater_Equal: {
      maybe_constant_fold(builder, source_range, result_value, a, b, >=);
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
  move_value(allocator, builder, source_range, temp_b, b);

  Value *reg_r11 = value_register_for_descriptor(allocator, Register_R11, larger_descriptor);
  move_value(allocator, builder, source_range, reg_r11, a);

  push_instruction(instructions, *source_range, (Instruction) {.assembly = {cmp, {reg_r11->operand, temp_b->operand, 0}}});

  Value *comparison_value = value_from_compare(allocator, operation);
  move_value(allocator, builder, source_range, result_value, comparison_value);
}

Value *
value_pointer_to(
  Compilation_Context *context,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *value
) {
  Array_Instruction *instructions = &builder->code_block.instructions;
  // TODO support register
  // TODO support immediates
  assert(value->operand.tag == Operand_Tag_Memory);
  Descriptor *result_descriptor = descriptor_pointer_to(context->allocator, value->descriptor);

  Value *reg_a = value_register_for_descriptor(context->allocator, Register_A, result_descriptor);
  Operand source_operand = value->operand;

  // TODO rethink operand sizing
  // We need to manually adjust the size here because even if we loading one byte
  // the right side is treated as an opaque address and does not participate in
  // instruction encoding.
  source_operand.byte_size = descriptor_byte_size(result_descriptor);

  push_instruction(instructions, *source_range, (Instruction) {.assembly = {lea, {reg_a->operand, source_operand, 0}}});

  Value *result = reserve_stack(context->allocator, builder, result_descriptor);
  move_value(context->allocator, builder, source_range, result, reg_a);

  return result;
}

typedef struct {
  Value saved;
  Value *stack_value;
} Saved_Register;
typedef dyn_array_type(Saved_Register) Array_Saved_Register;

Value *
call_function_overload(
  Compilation_Context *context,
  const Source_Range *source_range,
  Value *to_call,
  Array_Value_Ptr arguments
) {
  Function_Builder *builder = context->builder;
  Array_Instruction *instructions = &builder->code_block.instructions;
  assert(to_call->descriptor->tag == Descriptor_Tag_Function);
  Descriptor_Function *descriptor = &to_call->descriptor->Function;
  assert(dyn_array_length(descriptor->arguments) == dyn_array_length(arguments));

  Array_Saved_Register saved_array = dyn_array_make(Array_Saved_Register);

  for (Register reg_index = 0; reg_index <= Register_R15; ++reg_index) {
    if (register_bitset_get(builder->code_block.register_volatile_bitset, reg_index)) {
      if (register_bitset_get(builder->code_block.register_occupied_bitset, reg_index)) {
        Value to_save = {
          .descriptor = &descriptor_s64,
          .operand = {
            .tag = Operand_Tag_Register,
            .byte_size = 8,
            .Register.index = reg_index,
          }
        };
        Operand source = operand_register_for_descriptor(reg_index, &descriptor_s64);
        Value *stack_value = reserve_stack(context->allocator, builder, to_save.descriptor);
        push_instruction(instructions, *source_range, (Instruction) {.assembly = {mov, {stack_value->operand, source}}});
        dyn_array_push(saved_array, (Saved_Register){.saved = to_save, .stack_value = stack_value});
      }
    }
  }

  for (u64 i = 0; i < dyn_array_length(arguments); ++i) {
    Value *source_arg = *dyn_array_get(arguments, i);
    Value *target_arg = *dyn_array_get(descriptor->arguments, i);
    move_value(context->allocator, builder, source_range, target_arg, source_arg);
  }

  // If we call a function, then we need to reserve space for the home
  // area of at least 4 arguments?
  u64 parameters_stack_size = u64_max(4, dyn_array_length(arguments)) * 8;

  // :ReturnTypeLargerThanRegister
  u32 return_size = descriptor_byte_size(descriptor->returns->descriptor);
  if (return_size > 8) {
    parameters_stack_size += return_size;
    Descriptor *return_pointer_descriptor =
      descriptor_pointer_to(context->allocator, descriptor->returns->descriptor);
    Value *reg_c = value_register_for_descriptor(context->allocator, Register_C, return_pointer_descriptor);
    push_instruction(
      instructions, *source_range,
      (Instruction) {.assembly = {lea, {reg_c->operand, descriptor->returns->operand}}}
    );
  }

  builder->max_call_parameters_stack_size = u64_to_u32(u64_max(
    builder->max_call_parameters_stack_size,
    parameters_stack_size
  ));

  if (operand_is_label(&to_call->operand)) {
    push_instruction(instructions, *source_range, (Instruction) {.assembly = {call, {to_call->operand, 0, 0}}});
  } else {
    Value *reg_a = value_register_for_descriptor(context->allocator, Register_A, to_call->descriptor);
    move_value(context->allocator, builder, source_range, reg_a, to_call);
    push_instruction(
      instructions, *source_range, (Instruction) {.assembly = {call, {reg_a->operand, 0, 0}}}
    );
  }

  Value *result = descriptor->returns;
  if (return_size <= 8) {
    if (return_size != 0) {
      result = reserve_stack(context->allocator, builder, descriptor->returns->descriptor);
      move_value(context->allocator, builder, source_range, result, descriptor->returns);
    }
  }

  for (u64 i = 0; i < dyn_array_length(saved_array); ++i) {
    Saved_Register *reg = dyn_array_get(saved_array, i);
    move_value(context->allocator, builder, source_range, &reg->saved, reg->stack_value);
    // TODO :FreeStackAllocation
  }

  return result;
}


s64
calculate_arguments_match_score(
  Descriptor_Function *descriptor,
  Array_Value_Ptr arguments
) {
  enum {
    // TODO consider relationship between casts and literal types
    Score_Exact_Literal = 1000000,
    Score_Exact_Type = 1000,
    Score_Cast = 1,
  };
  assert(dyn_array_length(arguments) < 1000);
  s64 score = 0;
  for (u64 arg_index = 0; arg_index < dyn_array_length(arguments); ++arg_index) {
    Value *source_arg = *dyn_array_get(arguments, arg_index);
    Value *target_arg = *dyn_array_get(descriptor->arguments, arg_index);
    if (same_value_type(target_arg, source_arg)) {
      if (
        operand_is_immediate(&target_arg->operand) &&
        operand_equal(&target_arg->operand, &source_arg->operand)
      ) {
        score += Score_Exact_Literal;
      } else {
        score += Score_Exact_Type;
      }
    } else if(same_value_type_or_can_implicitly_move_cast(target_arg, source_arg)) {
      score += Score_Cast;
    } else {
      return -1;
    }
  }
  return score;
}

Value *
make_and(
  Compilation_Context *context,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *a,
  Value *b
) {
  Program *program = context->program;
  Array_Instruction *instructions = &builder->code_block.instructions;
  Value *result = reserve_stack(context->allocator, builder, &descriptor_s8);
  Label_Index label = make_label(program, &program->code_section);

  Value zero = {
    .descriptor = &descriptor_s8,
    .operand = imm8(context->allocator, 0),
  };

  Label_Index else_label = make_if(context, instructions, source_range, a);
  {
    compare(context->allocator, Compare_Type_Not_Equal, builder, source_range, result, b, &zero);
    push_instruction(instructions, *source_range, (Instruction) {.assembly = {jmp, {code_label32(label)}}});
  }
  push_instruction(instructions, *source_range, (Instruction) {
    .type = Instruction_Type_Label,
    .label = else_label,
  });

  move_value(context->allocator, builder, source_range, result, &zero);
  push_instruction(instructions, *source_range, (Instruction) {
    .type = Instruction_Type_Label,
    .label = label,
  });
  return result;
}

Value *
make_or(
  Compilation_Context *context,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *a,
  Value *b
) {
  Program *program = context->program;
  Array_Instruction *instructions = &builder->code_block.instructions;
  Value *result = reserve_stack(context->allocator, builder, &descriptor_s8);
  Label_Index label = make_label(program, &program->code_section);

  Value zero = {
    .descriptor = &descriptor_s8,
    .operand = imm8(context->allocator, 0),
  };

  compare(context->allocator, Compare_Type_Equal, builder, source_range, result, a, &zero);
  Label_Index else_label = make_if(context, instructions, source_range, result);
  {
    compare(context->allocator, Compare_Type_Not_Equal, builder, source_range, result, b, &zero);
    push_instruction(instructions, *source_range, (Instruction) {.assembly = {jmp, {code_label32(label)}}});
  }
  push_instruction(instructions, *source_range, (Instruction) {
    .type = Instruction_Type_Label,
    .label = else_label,
  });

  Value one = {
    .descriptor = &descriptor_s8,
    .operand = imm8(context->allocator, 1),
  };
  move_value(context->allocator, builder, source_range, result, &one);
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
  Operand operand = value->operand;
  if (operand.tag == Operand_Tag_Memory) return value;
  Value *result = allocator_allocate(allocator, Value);
  if (value->descriptor->tag != Descriptor_Tag_Pointer) assert(!"Not implemented");
  if (value->operand.tag != Operand_Tag_Register) assert(!"Not implemented");
  *result = (const Value) {
    .descriptor = value->descriptor->Pointer.to,
    .operand = {
      .tag = Operand_Tag_Memory,
      .Memory.location = {
        .tag = Memory_Location_Tag_Indirect,
        .Indirect = {
          .base = {
            .tag = Memory_Indirect_Operand_Tag_Register,
            .Register.index = value->operand.Register.index,
          }
        }
      }
    },
  };
  return result;
}

Value *
struct_get_field(
  Allocator *allocator,
  Value *raw_value,
  Slice name
) {
  Value *struct_value = ensure_memory(allocator, raw_value);
  Descriptor *descriptor = struct_value->descriptor;
  assert(descriptor->tag == Descriptor_Tag_Struct);
  for (u64 i = 0; i < dyn_array_length(descriptor->Struct.fields); ++i) {
    Descriptor_Struct_Field *field = dyn_array_get(descriptor->Struct.fields, i);
    if (slice_equal(name, field->name)) {
      Value *result = allocator_allocate(allocator, Value);
      Operand operand = struct_value->operand;
      // FIXME support more operands
      assert(operand.tag == Operand_Tag_Memory);
      assert(operand.Memory.location.tag == Memory_Location_Tag_Indirect);
      operand.byte_size = descriptor_byte_size(field->descriptor);
      operand.Memory.location.Indirect.offset += field->offset;
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








