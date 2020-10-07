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


Value *
ensure_register_or_memory(
  Array_Instruction *instructions,
  const Source_Location *location,
  Value *value
) {
  assert(value->operand.type != Operand_Type_None);
  if (operand_is_immediate(&value->operand)) {
    Value *result = value_register_for_descriptor(Register_A, value->descriptor);
    move_value(instructions, location, result, value);
    return result;
  }
  return value;
}

Value *
ensure_register(
  Array_Instruction *instructions,
  const Source_Location *location,
  Value *value,
  Register reg
) {
  assert(value->operand.type != Operand_Type_None);
  if (value->operand.type != Operand_Type_Register) {
    Value *result = value_register_for_descriptor(reg, value->descriptor);
    move_value(instructions, location, result, value);
    return result;
  }
  return value;
}

void
move_value(
  Array_Instruction *instructions,
  const Source_Location *location,
  Value *target,
  Value *source
) {
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
      move_value(instructions, location, reg_xmm4, source);
      move_value(instructions, location, target, reg_xmm4);
      return;
    }
  }

  // TODO figure out more type checking

  if (target_size != source_size) {
    if (source_size < target_size) {
      // TODO deal with unsigned numbers
      source = ensure_register_or_memory(instructions, location, source);
      if (target->operand.type == Operand_Type_Register) {
        if (source_size == 4) {
          Operand adjusted_target = {
            .type = Operand_Type_Register,
            .reg = target->operand.reg,
            .byte_size = 4,
          };
          push_instruction(instructions, location, (Instruction) {mov, {adjusted_target, source->operand, 0}});
        } else {
          push_instruction(instructions, location, (Instruction) {movsx, {target->operand, source->operand, 0}});
        }
      } else {
        Value *reg_a = value_register_for_descriptor(Register_A, target->descriptor);
        push_instruction(instructions, location, (Instruction) {movsx, {reg_a->operand, source->operand, 0}});
        push_instruction(instructions, location, (Instruction) {mov, {target->operand, reg_a->operand, 0}});
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
    Value *reg_a = value_register_for_descriptor(Register_A, target->descriptor);
    move_value(instructions, location, reg_a, source);
    move_value(instructions, location, target, reg_a);
    return;
  }

  if (
    target->operand.type == Operand_Type_Register &&
    (
      (source->operand.type == Operand_Type_Immediate_64 && source->operand.imm64 == 0) ||
      (source->operand.type == Operand_Type_Immediate_32 && source->operand.imm32 == 0) ||
      (source->operand.type == Operand_Type_Immediate_16 && source->operand.imm16 == 0) ||
      (source->operand.type == Operand_Type_Immediate_8  && source->operand.imm8 == 0)
    )
  ) {
    // This messes up flags register so comparisons need to be aware of this optimization
    push_instruction(instructions, location, (Instruction) {xor, {target->operand, target->operand, 0}});
    return;
  }

  //if (source->operand.type == Operand_Type_Immediate_64 && ((s32)source->operand.imm64) >= 0) {
    //move_value(builder, target, value_from_s32(source->operand.imm32));
  //}

  if ((
    target->operand.type != Operand_Type_Register &&
    source->operand.type == Operand_Type_Immediate_64
  ) || (
    operand_is_memory(&target->operand) &&
    operand_is_memory(&source->operand)
  )) {
    Value *reg_a = value_register_for_descriptor(Register_A, target->descriptor);
    move_value(instructions, location, reg_a, source);
    move_value(instructions, location, target, reg_a);
  } else {
    push_instruction(instructions, location, (Instruction) {mov, {target->operand, source->operand, 0}});
  }
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
  Label *prolog_label = make_label();
  Value *fn_value = temp_allocate(Value);
  *fn_value = (const Value) {
    .descriptor = descriptor,
    .operand = label32(prolog_label),
  };
  Function_Builder *builder = dyn_array_push(program->functions, (Function_Builder){
    .program = program,
    .stack_reserve = 0,
    .prolog_label = prolog_label,
    .epilog_label = make_label(),
    .descriptor = descriptor,
    .value = fn_value,
    .instructions = dyn_array_make(Array_Instruction, .allocator = temp_allocator),
  });
  *result = fn_value;

  return builder;
}

void
fn_ensure_frozen(
  Descriptor_Function *function
) {
  if(function->frozen) return;

  if (!function->returns) {
    function->returns = &void_value;
  }
  function->frozen = true;
}

void
fn_freeze(
  Function_Builder *builder
) {
  fn_ensure_frozen(&builder->descriptor->function);
}

bool
fn_is_frozen(
  Function_Builder *builder
) {
  return builder->descriptor->function.frozen;
}

void
fn_end(
  Function_Builder *builder
) {
  u8 alignment = 0x8;
  builder->stack_reserve += builder->max_call_parameters_stack_size;
  builder->stack_reserve = s32_align(builder->stack_reserve, 16) + alignment;

  fn_freeze(builder);
}

void
fn_maybe_remove_unnecessary_jump_from_return_statement_at_the_end_of_function(
  Function_Builder *builder
) {
  Instruction *last_instruction = dyn_array_last(builder->instructions);
  if (!last_instruction) return;
  if (last_instruction->maybe_label) return;
  if (last_instruction->mnemonic != jmp) return;
  Operand op = last_instruction->operands[0];
  if (op.type != Operand_Type_Label_32) return;
  if (op.label32 != builder->epilog_label) return;
  dyn_array_pop(builder->instructions);
}

void
fn_encode(
  Fixed_Buffer *buffer,
  Function_Builder *builder,
  RUNTIME_FUNCTION *function_exception_info,
  UNWIND_INFO *unwind_info,
  u32 unwind_data_rva
) {
  fn_maybe_remove_unnecessary_jump_from_return_statement_at_the_end_of_function(builder);

  s64 code_base_rva = builder->program->code_base_rva;
  u32 fn_start_rva = u64_to_u32(code_base_rva + buffer->occupied);
  encode_instruction(buffer, builder, (Instruction) {.maybe_label = builder->prolog_label});
  encode_instruction(buffer, builder, (Instruction) {sub, {rsp, imm_auto(builder->stack_reserve), 0}});
  u32 stack_allocation_offset_in_prolog =
    u64_to_u32(code_base_rva + buffer->occupied) - fn_start_rva;
  u32 size_of_prolog = u64_to_u32(code_base_rva + buffer->occupied) - fn_start_rva;

  for (u64 i = 0; i < dyn_array_length(builder->instructions); ++i) {
    Instruction *instruction = dyn_array_get(builder->instructions, i);
    encode_instruction(buffer, builder, *instruction);
  }

  encode_instruction(buffer, builder, (Instruction) {.maybe_label = builder->epilog_label});
  encode_instruction(buffer, builder, (Instruction) {add, {rsp, imm_auto(builder->stack_reserve), 0}});

  encode_instruction(buffer, builder, (Instruction) {ret, {0}});
  u32 fn_end_rva = u64_to_u32(code_base_rva + buffer->occupied);

  encode_instruction(buffer, builder, (Instruction) {int3, {0}});

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

    if (builder->stack_reserve) {
      assert(builder->stack_reserve >= 8);
      assert(builder->stack_reserve % 8 == 0);
      if (builder->stack_reserve <= 128) {
        unwind_info->CountOfCodes = 1;
        unwind_info->UnwindCode[0] = (UNWIND_CODE){
          .CodeOffset = u32_to_u8(stack_allocation_offset_in_prolog),
          .UnwindOp = UWOP_ALLOC_SMALL,
          .OpInfo = (builder->stack_reserve - 8) / 8,
        };
      } else {
        unwind_info->CountOfCodes = 2;
        unwind_info->UnwindCode[0] = (UNWIND_CODE){
          .CodeOffset = u32_to_u8(stack_allocation_offset_in_prolog),
          .UnwindOp = UWOP_ALLOC_LARGE,
          .OpInfo = 0,
        };
        unwind_info->UnwindCode[1] = (UNWIND_CODE){
          .DataForPreviousCode = u32_to_u16(builder->stack_reserve / 8),
        };
        // TODO support 512k + allocations
      }
    }
    *function_exception_info = (RUNTIME_FUNCTION) {
      .BeginAddress = fn_start_rva,
      .EndAddress = fn_end_rva,
      .UnwindData = unwind_data_rva,
    };
  }

  dyn_array_destroy(builder->instructions);
}

Value *
fn_arg(
  Function_Builder *builder,
  Descriptor *descriptor
) {
  assert(!fn_is_frozen(builder));
  Descriptor_Function *function = &builder->descriptor->function;
  Value *result = function_push_argument(function, descriptor);
  return result;
}

void
function_return_descriptor(
  Descriptor_Function *function,
  Descriptor *descriptor,
  Function_Return_Type return_type
) {
  if (!function->returns) {
    if (descriptor->type != Descriptor_Type_Void) {
      // TODO handle 16 bit non-float return values are returned in XMM0
      if (descriptor->type == Descriptor_Type_Float) {
        function->returns = value_register_for_descriptor(Register_Xmm0, descriptor);
      } else {
        function->returns = value_register_for_descriptor(Register_A, descriptor);
      }
    } else {
      function->returns = &void_value;
    }
  }
}

void
fn_return(
  Function_Builder *builder,
  const Source_Location *location,
  Value *to_return,
  Function_Return_Type return_type
) {
  function_return_descriptor(&builder->descriptor->function, to_return->descriptor, return_type);
  if (builder->descriptor->function.returns->descriptor->type == Descriptor_Type_Void) {
    if (to_return->descriptor->type != Descriptor_Type_Void) {
      assert(return_type == Function_Return_Type_Implicit);
    }
  } else {
    move_value(&builder->instructions, location, builder->descriptor->function.returns, to_return);
  }

  push_instruction(
    &builder->instructions, location, (Instruction){jmp, {label32(builder->epilog_label), 0, 0}}
  );
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
    u32 byte_size = descriptor_byte_size(value->descriptor);
    if (byte_size == 4 || byte_size == 8) {
      push_instruction(instructions, location, (Instruction) {cmp, {value->operand, imm32(0), 0}});
    } else if (byte_size == 1) {
      push_instruction(instructions, location, (Instruction) {cmp, {value->operand, imm8(0), 0}});
    } else {
      assert(!"Unsupported value inside `if`");
    }
    push_instruction(instructions, location, (Instruction) {jz, {label32(label), 0, 0}});
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
loop_end(
  Array_Instruction *instructions,
  const Source_Location *location,
  Loop_Builder *loop
) {
  push_instruction(instructions, location, (Instruction) {jmp, {label32(loop->label_start), 0, 0}});
  push_instruction(instructions, location, (Instruction) { .maybe_label = loop->label_end });
  loop->done = true;
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

#define maybe_constant_fold(_a_, _b_, _operator_)\
  do {\
    Operand *a_operand = &(_a_)->operand;\
    Operand *b_operand = &(_b_)->operand;\
    if (operand_is_immediate(a_operand) && operand_is_immediate(b_operand)) {\
      s64 a_s64 = operand_immediate_as_s64(a_operand);\
      s64 b_s64 = operand_immediate_as_s64(b_operand);\
      return value_from_signed_immediate(a_s64 _operator_ b_s64);\
    }\
  } while(0)

void
plus_or_minus(
  Arithmetic_Operation operation,
  Array_Instruction *instructions,
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
    move_value(instructions, location, result_value, value_from_signed_immediate(folded));
    return;
  }

  const X64_Mnemonic *mnemonic = 0;
  switch(operation) {
    case Arithmetic_Operation_Plus: {
      mnemonic = add;
      // Addition is commutative (a + b == b + a)
      // so we can swap operands and save one instruction
      if (operand_equal(&result_value->operand, &b->operand)) {
        Value *swap_temp = a;
        a = b;
        b = swap_temp;
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
    : value_register_for_descriptor(Register_R11, a->descriptor); // TODO register allocation

  move_value(instructions, location, temp, a);
  push_instruction(instructions, location, (Instruction) {mnemonic, {temp->operand, b->operand}});
  move_value(instructions, location, result_value, temp);
}

void
plus(
  Array_Instruction *instructions,
  const Source_Location *location,
  Value *result_value,
  Value *a,
  Value *b
) {
  plus_or_minus(Arithmetic_Operation_Plus, instructions, location, result_value, a, b);
}

void
minus(
  Array_Instruction *instructions,
  const Source_Location *location,
  Value *result_value,
  Value *a,
  Value *b
) {
  plus_or_minus(Arithmetic_Operation_Minus, instructions, location, result_value, a, b);
}

Value *
multiply(
  Function_Builder *builder,
  const Source_Location *location,
  Value *x,
  Value *y
) {
  assert(same_value_type(x, y));
  assert(x->descriptor->type == Descriptor_Type_Integer);

  assert_not_register_ax(x);
  assert_not_register_ax(y);

  maybe_constant_fold(x, y, *);

  // TODO deal with signed / unsigned
  // TODO support double the size of the result?
  // TODO make the move only for imm value
  Value *y_temp = reserve_stack(builder, y->descriptor);

  Value *reg_a = value_register_for_descriptor(Register_A, y->descriptor);
  move_value(&builder->instructions, location, reg_a, y);
  move_value(&builder->instructions, location, y_temp, reg_a);

  reg_a = value_register_for_descriptor(Register_A, x->descriptor);
  move_value(&builder->instructions, location, reg_a, x);

  push_instruction(&builder->instructions, location, (Instruction) {imul, {reg_a->operand, y_temp->operand}});

  Value *temp = reserve_stack(builder, x->descriptor);
  move_value(&builder->instructions, location, temp, reg_a);

  return temp;
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
    move_value(&builder->instructions, location, result_value, value_from_signed_immediate(folded));
    return;
  }

  // Save RDX as it will be used for the remainder
  Value *rdx_temp = reserve_stack(builder, &descriptor_s64);

  Value *reg_rdx = value_register_for_descriptor(Register_A, &descriptor_s64);
  move_value(&builder->instructions, location, rdx_temp, reg_rdx);

  Descriptor *larger_descriptor =
    descriptor_byte_size(a->descriptor) > descriptor_byte_size(b->descriptor)
    ? a->descriptor
    : b->descriptor;

  // TODO deal with signed / unsigned
  Value *divisor = reserve_stack(builder, larger_descriptor);
  move_value(&builder->instructions, location, divisor, b);

  Value *reg_a = value_register_for_descriptor(Register_A, larger_descriptor);
  {
    move_value(&builder->instructions, location, reg_a, a);

    switch (descriptor_byte_size(larger_descriptor)) {
      case 8: {
        push_instruction(&builder->instructions, location, (Instruction) {cqo, {0}});
        break;
      }
      case 4: {
        push_instruction(&builder->instructions, location, (Instruction) {cdq, {0}});
        break;
      }
      case 2: {
        push_instruction(&builder->instructions, location, (Instruction) {cwd, {0}});
        break;
      }
      case 1: {
        push_instruction(&builder->instructions, location, (Instruction) {cwb, {0}});
        break;
      }
      default: {
        assert(!"Unsupported byte size when dividing");
      }
    }
  }
  push_instruction(&builder->instructions, location, (Instruction) {idiv, {divisor->operand, 0, 0}});


  if (operation == Divide_Operation_Divide) {
    move_value(&builder->instructions, location, result_value, reg_a);
  } else {
    if (descriptor_byte_size(larger_descriptor) == 1) {
      Value *temp_result = value_register_for_descriptor(Register_AH, larger_descriptor);
      move_value(&builder->instructions, location, result_value, temp_result);
    } else {
      Value *temp_result = value_register_for_descriptor(Register_D, larger_descriptor);
      move_value(&builder->instructions, location, result_value, temp_result);
    }
  }

  // Restore RDX
  move_value(&builder->instructions, location, reg_rdx, rdx_temp);
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
remainder(
  Function_Builder *builder,
  const Source_Location *location,
  Value *result_value,
  Value *a,
  Value *b
) {
  divide_or_remainder(Divide_Operation_Remainder, builder, location, result_value, a, b);
}


Value *
compare(
  Compare_Type operation,
  Function_Builder *builder,
  const Source_Location *location,
  Value *a,
  Value *b
) {
  assert(a->descriptor->type == Descriptor_Type_Integer);
  assert(b->descriptor->type == Descriptor_Type_Integer);

  switch(operation) {
    case Compare_Type_Equal: {
      maybe_constant_fold(a, b, ==);
      break;
    }
    case Compare_Type_Not_Equal: {
      maybe_constant_fold(a, b, !=);
      break;
    }
    case Compare_Type_Less: {
      maybe_constant_fold(a, b, <);
      break;
    }
    case Compare_Type_Greater: {
      maybe_constant_fold(a, b, >);
      break;
    }
    case Compare_Type_Less_Equal: {
      maybe_constant_fold(a, b, <=);
      break;
    }
    case Compare_Type_Greater_Equal: {
      maybe_constant_fold(a, b, >=);
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
  move_value(&builder->instructions, location, temp_b, b);

  Value *reg_a = value_register_for_descriptor(Register_A, larger_descriptor);
  move_value(&builder->instructions, location,  reg_a, a);

  push_instruction(&builder->instructions, location, (Instruction) {cmp, {reg_a->operand, temp_b->operand, 0}});
  Value *result = reserve_stack(builder, &descriptor_s8);

  switch(operation) {
    case Compare_Type_Equal: {
      push_instruction(&builder->instructions, location, (Instruction) {setz, {result->operand, 0, 0}});
      break;
    }
    case Compare_Type_Not_Equal: {
      push_instruction(&builder->instructions, location, (Instruction) {setne, {result->operand, 0, 0}});
      break;
    }
    case Compare_Type_Less: {
      push_instruction(&builder->instructions, location, (Instruction) {setl, {result->operand, 0, 0}});
      break;
    }
    case Compare_Type_Less_Equal: {
      push_instruction(&builder->instructions, location, (Instruction) {setle, {result->operand, 0, 0}});
      break;
    }
    case Compare_Type_Greater: {
      push_instruction(&builder->instructions, location, (Instruction) {setg, {result->operand, 0, 0}});
      break;
    }
    case Compare_Type_Greater_Equal: {
      push_instruction(&builder->instructions, location, (Instruction) {setge, {result->operand, 0, 0}});
      break;
    }
    default: {
      assert(!"Unsupported comparison");
    }
  }
  return result;
}

Value *
value_pointer_to(
  Function_Builder *builder,
  const Source_Location *location,
  Value *value
) {
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

  push_instruction(&builder->instructions, location, (Instruction) {lea, {reg_a->operand, source_operand, 0}});

  Value *result = reserve_stack(builder, result_descriptor);
  move_value(&builder->instructions, location, result, reg_a);

  return result;
}


Value *
call_function_overload(
  Function_Builder *builder,
  const Source_Location *location,
  Value *to_call,
  Array_Value_Ptr arguments
) {
  assert(to_call->descriptor->type == Descriptor_Type_Function);
  Descriptor_Function *descriptor = &to_call->descriptor->function;
  assert(dyn_array_length(descriptor->arguments) == dyn_array_length(arguments));

  fn_ensure_frozen(descriptor);

  for (u64 i = 0; i < dyn_array_length(arguments); ++i) {
    Value *source_arg = *dyn_array_get(arguments, i);
    Value *target_arg = *dyn_array_get(descriptor->arguments, i);
    move_value(&builder->instructions, location, target_arg, source_arg);
  }

  // If we call a function, then we need to reserve space for the home
  // area of at least 4 arguments?
  u64 parameters_stack_size = u64_max(4, dyn_array_length(arguments)) * 8;

  // FIXME support this for fns that accept arguments
  u32 return_size = descriptor_byte_size(descriptor->returns->descriptor);
  if (return_size > 8) {
    parameters_stack_size += return_size;
    Descriptor *return_pointer_descriptor = descriptor_pointer_to(descriptor->returns->descriptor);
    Value *reg_c =
      value_register_for_descriptor(Register_C, return_pointer_descriptor);
    push_instruction(
      &builder->instructions, location,
      (Instruction) {lea, {reg_c->operand, descriptor->returns->operand, 0}}
    );
  }

  builder->max_call_parameters_stack_size = u64_to_u32(u64_max(
    builder->max_call_parameters_stack_size,
    parameters_stack_size
  ));

  if (to_call->operand.type == Operand_Type_Label_32) {
    push_instruction(&builder->instructions, location, (Instruction) {call, {to_call->operand, 0, 0}});
  } else {
    Value *reg_a = value_register_for_descriptor(Register_A, to_call->descriptor);
    move_value(&builder->instructions, location, reg_a, to_call);
    push_instruction(&builder->instructions, location, (Instruction) {call, {reg_a->operand, 0, 0}});
  }

  if (return_size <= 8) {
    if (return_size != 0) {
      Value *result = reserve_stack(builder, descriptor->returns->descriptor);
      move_value(&builder->instructions, location, result, descriptor->returns);
      return result;
    }
  }

  return descriptor->returns;
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
  dyn_array_destroy(arguments);
  assert(!"No matching overload found");
  return 0;
}

Value *
call_function_value(
  Function_Builder *builder,
  const Source_Location *location,
  Value *to_call,
  ...
) {
  assert(to_call);
  Array_Value_Ptr arguments = dyn_array_make(Array_Value_Ptr, .allocator = temp_allocator);
  {
    va_list va_values;
    va_start(va_values, to_call);
    for(;;) {
      Value *arg = va_arg(va_values, Value *);
      if (!arg) break;
      dyn_array_push(arguments, arg);
    }
  }
  return call_function_value_array(builder, location, to_call, arguments);
}

Value *
make_and(
  Function_Builder *builder,
  const Source_Location *location,
  Value *a,
  Value *b
) {
  Value *result = reserve_stack(builder, &descriptor_s8);
  Label *label = make_label();

  Label *else_label = make_if(&builder->instructions, location, a);
  {
    Value *rhs = compare(Compare_Type_Not_Equal, builder, location, b, value_from_s8(0));
    move_value(&builder->instructions, location, result, rhs);
    push_instruction(&builder->instructions, location, (Instruction) {jmp, {label32(label), 0, 0}});
  }
  push_instruction(&builder->instructions, location, (Instruction) {.maybe_label = else_label});

  move_value(&builder->instructions, location, result, value_from_s8(0));
  push_instruction(&builder->instructions, location, (Instruction) {.maybe_label = label});
  return result;
}

Value *
make_or(
  Function_Builder *builder,
  const Source_Location *location,
  Value *a,
  Value *b
) {
  Value *result = reserve_stack(builder, &descriptor_s8);
  Label *label = make_label();

  Label *else_label = make_if(
    &builder->instructions, location,
    compare(Compare_Type_Equal, builder, location, a, value_from_s8(0))
  );
  {
    Value *rhs = compare(Compare_Type_Not_Equal, builder, location, b, value_from_s8(0));
    move_value(&builder->instructions, location, result, rhs);
    push_instruction(&builder->instructions, location, (Instruction) {jmp, {label32(label), 0, 0}});
  }
  push_instruction(&builder->instructions, location, (Instruction) {.maybe_label = else_label});

  move_value(&builder->instructions, location, result, value_from_s8(1));
  push_instruction(&builder->instructions, location, (Instruction) {.maybe_label = label});
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








