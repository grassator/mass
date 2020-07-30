#include "function.h"

Value *
reserve_stack(
  Function_Builder *fn,
  Descriptor *descriptor
) {
  u32 byte_size = descriptor_byte_size(descriptor);
  fn->stack_reserve += byte_size;
  Operand operand = stack(-fn->stack_reserve, byte_size);
  Value *result = temp_allocate(Value);
  *result = (const Value) {
    .descriptor = descriptor,
    .operand = operand,
  };
  return result;
}

inline void
push_instruction_internal(
  const char *filename,
  u32 line_number,
  Function_Builder *builder,
  Instruction instruction
) {
  instruction.filename = filename;
  instruction.line_number = line_number;
  dyn_array_push(builder->instructions, instruction);
}

bool
is_memory_operand(
  Operand *operand
) {
  return (
    operand->type == Operand_Type_Memory_Indirect ||
    operand->type == Operand_Type_RIP_Relative
  );
}

void
move_value(
  Function_Builder *builder,
  Value *a,
  Value *b
) {
  // TODO figure out more type checking
  u32 a_size = descriptor_byte_size(a->descriptor);
  u32 b_size = descriptor_byte_size(b->descriptor);

  if (is_memory_operand(&a->operand) && is_memory_operand(&b->operand)) {
    Value *reg_a = value_register_for_descriptor(Register_A, a->descriptor);
    move_value(builder, reg_a, b);
    move_value(builder, a, reg_a);
    return;
  }

  if (a_size != b_size) {
    if (
      a->operand.type == Operand_Type_Register &&
      b_size < a_size &&
      a_size <= 4
    ) {
      // TODO deal with unsigned numbers
      push_instruction(builder, (Instruction) {movsx, {a->operand, b->operand, 0}});
      return;
    } else if (!(
      b->operand.type == Operand_Type_Immediate_32 &&
      a_size == 8
    )) {
      assert(!"Mismatched operand size when moving");
    }
  }

  if (
    a->operand.type == Operand_Type_Register &&
    (
      (b->operand.type == Operand_Type_Immediate_64 && b->operand.imm64 == 0) ||
      (b->operand.type == Operand_Type_Immediate_32 && b->operand.imm32 == 0) ||
      (b->operand.type == Operand_Type_Immediate_8  && b->operand.imm8 == 0)
    )
  ) {
    // This messes up flags register so comparisons need to be aware of this optimization
    push_instruction(builder, (Instruction) {xor, {a->operand, a->operand, 0}});
    return;
  }

  if (b->operand.type == Operand_Type_Immediate_64 && ((s32)b->operand.imm64) >= 0) {
    move_value(builder, a, value_from_s32(b->operand.imm32));
  }

  if ((
    a->operand.type != Operand_Type_Register &&
    b->operand.type == Operand_Type_Immediate_64
  ) || (
    a->operand.type == Operand_Type_Memory_Indirect &&
    b->operand.type == Operand_Type_Memory_Indirect
  )) {
    Value *reg_a = value_register_for_descriptor(Register_A, a->descriptor);
    move_value(builder, reg_a, b);
    move_value(builder, a, reg_a);
  } else {
    push_instruction(builder, (Instruction) {mov, {a->operand, b->operand, 0}});
  }
}

Function_Builder *
fn_begin(Value **result, Program *program) {
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
fn_encode(
  Fixed_Buffer *buffer,
  Function_Builder *builder
) {
  encode_instruction(buffer, builder, (Instruction) {.maybe_label = builder->prolog_label});
  encode_instruction(buffer, builder, (Instruction) {sub, {rsp, imm_auto(builder->stack_reserve), 0}});

  for (u64 i = 0; i < dyn_array_length(builder->instructions); ++i) {
    Instruction *instruction = dyn_array_get(builder->instructions, i);
    encode_instruction(buffer, builder, *instruction);
  }

  encode_instruction(buffer, builder, (Instruction) {.maybe_label = builder->epilog_label});
  encode_instruction(buffer, builder, (Instruction) {add, {rsp, imm_auto(builder->stack_reserve), 0}});
  encode_instruction(buffer, builder, (Instruction) {ret, {0}});
  encode_instruction(buffer, builder, (Instruction) {int3, {0}});
  dyn_array_destroy(builder->instructions);
}

Jit_Program
program_end(
  Program *program
) {
  u64 code_buffer_size = estimate_max_code_size_in_bytes(program);
  Jit_Program result = {
    .code_buffer = fixed_buffer_make(.allocator = allocator_system, .capacity = code_buffer_size),
    .data_buffer = program->data_buffer,
  };
  program->code_base_rva = (s64)result.code_buffer->memory;
  program->data_base_rva = (s64)program->data_buffer->memory;

  if (dyn_array_is_initialized(program->import_libraries)) {
    for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
      Import_Library *lib = dyn_array_get(program->import_libraries, i);
      const char *library_name = slice_to_c_string(temp_allocator, lib->name);
      HINSTANCE dll_handle = LoadLibraryA(library_name);
      assert(dll_handle);

      for (u64 i = 0; i < dyn_array_length(lib->symbols); ++i) {
        Import_Symbol *symbol = dyn_array_get(lib->symbols, i);

        const char *symbol_name = slice_to_c_string(temp_allocator, symbol->name);
        fn_type_opaque fn_address = (fn_type_opaque)GetProcAddress(dll_handle, symbol_name);
        assert(fn_address);
        s64 offset = program->data_buffer->occupied;
        fn_type_opaque *rip_target = fixed_buffer_allocate(program->data_buffer, fn_type_opaque);
        *rip_target = fn_address;
        symbol->offset_in_data = s64_to_s32(offset);
      }
    }
  }
  for (u64 i = 0; i < dyn_array_length(program->functions); ++i) {
    Function_Builder *builder = dyn_array_get(program->functions, i);
    fn_encode(result.code_buffer, builder);
  }

  // Making code executable
  DWORD dummy = 0;
  VirtualProtect(result.code_buffer, code_buffer_size, PAGE_EXECUTE_READ, &dummy);

  return result;
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
fn_return_descriptor(
  Function_Builder *builder,
  Descriptor *descriptor
) {
  Descriptor_Function *function = &builder->descriptor->function;
  if (function->returns) {
    assert(same_type(function->returns->descriptor, descriptor));
  } else {
    assert(!fn_is_frozen(builder));
    if (descriptor->type != Descriptor_Type_Void) {
      function->returns = value_register_for_descriptor(Register_A, descriptor);
    } else {
      function->returns = &void_value;
    }
  }
}

void
fn_return(
  Function_Builder *builder,
  Value *to_return
) {
  fn_return_descriptor(builder, to_return->descriptor);
  if (to_return->descriptor->type != Descriptor_Type_Void) {
    move_value(builder, builder->descriptor->function.returns, to_return);
  }

  push_instruction(builder, (Instruction) {jmp, {label32(builder->epilog_label), 0, 0}});
}

Label *make_if(
  Function_Builder *builder,
  Value *value
) {
  Label *label = make_label();
  u32 byte_size = descriptor_byte_size(value->descriptor);
  if (byte_size == 4 || byte_size == 8) {
    push_instruction(builder, (Instruction) {cmp, {value->operand, imm32(0), 0}});
  } else if (byte_size == 1) {
    push_instruction(builder, (Instruction) {cmp, {value->operand, imm8(0), 0}});
  } else {
    assert(!"Unsupported value inside `if`");
  }
  push_instruction(builder, (Instruction) {jz, {label32(label), 0, 0}});
  return label;
}

Loop_Builder
loop_start(
  Function_Builder *builder
) {
  Label *label_start = make_label();
  push_instruction(builder, (Instruction) { .maybe_label = label_start });
  return (Loop_Builder) {
    .done = false,
    .label_start = label_start,
    .label_end = make_label(),
  };
}

void
loop_end(
  Function_Builder *builder,
  Loop_Builder *loop
) {
  push_instruction(builder, (Instruction) {jmp, {label32(loop->label_start), 0, 0}});
  push_instruction(builder, (Instruction) { .maybe_label = loop->label_end });
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

Value *
plus_or_minus(
  Arithmetic_Operation operation,
  Function_Builder *builder,
  Value *a,
  Value *b
) {
  if (!(
    a->descriptor->type == Descriptor_Type_Pointer &&
    b->descriptor->type == Descriptor_Type_Integer &&
    b->descriptor->integer.byte_size == 8
  )) {
    assert(same_value_type(a, b));
    assert(a->descriptor->type == Descriptor_Type_Integer);
  }

  assert_not_register_ax(a);
  assert_not_register_ax(b);

  Value *temp_b = reserve_stack(builder, b->descriptor);
  move_value(builder, temp_b, b);

  Value *reg_a = value_register_for_descriptor(Register_A, a->descriptor);
  move_value(builder, reg_a, a);

  switch(operation) {
    case Arithmetic_Operation_Plus: {
      push_instruction(builder, (Instruction) {add, {reg_a->operand, temp_b->operand, 0}});
      break;
    }
    case Arithmetic_Operation_Minus: {
      push_instruction(builder, (Instruction) {sub, {reg_a->operand, temp_b->operand, 0}});
      break;
    }
    default: {
      assert(!"Unknown arithmetic operation");
    }
  }

  Value *temp = reserve_stack(builder, a->descriptor);
  move_value(builder, temp, reg_a);

  return temp;
}

Value *
plus(
  Function_Builder *builder,
  Value *a,
  Value *b
) {
  return plus_or_minus(Arithmetic_Operation_Plus, builder, a, b);
}

Value *
minus(
  Function_Builder *builder,
  Value *a,
  Value *b
) {
  return plus_or_minus(Arithmetic_Operation_Minus, builder, a, b);
}

Value *
multiply(
  Function_Builder *builder,
  Value *x,
  Value *y
) {
  assert(same_value_type(x, y));
  assert(x->descriptor->type == Descriptor_Type_Integer);

  assert_not_register_ax(x);
  assert_not_register_ax(y);

  // TODO deal with signed / unsigned
  // TODO support double the size of the result?
  // TODO make the move only for imm value
  Value *y_temp = reserve_stack(builder, y->descriptor);

  Value *reg_a = value_register_for_descriptor(Register_A, y->descriptor);
  move_value(builder, reg_a, y);
  move_value(builder, y_temp, reg_a);

  reg_a = value_register_for_descriptor(Register_A, x->descriptor);
  move_value(builder, reg_a, x);

  push_instruction(builder, (Instruction) {imul, {reg_a->operand, y_temp->operand}});

  Value *temp = reserve_stack(builder, x->descriptor);
  move_value(builder, temp, reg_a);

  return temp;
}

Value *
divide(
  Function_Builder *builder,
  Value *a,
  Value *b
) {
  assert(same_value_type(a, b));
  assert(a->descriptor->type == Descriptor_Type_Integer);

  // TODO type check values
  assert_not_register_ax(a);
  assert_not_register_ax(b);

  // Save RDX as it will be used for the remainder
  Value *rdx_temp = reserve_stack(builder, &descriptor_s64);

  Value *reg_rdx = value_register_for_descriptor(Register_A, &descriptor_s64);
  move_value(builder, rdx_temp, reg_rdx);

  Value *reg_a = value_register_for_descriptor(Register_A, a->descriptor);
  move_value(builder, reg_a, a);

  // TODO deal with signed / unsigned
  Value *divisor = reserve_stack(builder, b->descriptor);
  move_value(builder, divisor, b);

  switch (descriptor_byte_size(a->descriptor)) {
    case 8: {
      push_instruction(builder, (Instruction) {cqo, {0}});
      break;
    }
    case 4: {
      push_instruction(builder, (Instruction) {cdq, {0}});
      break;
    }
    case 2: {
      push_instruction(builder, (Instruction) {cwd, {0}});
      break;
    }
    default: {
      assert(!"Unsupported byte size when dividing");
    }
  }
  push_instruction(builder, (Instruction) {idiv, {divisor->operand, 0, 0}});

  Value *temp = reserve_stack(builder, a->descriptor);
  move_value(builder, temp, reg_a);

  // Restore RDX
  move_value(builder, reg_rdx, rdx_temp);

  return temp;
}

Value *
compare(
  Function_Builder *builder,
  Compare operation,
  Value *a,
  Value *b
) {
  assert(same_value_type(a, b));
  assert(a->descriptor->type == Descriptor_Type_Integer);
  Value *temp_b = reserve_stack(builder, b->descriptor);
  move_value(builder, temp_b, b);

  Value *reg_a = value_register_for_descriptor(Register_A, a->descriptor);
  move_value(builder, reg_a, a);

  push_instruction(builder, (Instruction) {cmp, {reg_a->operand, temp_b->operand, 0}});
  Value *result = reserve_stack(builder, &descriptor_s8);

  switch(operation) {
    case Compare_Equal: {
      push_instruction(builder, (Instruction) {setz, {result->operand, 0, 0}});
      break;
    }
    case Compare_Not_Equal: {
      push_instruction(builder, (Instruction) {setne, {result->operand, 0, 0}});
      break;
    }
    case Compare_Less: {
      push_instruction(builder, (Instruction) {setl, {result->operand, 0, 0}});
      break;
    }
    case Compare_Greater: {
      push_instruction(builder, (Instruction) {setg, {result->operand, 0, 0}});
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
  push_instruction(builder, (Instruction) {lea, {reg_a->operand, value->operand, 0}});

  Value *result = reserve_stack(builder, result_descriptor);
  move_value(builder, result, reg_a);

  return result;
}


Value *
call_function_overload(
  Function_Builder *builder,
  Value *to_call,
  Array_Value_Ptr arguments
) {
  assert(to_call->descriptor->type == Descriptor_Type_Function);
  Descriptor_Function *descriptor = &to_call->descriptor->function;
  assert(dyn_array_length(descriptor->arguments) == dyn_array_length(arguments));

  fn_ensure_frozen(descriptor);

  for (u64 i = 0; i < dyn_array_length(arguments); ++i) {
    Value *actual_arg = *dyn_array_get(arguments, i);
    Value *expected_arg = *dyn_array_get(descriptor->arguments, i);
    assert(same_value_type(expected_arg, actual_arg));
    move_value(builder, expected_arg, actual_arg);
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
    push_instruction(builder, (Instruction) {lea, {reg_c->operand, descriptor->returns->operand, 0}});
  }

  builder->max_call_parameters_stack_size = u64_to_u32(u64_max(
    builder->max_call_parameters_stack_size,
    parameters_stack_size
  ));

  if (to_call->operand.type == Operand_Type_Label_32) {
    push_instruction(builder, (Instruction) {call, {to_call->operand, 0, 0}});
  } else {
    Value *reg_a = value_register_for_descriptor(Register_A, to_call->descriptor);
    move_value(builder, reg_a, to_call);
    push_instruction(builder, (Instruction) {call, {reg_a->operand, 0, 0}});
  }

  if (return_size <= 8) {
    if (return_size != 0) {
      Value *result = reserve_stack(builder, descriptor->returns->descriptor);
      move_value(builder, result, descriptor->returns);
      return result;
    }
  }

  return descriptor->returns;
}

Value *
call_function_value_array(
  Function_Builder *builder,
  Value *to_call,
  Array_Value_Ptr arguments
) {
  for (;to_call; to_call = to_call->descriptor->function.next_overload) {
    Descriptor_Function *descriptor = &to_call->descriptor->function;
    if (dyn_array_length(arguments) != dyn_array_length(descriptor->arguments)) continue;
    bool match = true;
    for (u64 arg_index = 0; arg_index < dyn_array_length(arguments); ++arg_index) {
      Value *actual_arg = *dyn_array_get(arguments, arg_index);
      Value *expected_arg = *dyn_array_get(descriptor->arguments, arg_index);
      if(!same_value_type(actual_arg, expected_arg)) {
        match = false;
        break;
      }
    }
    if (match) {
      return call_function_overload(builder, to_call, arguments);
    }
  }
  dyn_array_destroy(arguments);
  assert(!"No matching overload found");
  return 0;
}

Value *
call_function_value(
  Function_Builder *builder,
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
  return call_function_value_array(builder, to_call, arguments);
}

Value *
make_and(
  Function_Builder *builder,
  Value *a,
  Value *b
) {
  Value *result = reserve_stack(builder, &descriptor_s8);
  Label *label = make_label();
  IfBuilder(builder, a) {
    Value *rhs = compare(builder, Compare_Not_Equal, b, value_from_s8(0));
    move_value(builder, result, rhs);
    push_instruction(builder, (Instruction) {jmp, {label32(label), 0, 0}});
  }
  move_value(builder, result, value_from_s8(0));
  push_instruction(builder, (Instruction) {.maybe_label = label});
  return result;
}

Value *
make_or(
  Function_Builder *builder,
  Value *a,
  Value *b
) {
  Value *result = reserve_stack(builder, &descriptor_s8);
  Label *label = make_label();
  IfBuilder(builder, compare(builder, Compare_Equal, b, value_from_s8(0))) {
    Value *rhs = compare(builder, Compare_Not_Equal, b, value_from_s8(0));
    move_value(builder, result, rhs);
    push_instruction(builder, (Instruction) {jmp, {label32(label), 0, 0}});
  }
  move_value(builder, result, value_from_s8(1));
  push_instruction(builder, (Instruction) {.maybe_label = label});
  return result;
}










