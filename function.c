#include "prelude.h"
#include "value.h"

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

void
push_instruction(
  Function_Builder *builder,
  Instruction instruction
) {
  assert(builder->instruction_count < MAX_INSTRUCTION_COUNT);
  builder->instructions[builder->instruction_count] = instruction;
  builder->instruction_count++;
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
//
//
  //if (a_size != b_size) {
    //if (b->operand.type == Operand_Type_Memory_Indirect || )
    //if (!(
      //b->operand.type == Operand_Type_Immediate_32 &&
      //a_size == 8
    //)) {
      //assert(!"Mismatched operand size when moving");
    //}
  //}
  if ((
    b_size == 1 && (a_size >= 2 && a_size <= 8)
  )) {
    assert(a->operand.type == Operand_Type_Register);
    Value *zero = value_from_s64(0);
    zero->descriptor = a->descriptor;

    move_value(builder, a, zero);
    push_instruction(builder, (Instruction) {mov, {a->operand, b->operand, 0}});
    // FIXME use movsx
    //push_instruction(builder, (Instruction) {movsx, {a->operand, b->operand, 0}});
    return;
  }

  if ((
    b->operand.type == Operand_Type_Immediate_64 &&
    a->operand.type != Operand_Type_Register
  ) || (
    a->operand.type == Operand_Type_Memory_Indirect &&
    b->operand.type == Operand_Type_Memory_Indirect
  )) {
    Value *reg_a = value_register_for_descriptor(Register_A, a->descriptor);
    // TODO Can be a problem if RAX is already used as temp
    push_instruction(builder, (Instruction) {mov, {reg_a->operand, b->operand, 0}});
    push_instruction(builder, (Instruction) {mov, {a->operand, reg_a->operand}});
  } else {
    push_instruction(builder, (Instruction) {mov, {a->operand, b->operand, 0}});
  }
}

Function_Builder
fn_begin(Value **result, Buffer *buffer) {
  Descriptor *descriptor = temp_allocate(Descriptor);
  *descriptor = (const Descriptor) {
    .type = Descriptor_Type_Function,
    .function = {
      .argument_list = temp_allocate_array(Value, 16),
      .argument_count = 0,
      .returns = 0,
    },
  };
  Function_Builder builder = {
    .stack_reserve = 0,
    .buffer = buffer,
    .epilog_label = make_label(),
    .descriptor = descriptor,
    .result = result,
    .code = buffer->memory + buffer->occupied,
    .stack_displacements = malloc(sizeof(Stack_Patch) * MAX_DISPLACEMENT_COUNT),
    .instructions = malloc(sizeof(Instruction) * MAX_INSTRUCTION_COUNT),
  };
  Value *fn_value = temp_allocate(Value);
  Label *label = make_label();
  label->target = builder.code;
  *fn_value = (const Value) {
    .descriptor = descriptor,
    .operand = label32(label),
  };
  *result = fn_value;

  return builder;
}

Descriptor *
fn_update_result(
  Function_Builder *builder
) {
  builder->descriptor->function.argument_count = builder->next_argument_index;
  return (*builder->result)->descriptor;
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
  s32 stack_size = align(builder->stack_reserve, 16) + alignment;
  encode_instruction(builder, (Instruction) {sub, {rsp, imm_auto(stack_size), 0}});

  for (u32 i = 0; i < builder->instruction_count; ++i) {
    encode_instruction(builder, builder->instructions[i]);
  }

  for (u64 i = 0; i < builder->stack_displacement_count; ++i) {
    Stack_Patch *patch = &builder->stack_displacements[i];
    s32 displacement = *patch->location;
    // @Volatile @StackPatch
    // Negative diplacement is used to encode local variables
    if (displacement < 0) {
      *patch->location = stack_size + displacement;
    } else
    // Positive values larger than max_call_parameters_stack_size
    if (displacement >= (s32)builder->max_call_parameters_stack_size) {
      // Return address will be pushed on the stack by the caller and we need to account for that
      s32 return_address_size = 8;
      *patch->location = stack_size + displacement + return_address_size;
    }
  }

  encode_instruction(builder, (Instruction) {.maybe_label = builder->epilog_label});

  encode_instruction(builder, (Instruction) {add, {rsp, imm_auto(stack_size), 0}});
  encode_instruction(builder, (Instruction) {ret, {0}});

  fn_freeze(builder);

  free(builder->stack_displacements);
  free(builder->instructions);
}

Value *
fn_arg(
  Function_Builder *builder,
  Descriptor *descriptor
) {
  assert(!fn_is_frozen(builder));
  u32 byte_size = descriptor_byte_size(descriptor);
  assert(byte_size <= 8);
  Descriptor_Function *function = &builder->descriptor->function;
  s32 argument_index = builder->next_argument_index;
  builder->next_argument_index++;
  switch (argument_index) {
    case 0: {
      function->argument_list[0] = *value_register_for_descriptor(Register_C, descriptor);
      break;
    }
    case 1: {
      function->argument_list[1] = *value_register_for_descriptor(Register_D, descriptor);
      break;
    }
    case 2: {
      function->argument_list[2] = *value_register_for_descriptor(Register_R8, descriptor);
      break;
    }
    case 3: {
      function->argument_list[3] = *value_register_for_descriptor(Register_R9, descriptor);
      break;
    }
    default: {
      // @Volatile @StackPatch
      s32 offset = argument_index * 8;
      Operand operand = stack(offset, byte_size);

      function->argument_list[argument_index] = (const Value) {
        .descriptor = descriptor,
        .operand = operand,
      };
      break;
    }
  }
  fn_update_result(builder);
  return &function->argument_list[argument_index];
}

void
fn_return(
  Function_Builder *builder,
  Value *to_return
) {
  Descriptor_Function *function = &builder->descriptor->function;
  if (function->returns) {
    assert(same_type(function->returns->descriptor, to_return->descriptor));
  } else {
    assert(!fn_is_frozen(builder));
    if (to_return->descriptor->type != Descriptor_Type_Void) {
      function->returns = value_register_for_descriptor(Register_A, to_return->descriptor);
    } else {
      function->returns = &void_value;
    }
  }

  if (to_return->descriptor->type != Descriptor_Type_Void) {
    move_value(builder, function->returns, to_return);
  }

  push_instruction(builder, (Instruction) {jmp, {label32(builder->epilog_label), 0, 0}});
  fn_update_result(builder);
}

Label *make_if(
  Function_Builder *builder,
  Value *value
) {
  Label *label = make_label();
  push_instruction(builder, (Instruction) {cmp, {value->operand, imm32(0), 0}});
  push_instruction(builder, (Instruction) {jz, {label32(label), 0, 0}});
  return label;
}

typedef struct {
  bool done;
  Label *label_start;
  Label *label_end;
} Loop_Builder;

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

  // TODO check operand sizes
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

#define Plus(_a_, _b_) plus(&builder_, _a_, _b_)
#define Minus(_a_, _b_) minus(&builder_, _a_, _b_)
#define Multiply(_a_, _b_) multiply(&builder_, _a_, _b_)
#define Divide(_a_, _b_) divide(&builder_, _a_, _b_)

#define SizeOfDescriptor(_descriptor_) value_from_s32(descriptor_byte_size(_descriptor_))
#define SizeOf(_value_) value_byte_size(_value_)

#define ReflectDescriptor (_descriptor_) fn_reflect(&builder_, _descriptor_)

#define IfBuilder(_builder_, _value_) \
  for (\
    Label *label__ = make_if(_builder_, _value_), *dummy__ = 0; \
    !(dummy__++); \
     push_instruction(_builder_, (Instruction) {.maybe_label = label__})\
  )
#define If(_value_) IfBuilder(&builder_, _value_)

#define Loop \
  for ( \
    Loop_Builder loop_builder_ = loop_start(&builder_); \
    !loop_builder_.done; \
    loop_end(&builder_, &loop_builder_) \
  )

#define Continue \
  push_instruction(&builder_, (Instruction) {jmp, {label32(loop_builder_.label_start), 0, 0}})
#define Break \
  push_instruction(&builder_, (Instruction) {jmp, {label32(loop_builder_.label_end), 0, 0}})

typedef enum {
  Compare_Equal,
  Compare_Less,
  Compare_Greater,
} Compare;

Value *
compare(
  Function_Builder *builder,
  Compare operation,
  Value *a,
  Value *b
) {
  Value *temp_b = reserve_stack(builder, b->descriptor);
  move_value(builder, temp_b, b);

  Value *reg_a = value_register_for_descriptor(Register_A, a->descriptor);
  move_value(builder, reg_a, a);

  // TODO check that types are comparable
  push_instruction(builder, (Instruction) {cmp, {reg_a->operand, temp_b->operand, 0}});

  // TODO use xor
  reg_a = value_register_for_descriptor(Register_A, &descriptor_s64);
  move_value(
    builder,
    reg_a,
    value_from_s64(0)
  );


  // TODO We can use a separate value here because of manual clearing of
  //      of the register above, but it is not great.
  Value *reg_a8 = value_register_for_descriptor(Register_A, &descriptor_s8);

  // TODO use correct operand size of a byte for these instructions
  switch(operation) {
    case Compare_Equal: {
      push_instruction(builder, (Instruction) {setz, {reg_a8->operand, 0, 0}});
      break;
    }
    case Compare_Less: {
      push_instruction(builder, (Instruction) {setl, {reg_a8->operand, 0, 0}});
      break;
    }
    case Compare_Greater: {
      push_instruction(builder, (Instruction) {setg, {reg_a8->operand, 0, 0}});
      break;
    }
    default: {
      assert(!"Unsupported comparison");
    }
  }

  Value *result = reserve_stack(builder, &descriptor_s64);
  move_value(builder, result, reg_a);
  return result;
}

#define Eq(_a_, _b_) compare(&builder_, Compare_Equal, (_a_), (_b_))
#define Less(_a_, _b_) compare(&builder_, Compare_Less, (_a_), (_b_))
#define Greater(_a_, _b_) compare(&builder_, Compare_Greater, (_a_), (_b_))

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
  Value **argument_list,
  s64 argument_count
) {
  assert(to_call->descriptor->type == Descriptor_Type_Function);
  Descriptor_Function *descriptor = &to_call->descriptor->function;
  assert(descriptor->argument_count == argument_count);

  fn_ensure_frozen(descriptor);

  for (s64 i = 0; i < argument_count; ++i) {
    assert(same_value_type(&descriptor->argument_list[i], argument_list[i]));
    move_value(builder, &descriptor->argument_list[i], argument_list[i]);
  }

  // If we call a function, then we need to reserve space for the home
  // area of at least 4 arguments?
  u32 parameters_stack_size = (u32)max(4, argument_count) * 8;

  // FIXME support this for fns that accept arguments
  u32 return_size = descriptor_byte_size(descriptor->returns->descriptor);
  if (return_size > 8) {
    parameters_stack_size += return_size;
    Descriptor *return_pointer_descriptor = descriptor_pointer_to(descriptor->returns->descriptor);
    Value *reg_c =
      value_register_for_descriptor(Register_C, return_pointer_descriptor);
    push_instruction(builder, (Instruction) {lea, {reg_c->operand, descriptor->returns->operand, 0}});
  }

  builder->max_call_parameters_stack_size = max(
    builder->max_call_parameters_stack_size,
    parameters_stack_size
  );

  if (to_call->operand.type == Operand_Type_Label_32) {
    push_instruction(builder, (Instruction) {call, {to_call->operand, 0, 0}});
  } else {
    Value *reg_a = value_register_for_descriptor(Register_A, to_call->descriptor);
    move_value(builder, reg_a, to_call);
    push_instruction(builder, (Instruction) {call, {reg_a->operand, 0, 0}});
  }

  if (return_size <= 8) {
    Value *result = reserve_stack(builder, descriptor->returns->descriptor);
    move_value(builder, result, descriptor->returns);
    return result;
  }

  return descriptor->returns;
}

Value *
call_function_value(
  Function_Builder *builder,
  Value *to_call,
  Value **argument_list,
  s64 argument_count
) {
  assert(to_call);
  while (to_call) {
    Descriptor_Function *descriptor = &to_call->descriptor->function;
    bool match = true;
    for (s64 arg_index = 0; arg_index < argument_count; ++arg_index) {
      // FIXME @Overloads
      Value *arg = argument_list[arg_index];
      if(!same_value_type(&descriptor->argument_list[arg_index], arg)) {
        match = false;
        break;
      }
    }
    if (match) {
      return call_function_overload(builder, to_call, argument_list, argument_count);
    }
    to_call = descriptor->next_overload;
  }
  assert(!"No matching overload found");
  return 0;
}

#define Function(_id_) \
  Value *_id_ = 0; \
  for (Function_Builder builder_ = fn_begin(&_id_, &function_buffer); !fn_is_frozen(&builder_); fn_end(&builder_))

#define Return(_value_) \
  fn_return(&builder_, _value_)

#define Arg(_id_, _descriptor_) \
  Value *_id_ = fn_arg(&builder_, (_descriptor_))

#define Arg_s32(_id_) Arg((_id_), &descriptor_s32)
#define Arg_s64(_id_) Arg((_id_), &descriptor_s64)

#define Stack(_id_, _descriptor_, _value_) \
  Value *_id_ = reserve_stack(&builder_, (_descriptor_)); \
  move_value(&builder_, _id_, (_value_))

#define Stack_s32(_id_, _value_) Stack((_id_), &descriptor_s32, _value_)
#define Stack_s64(_id_, _value_) Stack((_id_), &descriptor_s64, _value_)


#define Call(_target_, ...)\
  call_function_value(\
    &builder_,\
    (_target_),\
    (Value **)((Value *[]){0, ##__VA_ARGS__}) + 1, \
    static_array_size(((Value *[]){0, ##__VA_ARGS__})) - 1 \
  )


