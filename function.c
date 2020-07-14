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

inline void
push_instruction_internal(
  const char *filename,
  u32 line_number,
  Function_Builder *builder,
  Instruction instruction
) {
  instruction.filename = filename;
  instruction.line_number = line_number;
  array_push(builder->instructions, instruction);
}

#define push_instruction(...)\
  push_instruction_internal(__FILE__, __LINE__, __VA_ARGS__)

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
      .argument_list = temp_allocate_array(Value, 16),
      .argument_count = 0,
      .returns = 0,
    },
  };
  Function_Builder *builder = array_push(program->functions, (Function_Builder){
    .program = program,
    .stack_reserve = 0,
    .prolog_label = make_label(),
    .epilog_label = make_label(),
    .descriptor = descriptor,
    .result = result,
    .instructions = array_alloc(Array_Instruction, 32),
  });

  Value *fn_value = temp_allocate(Value);
  *fn_value = (const Value) {
    .descriptor = descriptor,
    .operand = label32(builder->prolog_label),
  };
  *result = fn_value;

  return builder;
}

Descriptor *
fn_update_result(
  Function_Builder *builder
) {
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
  builder->stack_reserve = align(builder->stack_reserve, 16) + alignment;

  fn_freeze(builder);
}

void
fn_encode(
  Buffer *buffer,
  Function_Builder *builder
) {
  encode_instruction(buffer, builder, (Instruction) {.maybe_label = builder->prolog_label});
  encode_instruction(buffer, builder, (Instruction) {sub, {rsp, imm_auto(builder->stack_reserve), 0}});

  for (
    Instruction *instruction = array_begin(builder->instructions);
    instruction != array_end(builder->instructions);
    ++instruction
  ) {
    encode_instruction(buffer, builder, *instruction);
  }

  encode_instruction(buffer, builder, (Instruction) {.maybe_label = builder->epilog_label});

  encode_instruction(buffer, builder, (Instruction) {add, {rsp, imm_auto(builder->stack_reserve), 0}});
  encode_instruction(buffer, builder, (Instruction) {ret, {0}});
  encode_instruction(buffer, builder, (Instruction) {int3, {0}});
  array_free(builder->instructions);
}

Jit_Program
program_end(
  Program *program
) {
  u64 code_buffer_size = estimate_max_code_size_in_bytes(program);
  Jit_Program result = {
    .code_buffer = make_buffer(code_buffer_size, PAGE_EXECUTE_READWRITE),
    .data_buffer = program->data_buffer,
  };
  program->code_base_rva = (s64)result.code_buffer.memory;
  program->data_base_rva = (s64)program->data_buffer.memory;

  if (program->import_libraries.array) {
    for (s64 i = 0; i < array_count(program->import_libraries); ++i) {
      Import_Library *lib = array_get(program->import_libraries, i);
      HINSTANCE dll_handle = LoadLibraryA(lib->name);
      assert(dll_handle);

      for (s32 i = 0; i < array_count(lib->symbols); ++i) {
        Import_Symbol *fn = array_get(lib->symbols, i);

        fn_type_opaque fn_address = (fn_type_opaque)GetProcAddress(dll_handle, fn->name);
        assert(fn_address);
        s64 offset = program->data_buffer.occupied;
        fn_type_opaque *rip_target =
          buffer_allocate_size(&program->data_buffer, sizeof(fn_type_opaque));
        *rip_target = fn_address;
        assert(fits_into_s32(offset));
        fn->offset_in_data = (s32)(offset);
      }
    }
  }

  for (
    Function_Builder *builder = array_begin(program->functions);
    builder != array_end(program->functions);
    ++builder
  ) {
    fn_encode(&result.code_buffer, builder);
  }
  return result;
}

Value *
fn_arg(
  Function_Builder *builder,
  Descriptor *descriptor
) {
  assert(!fn_is_frozen(builder));
  s32 argument_index = builder->next_argument_index;
  builder->next_argument_index++;
  Descriptor_Function *function = &builder->descriptor->function;
  Value *result = value_for_argument_index(function, descriptor, argument_index);
  fn_update_result(builder);
  return result;
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

#define Plus(_a_, _b_) plus(builder_, _a_, _b_)
#define Minus(_a_, _b_) minus(builder_, _a_, _b_)
#define Multiply(_a_, _b_) multiply(builder_, _a_, _b_)
#define Divide(_a_, _b_) divide(builder_, _a_, _b_)

#define SizeOfDescriptor(_descriptor_) value_from_s32(descriptor_byte_size(_descriptor_))
#define SizeOf(_value_) value_byte_size(_value_)

#define ReflectDescriptor (_descriptor_) fn_reflect(builder_, _descriptor_)

#define IfBuilder(_builder_, _value_) \
  for (\
    Label *label__ = make_if(_builder_, _value_), *dummy__ = 0; \
    !(dummy__++); \
     push_instruction(_builder_, (Instruction) {.maybe_label = label__})\
  )
#define If(_value_) IfBuilder(builder_, _value_)

#define Match\
  for (\
    Label *match_end_label__ = make_label(), *dummy__ = 0; \
    !(dummy__++); \
    push_instruction(builder_, (Instruction) {.maybe_label = match_end_label__})\
  )
#define Case(_value_)\
  for (\
    Label *label__ = make_if(builder_, _value_), *dummy__ = 0; \
    !(dummy__++); \
    push_instruction(builder_, (Instruction) {jmp, {label32(match_end_label__), 0, 0}}),\
    push_instruction(builder_, (Instruction) {.maybe_label = label__})\
  )
#define CaseAny

#define Loop \
  for ( \
    Loop_Builder loop_builder_ = loop_start(builder_); \
    !loop_builder_.done; \
    loop_end(builder_, &loop_builder_) \
  )

#define Continue \
  push_instruction(builder_, (Instruction) {jmp, {label32(loop_builder_.label_start), 0, 0}})
#define Break \
  push_instruction(builder_, (Instruction) {jmp, {label32(loop_builder_.label_end), 0, 0}})

typedef enum {
  Compare_Equal = 1,
  Compare_Not_Equal,
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

#define NotEq(_a_, _b_) compare(builder_, Compare_Not_Equal, (_a_), (_b_))
#define Eq(_a_, _b_) compare(builder_, Compare_Equal, (_a_), (_b_))
#define Less(_a_, _b_) compare(builder_, Compare_Less, (_a_), (_b_))
#define Greater(_a_, _b_) compare(builder_, Compare_Greater, (_a_), (_b_))

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
    if (return_size != 0) {
      Value *result = reserve_stack(builder, descriptor->returns->descriptor);
      move_value(builder, result, descriptor->returns);
      return result;
    }
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
  for (;to_call; to_call = to_call->descriptor->function.next_overload) {
    Descriptor_Function *descriptor = &to_call->descriptor->function;
    if (argument_count != descriptor->argument_count) continue;
    bool match = true;
    for (s64 arg_index = 0; arg_index < argument_count; ++arg_index) {
      Value *arg = argument_list[arg_index];
      if(!same_value_type(&descriptor->argument_list[arg_index], arg)) {
        match = false;
        break;
      }
    }
    if (match) {
      return call_function_overload(builder, to_call, argument_list, argument_count);
    }
  }
  assert(!"No matching overload found");
  return 0;
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

#define Function(_id_) \
  Value *_id_ = 0; \
  for (\
    Function_Builder *builder_ = fn_begin(&_id_, program_);\
    !fn_is_frozen(builder_);\
    fn_end(builder_)\
  )

#define Return(_value_) \
  fn_return(builder_, _value_)

#define Arg(_id_, _descriptor_) \
  Value *_id_ = fn_arg(builder_, (_descriptor_))

#define Arg_s8(_id_) Arg((_id_), &descriptor_s8)
#define Arg_s32(_id_) Arg((_id_), &descriptor_s32)
#define Arg_s64(_id_) Arg((_id_), &descriptor_s64)

#define Stack(_id_, _descriptor_, _value_) \
  Value *_id_ = reserve_stack(builder_, (_descriptor_)); \
  move_value(builder_, _id_, (_value_))

#define Stack_s32(_id_, _value_) Stack((_id_), &descriptor_s32, _value_)
#define Stack_s64(_id_, _value_) Stack((_id_), &descriptor_s64, _value_)


#define Call(_target_, ...)\
  call_function_value(\
    builder_,\
    (_target_),\
    (Value **)((Value *[]){0, ##__VA_ARGS__}) + 1, \
    static_array_size(((Value *[]){0, ##__VA_ARGS__})) - 1 \
  )

#define And(_a_, _b_) make_and(builder_, (_a_), (_b_))
#define Or(_a_, _b_) make_or(builder_, (_a_), (_b_))














